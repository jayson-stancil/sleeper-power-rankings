# =============================================================================
# Sleeper Power Rankings -- Shiny app
# Presentation layer: reads weekly rankings CSVs committed to GitHub by the
# scheduled Action, pulls team names/avatars live from the Sleeper API, and
# renders the rankings table plus rating trajectories.
# =============================================================================

library(shiny)
library(gt)
library(ggplot2)

# ---- Repo constants ---------------------------------------------------------
GH_USER   <- "jayson-stancil"
GH_REPO   <- "sleeper-power-rankings"
GH_BRANCH <- "main"

source("R/engine.R")   # sleeper(), build_team_table(), PA helpers

# ---- League configs ---------------------------------------------------------
league_files <- list.files("leagues", pattern = "\\.R$", full.names = TRUE)
leagues <- lapply(league_files, function(f) source(f, local = new.env())$value)
names(leagues) <- vapply(leagues, `[[`, character(1), "league_tag")

# ---- GitHub data access (raw URL first for freshness, local bundle fallback)
raw_url <- function(path) {
  paste0("https://raw.githubusercontent.com/", GH_USER, "/", GH_REPO, "/",
         GH_BRANCH, "/", utils::URLencode(path))
}

read_repo_csv <- function(path) {
  out <- suppressWarnings(
    tryCatch(read.csv(raw_url(path), stringsAsFactors = FALSE,
                      check.names = FALSE),
             error = function(e) NULL))
  if (is.null(out) && file.exists(path)) {
    out <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
  out
}

# List weekly rankings files: GitHub contents API (fresh), local fallback
list_weekly_files <- function(cfg) {
  dir_path <- file.path(cfg$data_dir, "Power Rankings", "Weeks 1-14")
  api <- suppressWarnings(tryCatch(
    jsonlite::fromJSON(paste0("https://api.github.com/repos/", GH_USER, "/",
                              GH_REPO, "/contents/",
                              utils::URLencode(dir_path))),
    error = function(e) NULL))
  files <- if (!is.null(api) && !is.null(api$name)) api$name
           else if (dir.exists(dir_path)) list.files(dir_path)
           else character(0)
  files[grepl("Power Rankings\\.csv$", files)]
}

week_from_filename <- function(x) {
  as.integer(sub(".* Week (\\d+) Power Rankings\\.csv$", "\\1", x))
}

# ---- UI ---------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Sleeper Power Rankings"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # League picker only when a repo carries more than one league config
      if (length(leagues) > 1) {
        selectInput("league", "League", choices = names(leagues))
      },
      selectInput("week", "Rankings for week", choices = NULL),
      helpText("Ratings: Glicko-2, recomputed weekly from all completed",
               "games. Data updates automatically every Tuesday.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Rankings", gt_output("rank_table")),
        tabPanel("Rating trajectory",
                 plotOutput("trajectory", height = "550px")),
        tabPanel("Points against", tableOutput("pa_table")),
        tabPanel("League history",
                 h4("Champions"),
                 tableOutput("champions"),
                 h4("All-time regular-season records"),
                 tableOutput("alltime"),
                 h4("Head-to-head (row owner's record vs column owner)"),
                 div(style = "overflow-x: auto;",
                     tableOutput("h2h")))
      )
    )
  )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  cfg <- reactive({
    if (length(leagues) == 1) leagues[[1]]
    else leagues[[req(input$league)]]
  })

  # Live Sleeper identity (team names, avatars) -- cached per session
  identity <- reactive({
    c_ <- cfg()
    users   <- sleeper(paste0("league/", c_$league_id, "/users"))
    rosters <- sleeper(paste0("league/", c_$league_id, "/rosters"))
    build_team_table(users, rosters, c_$owner_map)
  })

  league_meta <- reactive(sleeper(paste0("league/", cfg()$league_id)))

  weekly_files <- reactive(list_weekly_files(cfg()))

  observeEvent(weekly_files(), {
    wks <- sort(week_from_filename(weekly_files()), decreasing = TRUE)
    if (length(wks)) {
      updateSelectInput(session, "week", choices = wks, selected = max(wks))
    } else {
      updateSelectInput(session, "week",
                        choices = c("No rankings yet - season not started" = ""))
    }
  }, ignoreNULL = FALSE)

  games <- reactive({
    c_ <- cfg()
    read_repo_csv(file.path(c_$data_dir,
                            paste0("matchups ", league_meta()$season, " ",
                                   c_$league_tag, ".csv")))
  })

  weekly_csv <- function(wk) {
    c_ <- cfg()
    read_repo_csv(file.path(c_$data_dir, "Power Rankings", "Weeks 1-14",
                            paste0(c_$season_label, " ", c_$league_tag,
                                   " Week ", wk, " Power Rankings.csv")))
  }

  # Assembled table for the selected week
  tbl <- reactive({
    wk  <- as.integer(req(input$week))
    cur <- weekly_csv(wk)
    validate(need(!is.null(cur), "No rankings file for this week yet."))
    cur <- cur[order(-cur$Rating), ]
    cur$rank <- seq_len(nrow(cur))

    prev <- if (wk > 2) weekly_csv(wk - 1) else NULL
    if (!is.null(prev)) {
      prev <- prev[order(-prev$Rating), ]
      prev$prev_rank <- seq_len(nrow(prev))
      cur$prev_rank   <- prev$prev_rank[match(cur$Player, prev$Player)]
      cur$rating_change <- cur$Rating -
        prev$Rating[match(cur$Player, prev$Player)]
      cur$rank_change <- cur$prev_rank - cur$rank
    } else {
      cur$rank_change <- NA_integer_
      cur$rating_change <- NA_real_
    }

    ids <- identity()
    cur <- merge(cur, setNames(ids[, c("owner", "team_name", "avatar_url")],
                               c("Player", "team_name", "avatar_url")),
                 by = "Player", all.x = TRUE)

    g <- games()
    if (!is.null(g)) {
      g_wk <- g[g$week <= wk - 1, , drop = FALSE]
      id_to_owner <- setNames(ids$owner, ids$roster_id)
      pa <- create_points_against_table(g_wk)
      pa$Team <- id_to_owner[as.character(pa$Team)]
      cur$avg_pa <- pa$avg_pa[match(cur$Player, pa$Team)]
      long <- do.call(rbind, lapply(unique(g_wk$week), function(w)
        transform_games_df(g_wk, w)))
      pf <- aggregate(Points ~ ID, long, mean)
      cur$avg_pf <- pf$Points[match(cur$Player,
                                    id_to_owner[as.character(pf$ID)])]
    } else {
      cur$avg_pa <- NA_real_; cur$avg_pf <- NA_real_
    }

    cur <- cur[order(cur$rank), ]
    cur$record <- paste0(cur$Win, "-", cur$Loss,
                         ifelse(cur$Draw > 0, paste0("-", cur$Draw), ""))
    cur$move <- ifelse(is.na(cur$rank_change) | cur$rank_change == 0, "—",
                       ifelse(cur$rank_change > 0,
                              paste0("▲ ", cur$rank_change),
                              paste0("▼ ", abs(cur$rank_change))))
    cur$avatar_url[is.na(cur$avatar_url)] <-
      "https://sleepercdn.com/images/v2/icons/league/league_avatar_mint.png"
    cur$owner <- cur$Player
    cur
  })

  output$rank_table <- render_gt({
    wk <- as.integer(req(input$week))
    build_graphic(tbl(), league_meta()$name, wk, wk - 1)
  })

  output$trajectory <- renderPlot({
    wks <- sort(week_from_filename(weekly_files()))
    validate(need(length(wks) > 1, "Trajectories appear after two weeks."))
    hist_df <- do.call(rbind, lapply(wks, function(w) {
      d <- weekly_csv(w)
      if (is.null(d)) return(NULL)
      data.frame(week = w, Player = d$Player, Rating = d$Rating)
    }))
    ggplot(hist_df, aes(week, Rating, color = Player)) +
      geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
      geom_hline(yintercept = 1500, linetype = "dashed", color = "grey55") +
      scale_x_continuous(breaks = wks) +
      labs(x = "Rankings week", y = "Glicko-2 rating", color = NULL,
           title = "Rating trajectory") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "right")
  })

  output$pa_table <- renderTable({
    g <- games(); req(g)
    ids <- identity()
    pa <- create_points_against_table(g)
    pa$Team <- setNames(ids$owner, ids$roster_id)[as.character(pa$Team)]
    pa[order(pa$total_pa), ]
  }, digits = 1)

  # League history: walks previous_league_id through all seasons.
  # Computed once per session (reactive caches the result).
  history <- reactive({
    withProgress(
      message = "Building league history from the Sleeper API...",
      value = 0.4,
      fetch_league_history(cfg()$league_id, cfg()$owner_map)
    )
  })

  output$champions <- renderTable({
    ch <- history()$champions
    validate(need(nrow(ch) > 0, "No completed seasons with a champion yet."))
    ch
  }, striped = TRUE)

  output$alltime <- renderTable({
    t <- history()$totals
    validate(need(!is.null(t), "No completed games in league history yet."))
    t
  }, striped = TRUE, digits = 3)

  output$h2h <- renderTable({
    h <- history()$h2h
    validate(need(!is.null(h), "No completed games in league history yet."))
    h
  }, rownames = TRUE, striped = TRUE)
}

shinyApp(ui, server)
