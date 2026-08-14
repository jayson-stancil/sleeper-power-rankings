# =============================================================================
# Sleeper Power Rankings -- Shiny app
# Presentation layer: reads weekly rankings CSVs committed to GitHub by the
# scheduled Action, pulls team names/avatars/roster values live from the
# Sleeper and FantasyCalc APIs, and renders rankings, league stats, and
# league history.
# =============================================================================

library(shiny)
library(gt)
library(ggplot2)
library(DT)

# ---- Repo constants (set GH_USER after the repo exists) ---------------------
GH_USER   <- "jayson-stancil"
GH_REPO   <- "sleeper-power-rankings"
GH_BRANCH <- "main"

source("R/engine.R")   # sleeper(), build_team_table(), stats helpers

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
        id = "main_tabs",
        tabPanel("Rankings", gt_output("rank_table")),
        tabPanel("League Stats",
                 selectInput("stats_view", "View:",
                            choices = c("Summary", "Points For",
                                       "Rating Trajectory",
                                       "Points Against", "Overall Wins"),
                            selected = "Summary"),
                 conditionalPanel(
                   "input.stats_view == 'Summary'",
                   h4("Season Summary"),
                   helpText("OVW = Overall Wins: teams beaten that week if",
                            "every team played every team (all-play record)."),
                   tableOutput("summary_table")
                 ),
                 conditionalPanel(
                   "input.stats_view == 'Rating Trajectory'",
                   helpText("Highlighted owners are drawn in color; the rest",
                            "of the league shows as gray context lines."),
                   selectizeInput("traj_owners", "Highlight:", choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = "Select owners...")),
                   plotOutput("trajectory", height = "550px")
                 ),
                 conditionalPanel(
                   "input.stats_view == 'Points For'",
                   tableOutput("pf_table")
                 ),
                 conditionalPanel(
                   "input.stats_view == 'Points Against'",
                   tableOutput("pa_table")
                 ),
                 conditionalPanel(
                   "input.stats_view == 'Overall Wins'",
                   helpText("Teams beaten each week if every team played",
                            "every team (all-play record), not just the",
                            "actual opponent."),
                   tableOutput("ow_table")
                 )
        ),
        tabPanel("Transactions",
                 helpText("All season adds/drops and trades. Click a column",
                          "header to sort; use the filter boxes to narrow",
                          "by Type or Team."),
                 DT::dataTableOutput("transactions_table")
        ),
        tabPanel("League History",
                 h4("Champions"),
                 tableOutput("champions"),
                 h4("All-Time Regular-Season Records"),
                 tableOutput("alltime"),
                 h4("Head-to-Head"),
                 helpText("Head-to-head covers Sleeper seasons (2023-present);",
                          "game-level data is unavailable for earlier years."),
                 selectInput("h2h_owner", "Show record for:", choices = NULL),
                 tableOutput("h2h"))
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

  # Live FantasyCalc roster scores for the current league (NULL if disabled
  # or unreachable). Cached per session; used by both the Glicko-2 seed at
  # pipeline time (see run_all.R) and the Summary tab's ROST SCORE column.
  roster_scores_live <- reactive({
    c_ <- cfg()
    if (!identical(c_$roster_score_source, "fantasycalc")) return(NULL)
    tryCatch(
      compute_roster_scores(c_$league_id, is_dynasty = isTRUE(c_$is_dynasty),
                            ppr = if (is.null(c_$ppr)) 1 else c_$ppr),
      error = function(e) NULL)
  })

  # ---- Transactions ---------------------------------------------------------
  # Lazy: nothing here fetches until the Transactions tab is actually opened.

  # Sleeper's full players/nfl reference (~5MB); fetched once per session,
  # the first time it's needed.
  players_ref <- reactive(fetch_sleeper_players())

  transactions_data <- reactive({
    req(input$main_tabs == "Transactions")
    c_ <- cfg()
    withProgress(message = "Loading season transactions...", value = 0.4, {
      fetch_league_transactions(c_$league_id, 0:18, players_ref(), identity())
    })
  })

  output$transactions_table <- DT::renderDataTable({
    d <- transactions_data()
    validate(need(nrow(d) > 0, "No transactions recorded yet."))
    d$Type <- factor(d$Type, levels = c("Add/Drop", "Trade"))
    d$Team <- factor(d$Team)
    d
  }, filter = "top", rownames = FALSE,
     options = list(pageLength = 25, order = list(list(0, "desc"))))

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

  # ---- League Stats: Summary --------------------------------------------

  summary_data <- reactive({
    c_ <- cfg()
    wks <- sort(week_from_filename(weekly_files()), decreasing = TRUE)
    validate(need(length(wks) > 0, "No rankings yet - season not started."))
    latest <- weekly_csv(wks[1])
    validate(need(!is.null(latest), "No rankings file for the latest week."))

    g <- games()
    validate(need(!is.null(g), "No matchup data yet."))

    ids <- identity()
    id_to_owner <- setNames(ids$owner, ids$roster_id)

    out <- data.frame(Team = latest$Player, WINS = as.integer(latest$Win),
                      stringsAsFactors = FALSE)
    out$`WINS RANK` <- rank(-out$WINS, ties.method = "min")

    # ROST SCORE: rank 1-16 by summed roster value (live FantasyCalc, or the
    # manual roster_scores vector as a fallback; NA if neither is available).
    rs <- roster_scores_live()
    if (!is.null(rs)) {
      rs$Owner <- id_to_owner[as.character(rs$roster_id)]
      out$`ROST SCORE` <- as.integer(
        rank(-rs$total_value[match(out$Team, rs$Owner)], ties.method = "min"))
    } else if (!is.null(c_$roster_scores)) {
      rid <- ids$roster_id[match(out$Team, ids$owner)]
      rs_vec <- c_$roster_scores[match(rid, sort(unique(ids$roster_id)))]
      out$`ROST SCORE` <- as.integer(rank(-rs_vec, ties.method = "min"))
    } else {
      out$`ROST SCORE` <- NA_integer_
    }

    # Overall wins (all-play), summed across the season
    ow <- compute_overall_wins(g)
    ow_season <- aggregate(overall_wins ~ ID, ow, sum)
    ow_season$Owner <- id_to_owner[as.character(ow_season$ID)]
    out$OVW <- round(ow_season$overall_wins[match(out$Team, ow_season$Owner)], 1)
    out$`OVW RANK` <- as.integer(rank(-out$OVW, ties.method = "min"))

    # Average points for
    long <- do.call(rbind, lapply(unique(g$week), function(w)
      transform_games_df(g, w)))
    pf <- aggregate(Points ~ ID, long, mean)
    pf$Owner <- id_to_owner[as.character(pf$ID)]
    out$`AVG PF` <- round(pf$Points[match(out$Team, pf$Owner)], 2)
    out$`AVG PF RANK` <- as.integer(rank(-out$`AVG PF`, ties.method = "min"))

    out[order(out$`WINS RANK`), ]
  })

  output$summary_table <- renderTable({
    summary_data()
  }, striped = TRUE, digits = 2)

  # ---- League Stats: Rating Trajectory -----------------------------------

  traj_data <- reactive({
    wks <- sort(week_from_filename(weekly_files()))
    validate(need(length(wks) > 1, "Trajectories appear after two weeks."))
    do.call(rbind, lapply(wks, function(w) {
      d <- weekly_csv(w)
      if (is.null(d)) return(NULL)
      data.frame(week = w, Player = d$Player, Rating = d$Rating)
    }))
  })

  # Default highlight: current top 3 by rating
  observeEvent(traj_data(), {
    d <- traj_data()
    latest <- d[d$week == max(d$week), ]
    top3 <- latest$Player[order(-latest$Rating)][seq_len(min(3, nrow(latest)))]
    updateSelectizeInput(session, "traj_owners",
                         choices = sort(unique(d$Player)), selected = top3)
  })

  output$trajectory <- renderPlot({
    d <- traj_data()
    wks <- sort(unique(d$week))
    sel <- input$traj_owners

    p <- ggplot(d, aes(week, Rating, group = Player)) +
      geom_line(color = "grey80", linewidth = 0.6) +
      geom_hline(yintercept = 1500, linetype = "dashed", color = "grey55") +
      scale_x_continuous(breaks = wks,
                         expand = expansion(mult = c(0.02, 0.22))) +
      labs(x = "Rankings week", y = "Glicko-2 rating",
           title = "Rating Trajectory") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")

    if (length(sel)) {
      dh <- d[d$Player %in% sel, , drop = FALSE]
      lab <- dh[dh$week == max(wks), , drop = FALSE]
      p <- p +
        geom_line(data = dh, aes(color = Player), linewidth = 1.3) +
        geom_point(data = dh, aes(color = Player), size = 2.2) +
        geom_text(data = lab,
                  aes(color = Player,
                      label = paste0(Player, " (", round(Rating), ")")),
                  hjust = -0.08, fontface = "bold", size = 4.2,
                  show.legend = FALSE)
    }
    p
  })

  # ---- League Stats: Points For -------------------------------------------

  output$pf_table <- renderTable({
    g <- games(); req(g)
    create_points_for_table(g, identity())
  }, digits = 2)

  # ---- League Stats: Points Against --------------------------------------

  output$pa_table <- renderTable({
    g <- games(); req(g)
    ids <- identity()
    pa <- create_points_against_table(g)
    pa$Team <- setNames(ids$owner, ids$roster_id)[as.character(pa$Team)]
    pa[order(pa$total_pa), ]
  }, digits = 2)

  # ---- League Stats: Overall Wins ----------------------------------------

  output$ow_table <- renderTable({
    g <- games(); req(g)
    ids <- identity()
    t <- build_overall_wins_table(g)
    t$Team <- setNames(ids$owner, ids$roster_id)[as.character(t$Team)]
    t[order(-t$Total), ]
  }, digits = 1)

  # ---- League History -----------------------------------------------------
  # Lazy: walks previous_league_id through all seasons the first time the
  # League History tab is opened, then caches the result for the session.
  history <- reactive({
    req(input$main_tabs == "League History")
    withProgress(
      message = "Building league history from the Sleeper API...",
      value = 0.4,
      fetch_league_history(cfg()$league_id, cfg()$owner_map,
                           cfg()$history_seed)
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

  observeEvent(history(), {
    lg <- history()$long
    if (!is.null(lg)) {
      owners <- sort(unique(lg$owner))
      updateSelectInput(session, "h2h_owner", choices = owners,
                        selected = owners[1])
    }
  })

  # One owner at a time: vertical table reads cleanly on phones
  output$h2h <- renderTable({
    lg <- history()$long
    validate(need(!is.null(lg), "No completed games in league history yet."))
    me <- req(input$h2h_owner)
    sub <- lg[lg$owner == me, , drop = FALSE]
    validate(need(nrow(sub) > 0, "No games recorded for this owner."))
    W <- tapply(sub$win == 1,   sub$opponent, sum)
    L <- tapply(sub$win == 0,   sub$opponent, sum)
    T <- tapply(sub$win == 0.5, sub$opponent, sum)
    out <- data.frame(
      Opponent = names(W),
      Record   = paste0(W, "-", L,
                        ifelse(T > 0, paste0("-", T), "")),
      `Win %`  = round((W + 0.5 * T) / (W + L + T), 3),
      check.names = FALSE, row.names = NULL)
    out[order(-out$`Win %`, out$Opponent), ]
  }, striped = TRUE, digits = 3)
}

shinyApp(ui, server)
