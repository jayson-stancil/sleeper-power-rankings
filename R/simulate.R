# =============================================================================
# Season simulation (ffsimulator / ffscrapr) -- OPTIONAL, opt-in per league via
# the league config's enable_simulation = TRUE.
#
# This file is sourced ONLY by run_all.R, never by app.R. ffsimulator pulls in
# ffscrapr, nflreadr, and data.table -- heavy dependencies with real download
# cost (historical nflverse scoring data) -- so they're isolated here and only
# ever installed/run in the weekly GitHub Action, not in the Shiny app itself.
#
# Output: two small CSVs per league, written to <data_dir>/Simulation/:
#   summary_simulation.csv  -- one row per team, aggregated across every
#                               simulated season (h2h win%, all-play win%,
#                               points for/against, playoff/last-place odds)
#   summary_season.csv      -- one row per (simulated season, team), used for
#                               the standings-distribution plot in the app
# The app reads these via read_repo_csv(); it never loads this file or calls
# ffsimulator directly.
#
# --- 2026-10-13 rewrite: simulate the REST of the season, not a fresh one ---
# The original version called ff_simulate(..., n_weeks = 14, actual_schedule =
# TRUE) every single week, which re-projects every week of the season from
# scratch -- including weeks that have already been played -- so the output
# never reflected the real, current standings and looked identical run to
# run. This version:
#   1. Figures out which regular-season weeks are already complete, the same
#      way run_power_rankings() in engine.R does (league settings + Sleeper's
#      live state/nfl week/season_type).
#   2. Computes each team's REAL record/points/all-play-wins through those
#      completed weeks directly from Sleeper's matchup data, reusing the
#      existing build_matchups()/transform_games_df()/compute_overall_wins()
#      helpers from engine.R (no new fetch logic).
#   3. Simulates ONLY the remaining weeks (n_weeks = number of weeks left),
#      then adds the real, already-locked-in record onto every simulated
#      season before computing standings/odds.
#   actual_schedule is now FALSE for the simulated portion: ff_simulate()'s
#   n_weeks is a relative week count (1..n_weeks), so once n_weeks no longer
#   equals the full season, actual_schedule=TRUE would pull the REAL
#   schedule's weeks 1..n_weeks (the ones already played) instead of the
#   ones still ahead -- wrong opponents for the part we're actually trying
#   to project. A random schedule over the same remaining teams/weeks is the
#   standard, well-understood fallback used by most "rest of season" odds
#   tools, at the cost of not matching the exact real remaining matchups.
#
# ASSUMPTIONS WORTH CONFIRMING ON THE FIRST LOCAL TEST RUN (flagged because
# this file could not be run or syntax-checked against a live R session
# before being handed over):
#   - ffscrapr's franchise_id for a Sleeper connection equals the Sleeper
#     roster_id (already assumed by the pre-rewrite code; unchanged here).
#   - ff_simulate() accepts n_weeks down to 1 without erroring (used when
#     only one regular-season week remains).
#   - potential_points in summary_simulation.csv now reflects ONLY the
#     simulated remaining weeks (there is no "real" optimal-lineup figure to
#     fold in), so it will read lower later in the season than it did when
#     it covered all 14 weeks. Everything else (h2h wins/winpct, points
#     for/against, all-play wins/winpct, playoff/last-place odds) is the
#     real-to-date value plus the simulated remainder.
# =============================================================================

# nflreadr (a transitive dep of ffsimulator) defaults to an in-memory cache,
# which is useless in CI since every weekly run is a fresh process. Switch it
# to a filesystem cache so repeated downloads of the same historical nflverse
# data (base_seasons 2012:2020) are skipped on subsequent runs. The actual
# directory is controlled by the R_USER_CACHE_DIR env var (set in
# .github/workflows/weekly.yml) and persisted across runs via actions/cache;
# this option must be set before ffsimulator/nflreadr's namespace first
# loads, so it lives here at the top of the file rather than inside a
# function. Harmless no-op locally (falls back to a per-session temp dir).
options(nflreadr.cache = "filesystem")

# Real season-to-date record for every roster in the league, through
# completed_weeks only. Returns one row per roster_id (including rosters
# with zero completed games, e.g. before Week 1) with:
#   real_wins, real_points_for, real_points_against  -- head-to-head
#   real_allplay_wins -- sum, across completed_weeks, of each week's
#                         all-play ("overall") wins (see compute_overall_wins
#                         in engine.R; matches the League Stats tab's
#                         "Overall Wins" definition)
# Built entirely from the same build_matchups()/transform_games_df()/
# compute_overall_wins() helpers engine.R already uses for the live power
# rankings and League Stats tables -- no new Sleeper-fetching logic.
.ff_real_record <- function(league_id, completed_weeks, all_roster_ids) {
  if (!length(completed_weeks)) {
    return(data.frame(
      roster_id = all_roster_ids,
      real_wins = 0,
      real_points_for = 0,
      real_points_against = 0,
      real_allplay_wins = 0
    ))
  }

  games <- build_matchups(league_id, completed_weeks)

  h2h_long <- rbind(
    data.frame(roster_id = games$team,
               w = games$result,
               pf = games$team_points,
               pa = games$opponent_points),
    data.frame(roster_id = games$opponent,
               w = 1 - games$result,
               pf = games$opponent_points,
               pa = games$team_points)
  )
  h2h_agg <- aggregate(cbind(real_wins = w, real_points_for = pf,
                             real_points_against = pa) ~ roster_id,
                       h2h_long, sum)

  overall <- compute_overall_wins(games)
  allplay_agg <- aggregate(overall_wins ~ ID, overall, sum)
  names(allplay_agg) <- c("roster_id", "real_allplay_wins")

  out <- merge(h2h_agg, allplay_agg, by = "roster_id", all = TRUE)

  missing_ids <- setdiff(all_roster_ids, out$roster_id)
  if (length(missing_ids)) {
    out <- rbind(out, data.frame(
      roster_id = missing_ids, real_wins = 0, real_points_for = 0,
      real_points_against = 0, real_allplay_wins = 0
    ))
  }
  out[is.na(out)] <- 0
  out
}

# league_id            Sleeper league ID (string)
# season               NFL season as an integer, e.g. 2026
# roster_id_to_owner   Named vector/list: as.character(roster_id) -> owner
# n_seasons            Number of seasons to simulate (default 250)
# n_weeks              Regular-season length for this league (default 14,
#                       matches this app's Weeks 1-14 convention). Used only
#                       to know how many weeks the regular season has, NOT
#                       as the number of weeks actually simulated -- that is
#                       always just the weeks that haven't happened yet.
# seed                 Fixed for reproducible week-to-week comparisons
run_ff_simulation <- function(league_id, season, roster_id_to_owner,
                              n_seasons = 250, n_weeks = 14, seed = 42) {
  # Hard cap: protects CI runtime against a misconfigured league (e.g. a
  # typo'd extra zero in sim_n_seasons). 1000 seasons is already far more
  # than needed for stable odds at n_weeks = 14.
  if (n_seasons > 1000) {
    message("n_seasons (", n_seasons, ") exceeds the 1000 cap; using 1000.")
    n_seasons <- 1000
  }

  required <- c("ffsimulator", "ffscrapr")
  missing  <- required[!vapply(required, requireNamespace, logical(1),
                               quietly = TRUE)]
  if (length(missing)) {
    stop("Missing packages for simulation: ", paste(missing, collapse = ", "),
        "\nInstall with: install.packages(c(",
        paste0('"', missing, '"', collapse = ", "), "))")
  }

  # ---- Completed vs. remaining regular-season weeks (mirrors
  # run_power_rankings()'s logic in engine.R exactly, so the simulation and
  # the live power rankings always agree on "what's already happened").
  league <- sleeper(paste0("league/", league_id))
  state  <- sleeper("state/nfl")
  reg_season_weeks <- seq_len(n_weeks)

  if (identical(state$season, league$season) &&
      identical(state$season_type, "regular")) {
    completed_weeks <- reg_season_weeks[reg_season_weeks < state$week]
  } else if (state$season > league$season ||
             identical(state$season_type, "post")) {
    completed_weeks <- reg_season_weeks
  } else {
    completed_weeks <- integer(0)
  }
  remaining_weeks <- setdiff(reg_season_weeks, completed_weeks)

  all_roster_ids <- as.integer(names(roster_id_to_owner))
  n_teams <- length(all_roster_ids)
  real <- .ff_real_record(league_id, completed_weeks, all_roster_ids)

  playoff_teams <- tryCatch(
    league$settings$playoff_teams,
    error = function(e) NULL)
  if (is.null(playoff_teams) || is.na(playoff_teams)) playoff_teams <- 6L

  total_games <- length(completed_weeks) + length(remaining_weeks)

  if (!length(remaining_weeks)) {
    # Regular season is already fully played out live -- there is nothing
    # left to project. Every "simulated" season is identical (the real,
    # final record), so skip ffsimulator entirely rather than asking it to
    # simulate zero/one meaningless weeks.
    ss <- do.call(rbind, lapply(seq_len(n_seasons), function(s) {
      d <- real
      d$season <- s
      d$roster_id <- real$roster_id
      d
    }))
    ss$h2h_wins       <- ss$real_wins
    ss$points_for     <- ss$real_points_for
    ss$points_against <- ss$real_points_against
    ss$allplay_wins   <- ss$real_allplay_wins
  } else {
    conn <- ffsimulator::sleeper_connect(season = season, league_id = league_id)
    sim  <- ffsimulator::ff_simulate(conn, n_seasons = n_seasons,
                                     n_weeks = length(remaining_weeks),
                                     seed = seed, verbose = FALSE,
                                     actual_schedule = FALSE)

    ss <- as.data.frame(sim$summary_season)
    # NOTE: ffscrapr's franchise_id for Sleeper connections is expected to
    # equal the Sleeper roster_id (both are plain small integers for this
    # platform, unlike e.g. MFL's zero-padded IDs). This is the one
    # assumption in this file that isn't directly confirmed by ffscrapr's
    # docs -- the is.na(Owner) check below is a safety net in case it's
    # ever wrong.
    ss$roster_id <- as.integer(ss$franchise_id)

    ss <- merge(ss, real, by = "roster_id", all.x = TRUE)
    fill_cols <- c("real_wins", "real_points_for", "real_points_against",
                  "real_allplay_wins")
    ss[fill_cols] <- lapply(ss[fill_cols], function(x) ifelse(is.na(x), 0, x))

    # Fold the real, already-locked-in record onto every simulated
    # "rest of season" season.
    ss$h2h_wins       <- ss$h2h_wins + ss$real_wins
    ss$points_for     <- ss$points_for + ss$real_points_for
    ss$points_against <- ss$points_against + ss$real_points_against
    ss$allplay_wins   <- ss$allplay_wins + ss$real_allplay_wins
  }

  ss$h2h_winpct <- ss$h2h_wins / total_games
  ss$allplay_winpct <- ss$allplay_wins / (total_games * (n_teams - 1))

  # ---- Per-simulated-season standings (drives playoff odds + rank plot).
  ss <- ss[order(ss$season, -ss$h2h_wins, -ss$points_for), ]
  ss$sim_rank <- ave(seq_len(nrow(ss)), ss$season, FUN = seq_along)
  ss$made_playoffs <- as.integer(ss$sim_rank <= playoff_teams)
  ss$last_place    <- as.integer(ss$sim_rank == n_teams)
  ss$Owner <- unname(roster_id_to_owner[as.character(ss$roster_id)])

  odds <- aggregate(cbind(made_playoffs, last_place) ~ roster_id, ss, mean)

  # ---- Across-all-simulations team summary. Computed fresh from the
  # real-record-adjusted ss above rather than reusing ff_simulate()'s own
  # summary_simulation, since that one was aggregated BEFORE the real record
  # was folded in and would understate every team's actual standing.
  if (!length(remaining_weeks)) {
    # Every season is identical, so the "across all simulations" summary is
    # just the real record itself, replicated with the same column shape
    # summary_simulation.csv normally has.
    summ <- data.frame(
      roster_id        = real$roster_id,
      league_id        = league_id,
      franchise_id     = real$roster_id,
      franchise_name   = NA_character_,
      seasons          = n_seasons,
      h2h_wins         = real$real_wins,
      h2h_winpct       = real$real_wins / total_games,
      allplay_wins     = real$real_allplay_wins,
      allplay_winpct   = real$real_allplay_wins / (total_games * (n_teams - 1)),
      points_for       = real$real_points_for,
      points_against   = real$real_points_against,
      potential_points = NA_real_,
      stringsAsFactors = FALSE
    )
  } else {
    summ <- ffsimulator::ffs_summarise_simulation(ss)
    summ$roster_id <- as.integer(summ$franchise_id)
  }
  summ <- merge(summ, odds, by = "roster_id", all.x = TRUE)
  summ$Owner <- unname(roster_id_to_owner[as.character(summ$roster_id)])

  if (any(is.na(summ$Owner))) {
    warning("ff_simulate: could not map some franchise_id values to a ",
            "Sleeper roster_id/owner -- compare summary_simulation.csv's ",
            "franchise_name column against the league roster before ",
            "trusting these results.")
  }

  list(summary_simulation = summ, summary_season = ss,
      playoff_teams = playoff_teams, n_seasons = n_seasons,
      n_weeks = length(remaining_weeks), run_date = as.character(Sys.Date()))
}

# Runs run_ff_simulation() for one league config and writes its CSVs.
# Wrapped in tryCatch by the caller (run_all.R) so a simulation failure never
# blocks the rest of the weekly pipeline.
write_ff_simulation <- function(cfg) {
  users   <- sleeper(paste0("league/", cfg$league_id, "/users"))
  rosters <- sleeper(paste0("league/", cfg$league_id, "/rosters"))
  teams   <- build_team_table(users, rosters, cfg$owner_map)
  id_to_owner <- setNames(teams$owner, teams$roster_id)

  sim_season <- as.integer(substr(cfg$season_label, 1, 4))
  sim <- run_ff_simulation(
    league_id          = cfg$league_id,
    season             = sim_season,
    roster_id_to_owner = id_to_owner,
    n_seasons          = if (is.null(cfg$sim_n_seasons)) 250
                         else cfg$sim_n_seasons,
    n_weeks            = if (is.null(cfg$sim_n_weeks)) 14 else cfg$sim_n_weeks
  )

  sim_dir <- file.path(cfg$data_dir, "Simulation")
  if (!dir.exists(sim_dir)) dir.create(sim_dir, recursive = TRUE)
  write.csv(sim$summary_simulation,
            file.path(sim_dir, "summary_simulation.csv"), row.names = FALSE)
  write.csv(sim$summary_season,
            file.path(sim_dir, "summary_season.csv"), row.names = FALSE)
  writeLines(c(paste0("playoff_teams,", sim$playoff_teams),
              paste0("n_seasons,", sim$n_seasons),
              paste0("n_weeks,", sim$n_weeks),
              paste0("run_date,", sim$run_date)),
            file.path(sim_dir, "meta.csv"))
  message("Simulation complete: ", sim$n_seasons, " seasons written to ",
          sim_dir)
  invisible(sim)
}
