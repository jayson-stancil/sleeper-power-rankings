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

# league_id            Sleeper league ID (string)
# season               NFL season as an integer, e.g. 2026
# roster_id_to_owner   Named vector/list: as.character(roster_id) -> owner
# n_seasons            Number of seasons to simulate (default 250)
# n_weeks              Weeks per simulated season (default 14, matches this
#                       app's Weeks 1-14 regular-season convention)
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

  conn <- ffsimulator::sleeper_connect(season = season, league_id = league_id)
  sim  <- ffsimulator::ff_simulate(conn, n_seasons = n_seasons,
                                   n_weeks = n_weeks, seed = seed,
                                   verbose = FALSE)

  playoff_teams <- tryCatch(
    sleeper(paste0("league/", league_id))$settings$playoff_teams,
    error = function(e) NULL)
  if (is.null(playoff_teams) || is.na(playoff_teams)) playoff_teams <- 6L

  # ---- Per-simulated-season standings (drives playoff odds + rank plot).
  # NOTE: ffscrapr's franchise_id for Sleeper connections is expected to
  # equal the Sleeper roster_id (both are plain small integers for this
  # platform, unlike e.g. MFL's zero-padded IDs). This is the one assumption
  # in this file that isn't directly confirmed by ffscrapr's docs -- the
  # is.na(Owner) check below is a safety net in case it's ever wrong.
  ss <- as.data.frame(sim$summary_season)
  ss$roster_id <- as.integer(ss$franchise_id)
  ss <- ss[order(ss$season, -ss$h2h_wins, -ss$points_for), ]
  ss$sim_rank <- ave(seq_len(nrow(ss)), ss$season, FUN = seq_along)
  n_teams <- length(unique(ss$roster_id))
  ss$made_playoffs <- as.integer(ss$sim_rank <= playoff_teams)
  ss$last_place    <- as.integer(ss$sim_rank == n_teams)
  ss$Owner <- unname(roster_id_to_owner[as.character(ss$roster_id)])

  odds <- aggregate(cbind(made_playoffs, last_place) ~ roster_id, ss, mean)

  # ---- Across-all-simulations team summary.
  summ <- as.data.frame(sim$summary_simulation)
  summ$roster_id <- as.integer(summ$franchise_id)
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
      n_weeks = n_weeks, run_date = as.character(Sys.Date()))
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
