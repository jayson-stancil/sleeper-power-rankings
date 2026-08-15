# =============================================================================
# Runs the power rankings pipeline for every league config in leagues/.
# Executed weekly by GitHub Actions (.github/workflows/weekly.yml); outputs
# are committed back to the repo under each league's data_dir.
#
# A league with no completed weeks (e.g., pre-draft) is skipped, not fatal.
# =============================================================================

source("R/engine.R")
source("R/simulate.R")

league_files <- list.files("leagues", pattern = "\\.R$", full.names = TRUE)
if (!length(league_files)) stop("No league configs found in leagues/.")

# Sleeper's full players/nfl reference (~5MB); fetched once and reused across
# every league's transaction cache update below.
players_ref <- fetch_sleeper_players()

for (f in league_files) {
cfg <- source(f, local = new.env())$value
message("\n=== ", cfg$league_tag, " (", cfg$season_label, ") ===")

# Resolve roster_scores per roster_score_source (see the league config for
# the source contract). Falls back to NULL (all teams start at 1500) if
# FantasyCalc is unreachable, rather than failing the whole run.
roster_scores <- cfg$roster_scores
if (identical(cfg$roster_score_source, "fantasycalc")) {
rs <- tryCatch(
compute_roster_scores(cfg$league_id, is_dynasty = isTRUE(cfg$is_dynasty),
ppr = if (is.null(cfg$ppr)) 1 else cfg$ppr),
error = function(e) NULL)
if (is.null(rs)) {
message("FantasyCalc roster scores unavailable for ", cfg$league_tag,
"; falling back to flat 1500 initial ratings.")
} else {
roster_scores <- rs$total_value
}
}

tryCatch(
run_power_rankings(
league_id = cfg$league_id,
league_tag = cfg$league_tag,
season_label = cfg$season_label,
base_dir = cfg$data_dir,
roster_scores = roster_scores,
owner_map = cfg$owner_map
),
error = function(e) message("Skipped ", cfg$league_tag, ": ",
conditionMessage(e))
)

# ---- Transaction cache -- incremental, non-fatal on failure.
tryCatch(
update_transactions_cache(cfg$league_id, 0:18, players_ref,
cfg$owner_map, cfg$data_dir),
error = function(e) message("Transaction cache skipped for ",
cfg$league_tag, ": ", conditionMessage(e))
)

# ---- Season simulation (ffsimulator) -- opt-in, non-fatal on failure.
if (isTRUE(cfg$enable_simulation)) {
message("Running season simulation (ffsimulator) for ", cfg$league_tag,
"...")
tryCatch(
write_ff_simulation(cfg),
error = function(e) message("Simulation skipped for ", cfg$league_tag,
": ", conditionMessage(e))
)
}
}
message("\nAll leagues processed.")
