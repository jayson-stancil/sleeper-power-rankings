# =============================================================================
# Runs the power rankings pipeline for every league config in leagues/.
# Executed weekly by GitHub Actions (.github/workflows/weekly.yml); outputs
# are committed back to the repo under each league's data_dir.
#
# A league with no completed weeks (e.g., pre-draft) is skipped, not fatal.
# =============================================================================

source("R/engine.R")

league_files <- list.files("leagues", pattern = "\\.R$", full.names = TRUE)
if (!length(league_files)) stop("No league configs found in leagues/.")

for (f in league_files) {
    cfg <- source(f, local = new.env())$value
    message("\n=== ", cfg$league_tag, " (", cfg$season_label, ") ===")
    tryCatch(
          run_power_rankings(
                  league_id     = cfg$league_id,
                  league_tag    = cfg$league_tag,
                  season_label  = cfg$season_label,
                  base_dir      = cfg$data_dir,
                  roster_scores = cfg$roster_scores,
                  owner_map     = cfg$owner_map
                ),
          error = function(e) message("Skipped ", cfg$league_tag, ": ",
                                                                      conditionMessage(e))
        )
  }
message("\nAll leagues processed.")
