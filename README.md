# Sleeper Power Rankings

Automated fantasy football power rankings: a scheduled GitHub Action pulls
matchups from the Sleeper API every Tuesday, runs a Glicko-2 rating pipeline,
and commits the results to `data/`. A Shiny app (`app.R`) presents the
rankings with Sleeper team names and avatars, week-over-week movement, and
rating trajectories.

## Structure

| Path | Role |
|---|---|
| `R/engine.R` | League-agnostic pipeline (`run_power_rankings()`) |
| `leagues/*.R` | One config per league (ID, owner map, preseason scores) |
| `run_all.R` | Runs the pipeline for every league config |
| `data/` | Weekly outputs, committed by the Action |
| `app.R` | Shiny app |
| `.github/workflows/weekly.yml` | Tuesday 6 AM ET schedule + manual trigger |

## Adding a league

Copy `leagues/adg_redraft_2026.R`, edit `league_id`, `league_tag`,
`season_label`, `data_dir`, and `owner_map` (or set `owner_map = NULL`
to use Sleeper display names). The Action and the app pick it up automatically.

## Deployment

The app deploys from this repo (Posit Connect Cloud or shinyapps.io).
`GH_USER` in `app.R` points at this repo's owner so the app reads fresh
data from raw.githubusercontent.com; it falls back to the bundled `data/`
copy if offline.

## Method

Glicko-2 (PlayerRatings), recomputed each week over all completed games.
Initial-status volatility is set by latest-week performance (top scorer 0.08;
above-median winner 0.06; below-median winner 0.05; above-median loser 0.04;
else 0.03). Preseason ratings are a Gaussian transform of draft-based roster
scores (mean 1500, SD 100), deviation 200.
