# =============================================================================
# Sleeper Power Rankings Engine (league-agnostic)
#
# Provides run_power_rankings(): Sleeper API -> matchups CSV -> Glicko-2
# ratings -> weekly CSV + shareable graphic (PNG + PDF) with Sleeper team
# names and avatars.
#
# This file contains NO league-specific configuration. Each league gets its
# own runner script (see leagues/adg_redraft_2026.R) that sources this file
# and calls run_power_rankings() with its own settings.
#
# Author: Jayson Stancil | Refactored 2026-07-24
# =============================================================================

# --------------------------- DEPENDENCIES ------------------------------------

required_pkgs <- c("jsonlite", "PlayerRatings", "gt", "webshot2")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
stop("Missing packages: ", paste(missing_pkgs, collapse = ", "),
"\nInstall with: install.packages(c(",
paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}

# --------------------------- API HELPERS -------------------------------------

fetch_json <- function(url) {
out <- tryCatch(jsonlite::fromJSON(url),
error = function(e) stop("Sleeper API request failed: ", url,
"\n", conditionMessage(e)))
if (is.null(out)) stop("Sleeper API returned NULL for: ", url)
out
}

sleeper <- function(path) fetch_json(paste0("https://api.sleeper.app/v1/", path))

# Like sleeper(), but disables jsonlite's automatic data.frame simplification.
# Needed for the transactions endpoint: adds/drops are per-transaction named
# objects (player_id -> roster_id) that vary in their keys row to row, so
# jsonlite's default simplifyDataFrame=TRUE flattens them into one sparse
# wide data.frame across the whole response (a column per distinct player_id
# seen, NA elsewhere) instead of a clean list per transaction. With
# simplifyDataFrame=FALSE each transaction stays a plain nested list, so
# t$adds/t$drops are simple named lists as fetch_league_transactions() below
# expects.
sleeper_raw <- function(path) {
url <- paste0("https://api.sleeper.app/v1/", path)
out <- tryCatch(jsonlite::fromJSON(url, simplifyDataFrame = FALSE),
error = function(e) stop("Sleeper API request failed: ", url,
"\n", conditionMessage(e)))
if (is.null(out)) stop("Sleeper API returned NULL for: ", url)
out
}

# --------------------------- TEAM IDENTITY -----------------------------------

# roster_id -> owner name, Sleeper team name, avatar URL.
# owner_map: optional data.frame(user_id, owner) of canonical owner names.
# When NULL (or a user is unmapped), the Sleeper display name is used.
build_team_table <- function(users, rosters, owner_map = NULL) {
team_name <- users$metadata$team_name
if (is.null(team_name)) team_name <- rep(NA_character_, nrow(users))
avatar_url <- users$metadata$avatar # custom uploaded avatar (full URL)
if (is.null(avatar_url)) avatar_url <- rep(NA_character_, nrow(users))
fallback <- ifelse(is.na(users$avatar) | users$avatar == "",
NA_character_,
paste0("https://sleepercdn.com/avatars/thumbs/",
users$avatar))
u <- data.frame(
user_id = users$user_id,
display = users$display_name,
team_name = ifelse(is.na(team_name) | team_name == "",
users$display_name, team_name),
avatar_url = ifelse(is.na(avatar_url) | avatar_url == "", fallback,
avatar_url),
stringsAsFactors = FALSE
)
if (!is.null(owner_map)) {
stopifnot(all(c("user_id", "owner") %in% names(owner_map)))
u <- merge(u, owner_map, by = "user_id", all.x = TRUE)
unmapped <- is.na(u$owner)
if (any(unmapped)) {
warning("Unmapped Sleeper users (falling back to display name): ",
paste(u$display[unmapped], collapse = ", "),
". Add them to the league's owner_map.")
u$owner[unmapped] <- u$display[unmapped]
}
} else {
u$owner <- u$display
}
r <- data.frame(roster_id = rosters$roster_id,
user_id = rosters$owner_id,
stringsAsFactors = FALSE)
out <- merge(r, u, by = "user_id", all.x = TRUE)
out[order(out$roster_id),
c("roster_id", "user_id", "owner", "display", "team_name", "avatar_url")]
}

# --------------------------- MATCHUPS ----------------------------------------

# One row per game: week, team, team_points, opponent, opponent_points, result
# Convention: team = lower roster_id in the pair (matches historical CSVs).
build_matchups <- function(league_id, weeks) {
rows <- list()
for (wk in weeks) {
m <- sleeper(paste0("league/", league_id, "/matchups/", wk))
if (!is.data.frame(m) || !nrow(m)) next
if (all(m$points == 0, na.rm = TRUE)) next # unplayed week guard
for (mid in sort(unique(m$matchup_id))) {
pair <- m[!is.na(m$matchup_id) & m$matchup_id == mid, , drop = FALSE]
if (nrow(pair) != 2) {
warning("Week ", wk, " matchup ", mid, " has ", nrow(pair),
" rosters; skipping.")
next
}
pair <- pair[order(pair$roster_id), ]
tp <- pair$points[1]; op <- pair$points[2]
rows[[length(rows) + 1]] <- data.frame(
week = wk,
team = pair$roster_id[1],
team_points = tp,
opponent = pair$roster_id[2],
opponent_points = op,
result = ifelse(tp > op, 1, ifelse(tp < op, 0, 0.5))
)
}
}
if (!length(rows)) stop("No completed matchups found.")
do.call(rbind, rows)
}

# --------------------------- GLICKO-2 HELPERS --------------------------------
# Faithful port of the original ADG Redraft Team Ratings Rmd.

transform_games_df <- function(df, selected_week) {
df_week <- df[df$week == selected_week, , drop = FALSE]
rbind(
data.frame(Week = df_week$week, ID = df_week$team,
Points = df_week$team_points, WinLoss = df_week$result),
data.frame(Week = df_week$week, ID = df_week$opponent,
Points = df_week$opponent_points, WinLoss = 1 - df_week$result)
)
}

calculate_volatility <- function(df, selected_week) {
wd <- transform_games_df(df, selected_week)
top_scorer <- wd$ID[which.max(wd$Points)]
med <- median(wd$Points)
vol <- vapply(seq_len(nrow(wd)), function(i) {
if (wd$ID[i] == top_scorer) 0.08
else if (wd$WinLoss[i] == 1 && wd$Points[i] > med) 0.06
else if (wd$WinLoss[i] == 1 && wd$Points[i] < med) 0.05
else if (wd$WinLoss[i] == 0 && wd$Points[i] > med) 0.04
else 0.03
}, numeric(1))
data.frame(ID = wd$ID, volatility = vol)
}

create_points_against_table <- function(games) {
ids <- sort(unique(c(games$team, games$opponent)))
weeks <- sort(unique(games$week))
pa <- matrix(NA_real_, length(ids), length(weeks),
dimnames = list(ids, as.character(weeks)))
for (i in seq_len(nrow(games))) {
wk <- as.character(games$week[i])
pa[as.character(games$team[i]), wk] <- games$opponent_points[i]
pa[as.character(games$opponent[i]), wk] <- games$team_points[i]
}
out <- data.frame(Team = rownames(pa), pa, check.names = FALSE,
row.names = NULL)
wm <- as.matrix(out[, as.character(weeks), drop = FALSE])
out$total_pa <- rowSums(wm, na.rm = TRUE)
out$std_dev_pa <- apply(wm, 1, sd, na.rm = TRUE)
out$avg_pa <- rowMeans(wm, na.rm = TRUE)
out$med_pa <- apply(wm, 1, median, na.rm = TRUE)
out
}

# --------------------------- FANTASYCALC ROSTER SCORES -----------------------
# FantasyCalc (api.fantasycalc.com) prices skill-position players (QB/RB/WR/TE)
# from real trade data; it does not price K/DEF, which contribute 0. Values
# are keyed by Sleeper player_id, so no ID mapping is required.

fetch_fantasycalc_values <- function(is_dynasty = FALSE, num_qbs = 1,
num_teams = 12, ppr = 1) {
url <- paste0("https://api.fantasycalc.com/values/current?isDynasty=",
tolower(as.character(is_dynasty)), "&numQbs=", num_qbs,
"&numTeams=", num_teams, "&ppr=", ppr)
d <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
if (is.null(d) || !is.data.frame(d) || !("player" %in% names(d))) {
warning("FantasyCalc fetch failed or returned unexpected shape: ", url)
return(NULL)
}
data.frame(sleeper_id = as.character(d$player$sleeperId),
value = d$value, stringsAsFactors = FALSE)
}

# Sums FantasyCalc player value per Sleeper roster. Returns a data.frame
# (roster_id, total_value) ordered by roster_id, suitable for passing (after
# extracting $total_value) as run_power_rankings()'s roster_scores argument.
# is_dynasty/ppr should match the league's format; ppr = 1 for full PPR.
compute_roster_scores <- function(league_id, is_dynasty = FALSE, ppr = 1) {
rosters <- sleeper(paste0("league/", league_id, "/rosters"))
fc <- fetch_fantasycalc_values(is_dynasty = is_dynasty, num_qbs = 1,
num_teams = nrow(rosters), ppr = ppr)
if (is.null(fc)) return(NULL)
val_by_id <- setNames(fc$value, fc$sleeper_id)
totals <- vapply(rosters$players, function(pids) {
if (is.null(pids) || !length(pids)) return(0)
v <- val_by_id[as.character(pids)]
sum(v, na.rm = TRUE)
}, numeric(1))
data.frame(roster_id = rosters$roster_id, total_value = totals,
stringsAsFactors = FALSE)[order(rosters$roster_id), ]
}

# --------------------------- TRANSACTIONS --------------------------------------
# Season transaction log: adds/drops (waiver or free agency) and trades, one
# row per team side per transaction so the whole log is a single flat table
# (sortable/filterable client-side in the Shiny app via DT). No FantasyCalc
# dependency -- just Sleeper's own transaction records and player names.

fetch_sleeper_players <- function() sleeper("players/nfl")

# league_id Sleeper league ID
# weeks Integer vector of rounds to check, e.g. 0:18 (0 covers preseason
# waiver activity; extra out-of-range rounds are silently skipped)
# players_ref fetch_sleeper_players() output, for player_id -> name
# teams build_team_table() output, for roster_id -> owner
fetch_league_transactions <- function(league_id, weeks, players_ref, teams) {
id_to_owner <- setNames(teams$owner, teams$roster_id)
pname <- function(pid) {
p <- players_ref[[as.character(pid)]]
if (!is.null(p$full_name) && nzchar(p$full_name)) p$full_name
else as.character(pid)
}

rows <- list()
for (wk in weeks) {
tx_list <- tryCatch(sleeper_raw(paste0("league/", league_id, "/transactions/", wk)),
error = function(e) NULL)
if (is.null(tx_list) || !length(tx_list)) next

for (t in tx_list) {
if (!identical(t$status, "complete")) next
type <- if (identical(t$type, "trade")) "Trade" else "Add/Drop"
adds <- t$adds
drops <- t$drops
rids <- unique(c(if (!is.null(adds)) unname(unlist(adds)) else NULL,
if (!is.null(drops)) unname(unlist(drops)) else NULL,
unlist(t$roster_ids)))
if (!length(rids)) next

for (rid in rids) {
added_p <- if (!is.null(adds)) names(adds)[unlist(adds) == rid]
else character(0)
dropped_p <- if (!is.null(drops)) names(drops)[unlist(drops) == rid]
else character(0)
if (!length(added_p) && !length(dropped_p)) next
rows[[length(rows) + 1]] <- data.frame(
Date = format(as.POSIXct(t$created / 1000, origin = "1970-01-01",
tz = "America/New_York"), "%Y-%m-%d"),
Week = wk,
Type = type,
Team = unname(id_to_owner[as.character(rid)]),
Added = paste(vapply(added_p, pname, character(1)), collapse = ", "),
Dropped = paste(vapply(dropped_p, pname, character(1)), collapse = ", "),
stringsAsFactors = FALSE)
}
}
}

if (!length(rows)) {
return(data.frame(Date = character(0), Week = integer(0),
Type = character(0), Team = character(0),
Added = character(0), Dropped = character(0),
stringsAsFactors = FALSE))
}
out <- do.call(rbind, rows)
out[order(out$Date, decreasing = TRUE), ]
}

# Full transaction history across every season: walks the league chain via
# previous_league_id (same mechanism as fetch_league_history()) and calls
# fetch_league_transactions() once per season, tagging each season's rows.
# Each season gets its own roster_id -> owner mapping since rosters reshuffle
# between seasons, so owner_map (not a single teams table) is passed through
# and resolved fresh per season. Limited to Sleeper-hosted seasons (no
# transaction data exists for earlier, pre-Sleeper years).
#
# league_id Sleeper league ID for the CURRENT season (chain walks backward)
# weeks Integer vector of rounds to check per season, e.g. 0:18
# players_ref fetch_sleeper_players() output, for player_id -> name
# owner_map Optional data.frame(user_id, owner); NULL -> Sleeper display
# names, resolved per season
fetch_league_transactions_history <- function(league_id, weeks, players_ref,
owner_map = NULL) {
chain <- fetch_league_chain(league_id)
rows <- list()

for (lg in chain) {
season_rows <- tryCatch({
users <- sleeper(paste0("league/", lg$league_id, "/users"))
rosters <- sleeper(paste0("league/", lg$league_id, "/rosters"))
teams <- suppressWarnings(build_team_table(users, rosters, owner_map))
tx <- fetch_league_transactions(lg$league_id, weeks, players_ref, teams)
if (nrow(tx)) tx$Season <- lg$season
tx
}, error = function(e) NULL)
if (!is.null(season_rows) && nrow(season_rows)) {
rows[[length(rows) + 1]] <- season_rows
}
}

if (!length(rows)) {
return(data.frame(Season = character(0), Date = character(0),
Week = integer(0), Type = character(0),
Team = character(0), Added = character(0),
Dropped = character(0), stringsAsFactors = FALSE))
}
out <- do.call(rbind, rows)
out <- out[, c("Season", "Date", "Week", "Type", "Team", "Added", "Dropped")]
out[order(out$Season, out$Date, decreasing = TRUE), ]
}

# Incrementally caches per-season transaction CSVs to disk, one file per
# season under <base_dir>/Transactions/<season>.csv. Completed seasons are
# immutable once played, so a season already cached on disk is never
# refetched; only the current season (the first link in the chain) is always
# refreshed, since it's the only one still changing. Intended to be called
# from run_all.R (weekly CI), not from a live Shiny session -- this writes
# to disk, and the app should only ever read the resulting CSVs.
#
# league_id Sleeper league ID for the CURRENT season (chain walks backward)
# weeks Integer vector of rounds to check per season, e.g. 0:18
# players_ref fetch_sleeper_players() output, for player_id -> name
# owner_map Optional data.frame(user_id, owner); NULL -> Sleeper display
# names, resolved per season
# base_dir League's data_dir; CSVs written beneath <base_dir>/Transactions
update_transactions_cache <- function(league_id, weeks, players_ref,
owner_map = NULL, base_dir) {
chain <- fetch_league_chain(league_id)
if (!length(chain)) {
message("No seasons found for league ", league_id, "; nothing to cache.")
return(invisible(NULL))
}

tx_dir <- file.path(base_dir, "Transactions")
if (!dir.exists(tx_dir)) dir.create(tx_dir, recursive = TRUE)

for (i in seq_along(chain)) {
lg <- chain[[i]]
is_current_season <- (i == 1)
out_file <- file.path(tx_dir, paste0(lg$season, ".csv"))

if (!is_current_season && file.exists(out_file)) {
next # completed season already cached; immutable, skip
}

season_tx <- tryCatch({
users <- sleeper(paste0("league/", lg$league_id, "/users"))
rosters <- sleeper(paste0("league/", lg$league_id, "/rosters"))
teams <- suppressWarnings(build_team_table(users, rosters, owner_map))
fetch_league_transactions(lg$league_id, weeks, players_ref, teams)
}, error = function(e) {
message("Skipped transactions for season ", lg$season, ": ",
conditionMessage(e))
NULL
})

if (is.null(season_tx)) next
write.csv(season_tx, out_file, row.names = FALSE)
message("Transactions cached: ", out_file, " (", nrow(season_tx),
" rows)")
}

invisible(NULL)
}

# --------------------------- ROSTER DETAILS -------------------------------------
# Full roster listing per team: player name/position/NFL team (from Sleeper's
# players/nfl reference) and FantasyCalc value (NA for unpriced players, e.g.
# K/DEF). players_ref: fetch_sleeper_players() output, reused across calls.

# rosters: league/<id>/rosters response. teams: build_team_table() output
# (for roster_id -> owner). players_ref: fetch_sleeper_players() output.
build_roster_details <- function(rosters, teams, players_ref,
is_dynasty = FALSE, ppr = 1) {
fc <- fetch_fantasycalc_values(is_dynasty = is_dynasty, num_qbs = 1,
num_teams = nrow(rosters), ppr = ppr)
val_by_id <- if (!is.null(fc)) setNames(fc$value, fc$sleeper_id) else NULL

rows <- list()
for (i in seq_len(nrow(rosters))) {
rid <- rosters$roster_id[i]
pids <- rosters$players[[i]]
if (is.null(pids) || !length(pids)) next
for (pid in pids) {
p <- players_ref[[as.character(pid)]]
name <- if (!is.null(p$full_name) && nzchar(p$full_name)) p$full_name
else as.character(pid)
rows[[length(rows) + 1]] <- data.frame(
roster_id = rid,
player_id = as.character(pid),
Player = name,
Position = if (!is.null(p$position)) p$position else NA_character_,
NFL = if (!is.null(p$team)) p$team else "FA",
Value = if (!is.null(val_by_id))
unname(val_by_id[as.character(pid)]) else NA_real_,
stringsAsFactors = FALSE)
}
}
out <- do.call(rbind, rows)
out$Owner <- teams$owner[match(out$roster_id, teams$roster_id)]
out[order(out$roster_id, -out$Value, out$Player), ]
}

# --------------------------- POINTS FOR ----------------------------------------
# Team x week scoring matrix plus season Total/Avg/High/Low/Dev (sample sd),
# mirroring create_points_against_table()'s shape. teams: build_team_table()
# output, for Teams (team_name) and Name (Sleeper display_name) labels.

create_points_for_table <- function(games, teams) {
ids <- sort(unique(c(games$team, games$opponent)))
weeks <- sort(unique(games$week))
pf <- matrix(NA_real_, length(ids), length(weeks),
dimnames = list(ids, paste0("WK", weeks)))
for (i in seq_len(nrow(games))) {
wk <- paste0("WK", games$week[i])
pf[as.character(games$team[i]), wk] <- games$team_points[i]
pf[as.character(games$opponent[i]), wk] <- games$opponent_points[i]
}
wk_cols <- colnames(pf)
out <- data.frame(roster_id = as.integer(rownames(pf)), pf,
check.names = FALSE, row.names = NULL)
wm <- as.matrix(out[, wk_cols, drop = FALSE])
out$TOTAL <- rowSums(wm, na.rm = TRUE)
out$AVG <- rowMeans(wm, na.rm = TRUE)
out$HIGH <- apply(wm, 1, max, na.rm = TRUE)
out$LOW <- apply(wm, 1, min, na.rm = TRUE)
out$DEV <- apply(wm, 1, sd, na.rm = TRUE)

out$Teams <- teams$team_name[match(out$roster_id, teams$roster_id)]
out$Name <- teams$display[match(out$roster_id, teams$roster_id)]
out <- out[, c("Teams", "Name", wk_cols, "TOTAL", "AVG", "HIGH", "LOW", "DEV")]

# Footer row: league-wide median score for each week (medians across teams,
# not a team row -- Teams/Name label it, TOTAL/AVG/HIGH/LOW/DEV left blank).
med_row <- as.data.frame(as.list(apply(wm, 2, median, na.rm = TRUE)),
check.names = FALSE)
med_row$Teams <- "MEDIAN"
med_row$Name <- ""
med_row$TOTAL <- NA_real_
med_row$AVG <- NA_real_
med_row$HIGH <- NA_real_
med_row$LOW <- NA_real_
med_row$DEV <- NA_real_
med_row <- med_row[, names(out)]

rbind(out, med_row)
}

# --------------------------- OVERALL WINS (ALL-PLAY) --------------------------
# For each week, compares every team's score to every OTHER team's score that
# week (not just their actual opponent) and counts how many they'd have beaten
# (ties count as 0.5). Returns one row per team per week; summed across weeks
# for a season total. Independent of actual matchup pairing/luck.

compute_overall_wins <- function(games) {
weeks <- sort(unique(games$week))
long_all <- do.call(rbind, lapply(weeks, function(w) transform_games_df(games, w)))
long_all$overall_wins <- unlist(lapply(weeks, function(w) {
wk <- long_all[long_all$Week == w, , drop = FALSE]
vapply(seq_len(nrow(wk)), function(i) {
others <- wk$Points[-i]
sum(wk$Points[i] > others) + 0.5 * sum(wk$Points[i] == others)
}, numeric(1))
}))
long_all
}

# Wide table (one row per team, one column per week) of overall_wins, plus a
# Total column. Mirrors create_points_against_table's shape/convention.
build_overall_wins_table <- function(games) {
ow <- compute_overall_wins(games)
ids <- sort(unique(ow$ID))
weeks <- sort(unique(ow$Week))
m <- matrix(NA_real_, length(ids), length(weeks),
dimnames = list(ids, as.character(weeks)))
for (i in seq_len(nrow(ow))) {
m[as.character(ow$ID[i]), as.character(ow$Week[i])] <- ow$overall_wins[i]
}
out <- data.frame(Team = rownames(m), m, check.names = FALSE,
row.names = NULL)
wm <- as.matrix(out[, as.character(weeks), drop = FALSE])
out$Total <- rowSums(wm, na.rm = TRUE)
out
}

# --------------------------- GRAPHIC -----------------------------------------

build_graphic <- function(tbl, league_name, next_week, latest_week) {
gt_df <- tbl[, c("rank", "move", "avatar_url", "team_name", "owner",
"Rating", "rating_change", "record", "avg_pf", "avg_pa")]
gt::gt(gt_df) |>
gt::tab_header(
title = gt::md(paste0("**", league_name, " â Power Rankings**")),
subtitle = paste0("Week ", next_week,
" | Glicko-2 ratings through Week ", latest_week)
) |>
gt::text_transform(
locations = gt::cells_body(columns = "avatar_url"),
fn = function(x) gt::web_image(x, height = 32)
) |>
gt::cols_label(rank = "#", move = "", avatar_url = "", team_name = "Team",
owner = "Owner", Rating = "Rating",
rating_change = "Î Rating", record = "W-L",
avg_pf = "Avg PF", avg_pa = "Avg PA") |>
gt::fmt_number(columns = "Rating", decimals = 0) |>
gt::fmt_number(columns = "rating_change", decimals = 1,
force_sign = TRUE) |>
gt::fmt_number(columns = c("avg_pf", "avg_pa"), decimals = 2) |>
gt::sub_missing(columns = "rating_change", missing_text = "â") |>
gt::data_color(columns = "Rating",
palette = c("#d73027", "#fee08b", "#1a9850")) |>
gt::tab_style(
style = gt::cell_text(color = "#1a9850", weight = "bold"),
locations = gt::cells_body(columns = "move",
rows = grepl("â²", tbl$move))
) |>
gt::tab_style(
style = gt::cell_text(color = "#d73027", weight = "bold"),
locations = gt::cells_body(columns = "move",
rows = grepl("â¼", tbl$move))
) |>
gt::tab_style(style = gt::cell_text(weight = "bold"),
locations = gt::cells_body(columns = c("rank", "team_name"))) |>
gt::tab_source_note(paste0("Generated ", format(Sys.Date(), "%B %d, %Y"),
" | Glicko-2 (PlayerRatings)")) |>
gt::tab_options(table.font.size = gt::px(14),
heading.title.font.size = gt::px(20),
data_row.padding = gt::px(6))
}

# --------------------------- MAIN ENTRY POINT --------------------------------

# league_id Sleeper league ID (string)
# league_tag Short label used in filenames, e.g. "ADG Redraft"
# season_label e.g. "2026-2027"
# base_dir Season folder; outputs created beneath it
# roster_scores Optional numeric vector by roster_id (higher = stronger
# preseason team); NULL -> all start at 1500
# owner_map Optional data.frame(user_id, owner) of canonical names;
# NULL -> Sleeper display names
# through_week Optional cap on the last completed week (NULL = auto-detect)
# make_graphic Set FALSE to skip PNG/PDF rendering
run_power_rankings <- function(league_id, league_tag, season_label, base_dir,
roster_scores = NULL, owner_map = NULL,
through_week = NULL, make_graphic = TRUE) {

weeks_dir <- file.path(base_dir, "Power Rankings", "Weeks 1-14")
graphic_dir <- file.path(base_dir, "Power Rankings", "Graphics")
pr_dir <- file.path(base_dir, "Power Rankings")

# ---- Fetch
league <- sleeper(paste0("league/", league_id))
state <- sleeper("state/nfl")
users <- sleeper(paste0("league/", league_id, "/users"))
rosters <- sleeper(paste0("league/", league_id, "/rosters"))

n_teams <- league$total_rosters
playoff_week_start <- league$settings$playoff_week_start
reg_season_weeks <- seq_len(playoff_week_start - 1)

# ---- Completed regular-season weeks
if (identical(state$season, league$season) &&
identical(state$season_type, "regular")) {
completed_weeks <- reg_season_weeks[reg_season_weeks < state$week]
} else if (state$season > league$season ||
identical(state$season_type, "post")) {
completed_weeks <- reg_season_weeks
} else {
completed_weeks <- integer(0)
}
if (!is.null(through_week)) {
completed_weeks <- completed_weeks[completed_weeks <= through_week]
}
if (!length(completed_weeks)) {
stop("No completed regular-season weeks for league ", league_id,
" (season ", league$season, ", NFL state: ", state$season_type,
" week ", state$week, "). Nothing to compute.")
}

teams <- build_team_table(users, rosters, owner_map)
if (nrow(teams) != n_teams) {
stop("Expected ", n_teams, " rosters, got ", nrow(teams))
}
id_to_owner <- setNames(teams$owner, teams$roster_id)

# ---- Matchups
games <- build_matchups(league_id, completed_weeks)
latest_week <- max(games$week)
next_week <- latest_week + 1

for (d in c(base_dir, pr_dir, weeks_dir, graphic_dir)) {
if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}
matchups_file <- file.path(base_dir, paste0("matchups ", league$season, " ",
league_tag, ".csv"))
write.csv(games, matchups_file, row.names = FALSE)
message("Matchups through week ", latest_week, " written: ", matchups_file)

# ---- Glicko-2
roster_ids <- sort(teams$roster_id)
if (is.null(roster_scores)) {
init_rating <- rep(1500, length(roster_ids))
} else {
stopifnot(length(roster_scores) == length(roster_ids))
init_rating <- as.numeric(1500 + scale(roster_scores) * 100)
}
initial_ratings <- data.frame(Player = roster_ids, Rating = init_rating,
Deviation = 200, Volatility = 0.06)
vol_df <- calculate_volatility(games, latest_week)
initial_ratings$Volatility <-
vol_df$volatility[match(initial_ratings$Player, vol_df$ID)]
initial_ratings$Volatility[is.na(initial_ratings$Volatility)] <- 0.06

glicko2_data <- data.frame(date = games$week, player1 = games$team,
player2 = games$opponent, result = games$result)
g2 <- PlayerRatings::glicko2(glicko2_data, status = initial_ratings)
rankings <- as.data.frame(g2$ratings)
rankings$roster_id <- as.integer(as.character(rankings$Player))
rankings$Player <- id_to_owner[as.character(rankings$roster_id)]

# ---- Points against / points for
points_against_df <- create_points_against_table(games)
points_against_df$Team <- id_to_owner[as.character(points_against_df$Team)]
write.csv(points_against_df,
file.path(pr_dir, paste0(league_tag, " ", season_label,
" Points Against.csv")),
row.names = FALSE)

long_all <- do.call(rbind, lapply(unique(games$week),
function(w) transform_games_df(games, w)))
pf <- aggregate(Points ~ ID, long_all, mean)
names(pf) <- c("roster_id", "avg_pf")

# ---- Weekly rankings CSV (same format as historical files)
csv_out <- rankings[order(rankings$roster_id),
c("Player", "Rating", "Deviation", "Volatility",
"Games", "Win", "Draw", "Loss", "Lag")]
weekly_file <- file.path(weeks_dir,
paste0(season_label, " ", league_tag, " Week ",
next_week, " Power Rankings.csv"))
write.csv(csv_out, weekly_file, row.names = FALSE)
message("Rankings written: ", weekly_file)

# ---- Rank movement vs previous week
prev_file <- file.path(weeks_dir,
paste0(season_label, " ", league_tag, " Week ",
latest_week, " Power Rankings.csv"))
tbl <- rankings[order(-rankings$Rating), ]
tbl$rank <- seq_len(nrow(tbl))
if (file.exists(prev_file)) {
prev <- read.csv(prev_file, stringsAsFactors = FALSE)
prev <- prev[order(-prev$Rating), ]
prev$prev_rank <- seq_len(nrow(prev))
tbl$prev_rank <- prev$prev_rank[match(tbl$Player, prev$Player)]
tbl$prev_rating <- prev$Rating[match(tbl$Player, prev$Player)]
tbl$rank_change <- tbl$prev_rank - tbl$rank
tbl$rating_change <- tbl$Rating - tbl$prev_rating
} else {
tbl$rank_change <- NA_integer_
tbl$rating_change <- NA_real_
}

# ---- Assemble graphic data
tbl <- merge(tbl, teams, by = "roster_id", all.x = TRUE, sort = FALSE)
tbl <- merge(tbl, pf, by = "roster_id", all.x = TRUE, sort = FALSE)
tbl <- merge(tbl, setNames(points_against_df[, c("Team", "avg_pa")],
c("Player", "avg_pa")),
by = "Player", all.x = TRUE, sort = FALSE)
tbl <- tbl[order(tbl$rank), ]
tbl$record <- paste0(tbl$Win, "-", tbl$Loss,
ifelse(tbl$Draw > 0, paste0("-", tbl$Draw), ""))
tbl$move <- ifelse(is.na(tbl$rank_change) | tbl$rank_change == 0, "â",
ifelse(tbl$rank_change > 0,
paste0("â² ", tbl$rank_change),
paste0("â¼ ", abs(tbl$rank_change))))
tbl$avatar_url[is.na(tbl$avatar_url)] <-
"https://sleepercdn.com/images/v2/icons/league/league_avatar_mint.png"

png_file <- file.path(graphic_dir,
paste0(season_label, " ", league_tag, " Week ",
next_week, " Power Rankings.png"))
if (make_graphic) {
pr_table <- build_graphic(tbl, league$name, next_week, latest_week)
pdf_file <- sub("\\.png$", ".pdf", png_file)
save_ok <- tryCatch({
gt::gtsave(pr_table, png_file, expand = 10)
gt::gtsave(pr_table, pdf_file, expand = 10)
TRUE
}, error = function(e) {
warning("Graphic export failed (chromote requires Chrome/Edge): ",
conditionMessage(e), "\nSaving HTML fallback instead.")
gt::gtsave(pr_table, sub("\\.png$", ".html", png_file))
FALSE
})
message(if (save_ok) paste0("Graphic saved: ", png_file, " (+ PDF)")
else "HTML fallback saved; install Chrome for PNG/PDF export.")
}

message("Done. ", league_tag, " Week ", next_week,
" power rankings complete.")
invisible(list(rankings = tbl, games = games,
points_against = points_against_df,
weekly_file = weekly_file, png_file = png_file))
}

# --------------------------- LEAGUE HISTORY ----------------------------------
# Walks previous_league_id back through every season. Aggregates by Sleeper
# user_id so records follow owners across seasons and roster reshuffles.
# Records and head-to-head use regular-season games only.

fetch_league_chain <- function(league_id) {
chain <- list()
id <- league_id
while (!is.null(id) && nzchar(id) && !identical(id, "0")) {
lg <- tryCatch(sleeper(paste0("league/", id)), error = function(e) NULL)
if (is.null(lg)) break
chain[[length(chain) + 1]] <- lg
id <- lg$previous_league_id
}
chain
}

# Champion = winner of the playoff-bracket match that decides 1st place (p == 1)
season_champion_roster <- function(lg) {
wb <- tryCatch(sleeper(paste0("league/", lg$league_id, "/winners_bracket")),
error = function(e) NULL)
if (is.null(wb) || !is.data.frame(wb) || !all(c("p", "w") %in% names(wb))) {
return(NA_integer_)
}
fin <- wb[!is.na(wb$p) & wb$p == 1 & !is.na(wb$w), , drop = FALSE]
if (!nrow(fin)) return(NA_integer_)
as.integer(fin$w[1])
}

# history_seed: optional list(champions = data.frame(Season, Champion),
# records = data.frame(Owner, Seasons, W, L)) for pre-Sleeper seasons; merged
# into champions and all-time totals. H2H remains API-era only (no game-level
# data exists for seeded seasons).
fetch_league_history <- function(league_id, owner_map = NULL,
history_seed = NULL) {
chain <- fetch_league_chain(league_id)
long_all <- list(); champ_rows <- list(); season_owners <- list()

for (lg in chain) {
users <- tryCatch(sleeper(paste0("league/", lg$league_id, "/users")),
error = function(e) NULL)
rosters <- tryCatch(sleeper(paste0("league/", lg$league_id, "/rosters")),
error = function(e) NULL)
if (is.null(users) || is.null(rosters)) next
teams <- suppressWarnings(build_team_table(users, rosters, owner_map))
id_to_owner <- setNames(teams$owner, teams$roster_id)

champ <- season_champion_roster(lg)
if (!is.na(champ)) {
champ_rows[[length(champ_rows) + 1]] <- data.frame(
Season = lg$season,
Champion = unname(id_to_owner[as.character(champ)]),
stringsAsFactors = FALSE)
}

reg_weeks <- seq_len(lg$settings$playoff_week_start - 1)
games <- tryCatch(build_matchups(lg$league_id, reg_weeks),
error = function(e) NULL)
if (is.null(games)) next

season_owners[[length(season_owners) + 1]] <- data.frame(
season = lg$season, owner = unname(id_to_owner),
stringsAsFactors = FALSE)

long_all[[length(long_all) + 1]] <- data.frame(
owner = unname(id_to_owner[as.character(c(games$team,
games$opponent))]),
opponent = unname(id_to_owner[as.character(c(games$opponent,
games$team))]),
win = c(games$result, 1 - games$result),
stringsAsFactors = FALSE)
}

champs_df <- if (length(champ_rows)) do.call(rbind, champ_rows) else
data.frame(Season = character(0), Champion = character(0))
if (!is.null(history_seed) && !is.null(history_seed$champions)) {
champs_df <- rbind(champs_df,
history_seed$champions[, c("Season", "Champion")])
}
if (nrow(champs_df)) {
champs_df <- champs_df[order(champs_df$Season, decreasing = TRUE), ]
}

finalize_totals <- function(totals, champs_df) {
totals$`Win %` <- round((totals$W + 0.5 * totals$T) /
pmax(totals$W + totals$L + totals$T, 1), 3)
titles <- table(champs_df$Champion)
totals$Titles <- as.integer(ifelse(is.na(titles[totals$Owner]), 0,
titles[totals$Owner]))
totals <- totals[order(-totals$`Win %`, -totals$W), ]
if (all(totals$T == 0)) totals$T <- NULL
totals
}

if (!length(long_all)) {
totals <- NULL
if (!is.null(history_seed) && !is.null(history_seed$records)) {
totals <- history_seed$records
totals$T <- 0
totals <- finalize_totals(totals, champs_df)
}
return(list(champions = champs_df, totals = totals, h2h = NULL,
long = NULL))
}
long <- do.call(rbind, long_all)
owners <- sort(unique(long$owner))

# ---- All-time totals
so <- unique(do.call(rbind, season_owners))
totals <- data.frame(
Owner = owners,
Seasons = as.integer(table(so$owner)[owners]),
W = as.integer(tapply(long$win == 1, long$owner, sum)[owners]),
L = as.integer(tapply(long$win == 0, long$owner, sum)[owners]),
T = as.integer(tapply(long$win == 0.5, long$owner, sum)[owners]),
stringsAsFactors = FALSE, check.names = FALSE)

# Merge pre-API seed records (outer join: owners may exist in either era)
if (!is.null(history_seed) && !is.null(history_seed$records)) {
m <- merge(totals, history_seed$records[, c("Owner", "Seasons", "W", "L")],
by = "Owner", all = TRUE, suffixes = c("", "_seed"))
for (cl in c("Seasons", "W", "L", "T", "Seasons_seed", "W_seed",
"L_seed")) {
m[[cl]][is.na(m[[cl]])] <- 0
}
m$Seasons <- m$Seasons + m$Seasons_seed
m$W <- m$W + m$W_seed
m$L <- m$L + m$L_seed
totals <- m[, c("Owner", "Seasons", "W", "L", "T")]
}
totals <- finalize_totals(totals, champs_df)

# ---- Head-to-head matrix (W-L from the row owner's perspective)
h2h <- matrix("â", length(owners), length(owners),
dimnames = list(owners, owners))
for (i in owners) {
for (j in owners) {
if (i == j) next
sub <- long[long$owner == i & long$opponent == j, , drop = FALSE]
if (!nrow(sub)) next
ties <- sum(sub$win == 0.5)
h2h[i, j] <- paste0(sum(sub$win == 1), "-", sum(sub$win == 0),
if (ties > 0) paste0("-", ties) else "")
}
}
h2h_df <- data.frame(h2h, check.names = FALSE)

list(champions = champs_df, totals = totals, h2h = h2h_df, long = long)
}
