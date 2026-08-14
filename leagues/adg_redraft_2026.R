# =============================================================================
# League config: ADG Redraft, 2026-2027 season
# Returns a config list consumed by run_all.R and app.R.
# To add another league, copy this file and edit the values.
# =============================================================================

list(
    league_id    = "1385392905396453376",
    league_tag   = "ADG Redraft",
    season_label = "2026-2027",
    data_dir     = "data/adg_redraft",

    # Preseason/roster strength scoring, used for (a) Glicko-2 initial ratings
    # and (b) the ROST SCORE column in League Stats > Summary.
    #   "fantasycalc" - computed live each run: sums FantasyCalc player values
    #                   (see is_dynasty/ppr below) per Sleeper roster, then
    #                   ranks teams 1-16 (1 = strongest roster).
    #   "manual"      - uses the roster_scores vector below instead.
    #   NULL          - disabled; all teams start at 1500, ROST SCORE hidden.
    roster_score_source = "fantasycalc",
    is_dynasty = FALSE,  # FALSE = redraft player values, TRUE = dynasty
    ppr        = 1,      # matches this league's full-PPR scoring

    # Manual override, only used when roster_score_source == "manual".
    # By roster_id (higher = stronger); e.g. reversed Borda ranks.
    # 2025 values for reference: c(16,5,8,7,13,12,9,4,15,11,1,10,2,3,14,6)
    roster_scores = NULL,

    # Canonical owner names keyed by Sleeper user_id (stable across seasons).
    # Set to NULL to use Sleeper display names.
    owner_map = data.frame(
          user_id = c("940759302413967360", "967474399098077184",
                                      "940813195177062400", "941159594544443392",
                                      "940763315846889472", "970049231559954432",
                                      "970084067305750528", "940794771403046912",
                                      "851975252232732672", "940792224906813440",
                                      "983872106012184576", "940811524497707008",
                                      "983524250025799680", "983812076013502464",
                                      "1124108971461148672", "1135467195745964032",
                "972712009852665856", "980561693363228672"),
          owner   = c("KING COMMISH", "Gus Buck", "Kellen McHugh", "Elijah Sartin",
                                      "Ethan Thimme", "Neal Adams", "Jack Huff", "Tommy Pack",
                                      "Gabe Lewitt", "Colin Kelly", "Will Allemang", "Ethan Purdy",
                                      "Bryan Birk", "Connor Helgeson", "Jon Michael Gaudin",
                                      "Caleb Garcia", "Brady Hopkins", "Logan Smith"),
    stringsAsFactors = FALSE
  ),

  # Pre-Sleeper era (2020-2022): not available from the API. Merged into the
  # League history tab by fetch_league_history(). Owner names must match the
  # canonical names above ("Jayson Stancil" is recorded as KING COMMISH).
  # Note: W/L below are as provided (wins sum 344, losses 349 - source data
  # may include playoffs); edit here to correct.
  history_seed = list(
    champions = data.frame(
      Season   = c("2022", "2021", "2020"),
      Champion = c("Elijah Sartin", "Connor Helgeson", "KING COMMISH"),
      stringsAsFactors = FALSE
    ),
    records = data.frame(
      Owner = c("Tommy Pack", "KING COMMISH", "Johnny Hackett",
                "Ethan Thimme", "Chris Gansen", "Will Lewis",
                "Elijah Sartin", "Will Allemang", "Neal Adams",
                "Bryan Birk", "Connor Helgeson", "Logan Smith",
                "Jack Huff", "Axel Halvarson", "Kellen McHugh",
                "Colin Kelly", "Brady Hopkins", "Ethan Purdy",
                "Jacob Doherty", "Jordan Lacy", "Gus Buck",
                "Hunter Henderson", "Mark Belvoix"),
      Seasons = c(3, 3, 1, 2, 2, 2, 2, 1, 3, 3, 2, 1,
                  1, 2, 2, 3, 1, 3, 1, 2, 2, 2, 1),
      W = c(29, 36, 8, 18, 17, 13, 15, 6, 24, 26, 11, 8,
            5, 12, 16, 22, 8, 23, 5, 18, 12, 9, 3),
      L = c(11, 16, 4, 9, 10, 12, 12, 6, 28, 25, 16, 5,
            8, 13, 24, 18, 5, 29, 7, 33, 28, 18, 12),
      stringsAsFactors = FALSE
    )
  )
)
