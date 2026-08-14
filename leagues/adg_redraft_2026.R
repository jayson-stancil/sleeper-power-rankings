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

    # Preseason scores by roster_id (higher = stronger); fill in after the
    # 2026 draft, e.g. reversed Borda ranks. NULL -> all teams start at 1500.
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
        )
  )
