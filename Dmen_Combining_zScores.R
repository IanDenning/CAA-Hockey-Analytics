library(dplyr)

# ==============================================================================
# DEFENSEMEN MASTER Z-SCORE SHEET
# CAA Hockey Analytics — May 2026
# Merges: Dmen_onIce_zScores, Dmen_iRates_zScores,
#         Dmen_ZoneEntriesFor_zScores, Dmen_ZoneEntriesAgainst_zScores,
#         Dmen_DZoneExits_zScores
# Join key: Player (consistent across all five files)
# Files already loaded in environment — no read.csv needed
# ==============================================================================


# ==============================================================================
# 1. onIce — rename composite cols, select all metrics
#    This is the base table: Player, Team, Position, GP/TOI live here only
# ==============================================================================

onIce_select <- Dmen_onIce_zScores %>%
  rename(
    Dmen_onIce_PositiveComp_zScore    = Positive_Composite_Zscore,
    Dmen_onIce_PositiveComp_Percentile = `Positive_Composite_Percentile (Higher = Better)`,
    Dmen_onIce_NegativeComp_zScore    = Negative_Composite_Zscore,
    Dmen_onIce_NegativeComp_Percentile = `Negative_Composite_Percentile (Higher = Worse)`
  ) %>%
  select(
    Player, Team, Position,
    Dmen_onIce_PositiveComp_zScore, Dmen_onIce_PositiveComp_Percentile,
    Dmen_onIce_NegativeComp_zScore, Dmen_onIce_NegativeComp_Percentile,
    GP, GP_zScore, GP_percentile,
    TOI, TOI_zScore, TOI_percentile,
    TOI_perGP, TOI_perGP_zScore, TOI_perGP_percentile,
    CF_per60, CF_per60_zScore, CF_per60_percentile,
    CA_per60, CA_per60_zScore, CA_per60_percentile,
    CF_pct,   CF_pct_zScore,   CF_pct_percentile,
    FF_per60, FF_per60_zScore, FF_per60_percentile,
    FA_per60, FA_per60_zScore, FA_per60_percentile,
    FF_pct,   FF_pct_zScore,   FF_pct_percentile,
    SF_per60, SF_per60_zScore, SF_per60_percentile,
    SA_per60, SA_per60_zScore, SA_per60_percentile,
    SF_pct,   SF_pct_zScore,   SF_pct_percentile,
    GF_per60, GF_per60_zScore, GF_per60_percentile,
    GA_per60, GA_per60_zScore, GA_per60_percentile,
    GF_pct,   GF_pct_zScore,   GF_pct_percentile,
    xGF_per60, xGF_per60_zScore, xGF_per60_percentile,
    xGA_per60, xGA_per60_zScore, xGA_per60_percentile,
    xGF_pct,   xGF_pct_zScore,   xGF_pct_percentile,
    SCF_per60, SCF_per60_zScore, SCF_per60_percentile,
    SCA_per60, SCA_per60_zScore, SCA_per60_percentile,
    SCF_pct,   SCF_pct_zScore,   SCF_pct_percentile,
    HDCF_per60, HDCF_per60_zScore, HDCF_per60_percentile,
    HDCA_per60, HDCA_per60_zScore, HDCA_per60_percentile,
    HDCF_pct,   HDCF_pct_zScore,   HDCF_pct_percentile,
    HDGF_per60, HDGF_per60_zScore, HDGF_per60_percentile,
    HDGA_per60, HDGA_per60_zScore, HDGA_per60_percentile,
    HDGF_pct,   HDGF_pct_zScore,   HDGF_pct_percentile,
    MDCF_per60, MDCF_per60_zScore, MDCF_per60_percentile,
    MDCA_per60, MDCA_per60_zScore, MDCA_per60_percentile,
    MDCF_pct,   MDCF_pct_zScore,   MDCF_pct_percentile,
    MDGF_per60, MDGF_per60_zScore, MDGF_per60_percentile,
    MDGA_per60, MDGA_per60_zScore, MDGA_per60_percentile,
    MDGF_pct,   MDGF_pct_zScore,   MDGF_pct_percentile,
    LDCF_per60, LDCF_per60_zScore, LDCF_per60_percentile,
    LDCA_per60, LDCA_per60_zScore, LDCA_per60_percentile,
    LDCF_pct,   LDCF_pct_zScore,   LDCF_pct_percentile,
    LDGF_per60, LDGF_per60_zScore, LDGF_per60_percentile,
    LDGA_per60, LDGA_per60_zScore, LDGA_per60_percentile,
    LDGF_pct,   LDGF_pct_zScore,   LDGF_pct_percentile,
    OnIce_SH_pct, OnIce_SH_pct_zScore, OnIce_SH_pct_percentile,
    OnIce_SV_pct, OnIce_SV_pct_zScore, OnIce_SV_pct_percentile,
    PDO, PDO_zScore, PDO_percentile,
    Off_ZoneStarts_per60, Off_ZoneStarts_per60_zScore, Off_ZoneStarts_per60_percentile,
    Neu_ZoneStarts_per60, Neu_ZoneStarts_per60_zScore, Neu_ZoneStarts_per60_percentile,
    Def_ZoneStarts_per60, Def_ZoneStarts_per60_zScore, Def_ZoneStarts_per60_percentile,
    On_theFly_ZoneStarts_per60, On_theFly_ZoneStarts_per60_zScore, On_theFly_ZoneStarts_per60_percentile,
    Off_ZoneStart_pct, Off_ZoneStart_pct_zScore, Off_ZoneStart_pct_percentile,
    Off_ZoneFaceoffs_per60, Off_ZoneFaceoffs_per60_zScore, Off_ZoneFaceoffs_per60_percentile,
    Neu_ZoneFaceoffs_per60, Neu_ZoneFaceoffs_per60_zScore, Neu_ZoneFaceoffs_per60_percentile,
    Def_ZoneFaceoffs_per60, Def_ZoneFaceoffs_per60_zScore, Def_ZoneFaceoffs_per60_percentile,
    Off_ZoneFaceoffs_pct, Off_ZoneFaceoffs_pct_zScore, Off_ZoneFaceoffs_pct_percentile
  )


# ==============================================================================
# 2. iRates — rename composite, fix IPP_zscore casing, drop duplicate GP/TOI
# ==============================================================================

iRates_select <- Dmen_iRates_zScores %>%
  rename(
    Dmen_iRatesComp_zScore    = Positive_Composite_Zscore,
    Dmen_iRatesComp_Percentile = `Positive_Composite_Percentile (Higher = Better)`,
    IPP_zScore                = IPP_zscore
  ) %>%
  select(
    Player,
    Dmen_iRatesComp_zScore, Dmen_iRatesComp_Percentile,
    iGoals_per60, iGoals_per60_zScore, iGoals_per60_percentile,
    iTotal_Assists_per60, iTotal_Assists_per60_zScore, iTotal_Assists_per60_percentile,
    iFirst_Assists_per60, iFirst_Assists_per60_zScore, iFirst_Assists_per60_percentile,
    iSecond_Assists_per60, iSecond_Assists_per60_zScore, iSecond_Assists_per60_percentile,
    iTotal_Points_per60, iTotal_Points_per60_zScore, iTotal_Points_per60_percentile,
    IPP, IPP_zScore, IPP_percentile,
    iShots_per60, iShots_per60_zScore, iShots_per60_percentile,
    iShooting_pct, iShooting_pct_zScore, iShooting_pct_percentile,
    ixG_per60, ixG_per60_zScore, ixG_per60_percentile,
    iCF_per60, iCF_per60_zScore, iCF_per60_percentile,
    iFF_per60, iFF_per60_zScore, iFF_per60_percentile,
    iSCF_per60, iSCF_per60_zScore, iSCF_per60_percentile,
    iHDCF_per60, iHDCF_per60_zScore, iHDCF_per60_percentile,
    iRush_Attempts_per60, iRush_Attempts_per60_zScore, iRush_Attempts_per60_percentile,
    iRebounds_Created_per60, iRebounds_Created_per60_zScore, iRebounds_Created_per60_percentile,
    iPIM_per60, iPIM_per60_zScore, iPIM_per60_percentile,
    iTotal_Penalties_per60, iTotal_Penalties_per60_zScore, iTotal_Penalties_per60_percentile,
    iMinor_per60, iMinor_per60_zScore, iMinor_per60_percentile,
    iMajor_per60, iMajor_per60_zScore, iMajor_per60_percentile,
    iMisconduct_per60, iMisconduct_per60_zScore, iMisconduct_per60_percentile,
    iPenalties_Drawn_per60, iPenalties_Drawn_per60_zScore, iPenalties_Drawn_per60_percentile,
    iGiveaways_per60, iGiveaways_per60_zScore, iGiveaways_per60_percentile,
    iTakeaways_per60, iTakeaways_per60_zScore, iTakeaways_per60_percentile,
    iHits_per60, iHits_per60_zScore, iHits_per60_percentile,
    iHits_Taken_per60, iHits_Taken_per60_zScore, iHits_Taken_per60_percentile,
    iShots_Blocked_per60, iShots_Blocked_per60_zScore, iShots_Blocked_per60_percentile
  )


# ==============================================================================
# 3. ZoneEntriesFor — rename composite, fix _zscore casing, drop extra cols
#    Extra cols dropped: Sum of 5v5 TOI, Cap_Hit, Agent, raw count columns
# ==============================================================================

entriesFor_select <- Dmen_ZoneEntriesFor_zScores %>%
  rename(
    Dmen_ZoneEntriesFor_Comp_zScore    = Positive_Composite_Zscore,
    Dmen_ZoneEntriesFor_Comp_Percentile = `Positive_Composite_Percentile (Higher = Better)`,
    Entries_per60_zScore               = Entries_per60_zscore,
    Carries_per60_zScore               = Carries_per60_zscore,
    Failed_Entries_per60_zScore        = Failed_Entries_per60_zscore,
    Entries_wPassingPlay_per60_zScore  = Entries_wPassingPlay_per60_zscore,
    Recoveries_per60_zScore            = Recoveries_per60_zscore,
    Carries_wChances_per60_zScore      = Carries_wChances_per60_zscore,
    Dumps_wChances_per60_zScore        = Dumps_wChances_per60_zscore,
    Entries_wChances_per60_zScore      = Entries_wChances_per60_zscore,
    Pressures_per60_zScore             = Pressures_per60_zscore
  ) %>%
  select(
    Player,
    Dmen_ZoneEntriesFor_Comp_zScore, Dmen_ZoneEntriesFor_Comp_Percentile,
    Entries_per60, Entries_per60_zScore, Entries_per60_percentile,
    Carries_per60, Carries_per60_zScore, Carries_per60_percentile,
    Failed_Entries_per60, Failed_Entries_per60_zScore, Failed_Entries_per60_percentile,
    Entries_wPassingPlay_per60, Entries_wPassingPlay_per60_zScore, Entries_wPassingPlay_per60_percentile,
    Recoveries_per60, Recoveries_per60_zScore, Recoveries_per60_percentile,
    Carries_wChances_per60, Carries_wChances_per60_zScore, Carries_wChances_per60_percentile,
    Dumps_wChances_per60, Dumps_wChances_per60_zScore, Dumps_wChances_per60_percentile,
    Entries_wChances_per60, Entries_wChances_per60_zScore, Entries_wChances_per60_percentile,
    Pressures_per60, Pressures_per60_zScore, Pressures_per60_percentile
  )


# ==============================================================================
# 4. ZoneEntriesAgainst — rename composite, fix _zscore casing, drop extra cols
#    Extra cols dropped: Sum of 5v5 TOI2, Cap_Hit, Agent, raw count columns
#    NOTE: Chances_Allowed_per60 has no _zscore/_percentile in source file —
#          raw value included only; add zScore/percentile if computed later
# ==============================================================================

entriesAgainst_select <- Dmen_ZoneEntriesAgainst_zScores %>%
  rename(
    Dmen_ZoneEntriesAgainst_Comp_zScore    = Composite_Zscore,
    Dmen_ZoneEntriesAgainst_Comp_Percentile = `Composite_Percentile (Higher = Worse)`,
    Targets_per60_zScore                   = Targets_per60_zscore,
    Carries_Against_per60_zScore           = Carries_Against_per60_zscore,
    Denials_per60_zScore                   = Denials_per60_zscore,
    `Denials_per60_percentile (Higher = Better)` = `Denials_per60_percentile (Higher = Better)`,
    Entry_Passes_Allowed_per60_zScore      = Entry_Passes_Allowed_per60_zscore,
    Carries_wChance_Allowed_per60_zScore   = Carries_wChance_Allowed_per60_zscore,
    Dump_wChance_Allowed_per60_zScore      = Dump_wChance_Allowed_per60_zscore,
    Chances_Allowed_per60_zScore           = Chances_Allowed_per60_zscore
  ) %>%
  select(
    Player,
    Dmen_ZoneEntriesAgainst_Comp_zScore, Dmen_ZoneEntriesAgainst_Comp_Percentile,
    Targets_per60, Targets_per60_zScore, Targets_per60_percentile,
    Carries_Against_per60, Carries_Against_per60_zScore, Carries_Against_per60_percentile,
    Denials_per60, Denials_per60_zScore, `Denials_per60_percentile (Higher = Better)`,
    Entry_Passes_Allowed_per60, Entry_Passes_Allowed_per60_zScore, Entry_Passes_Allowed_per60_percentile,
    Carries_wChance_Allowed_per60, Carries_wChance_Allowed_per60_zScore, Carries_wChance_Allowed_per60_percentile,
    Dump_wChance_Allowed_per60, Dump_wChance_Allowed_per60_zScore, Dump_wChance_Allowed_per60_percentile,
    Chances_Allowed_per60, Chances_Allowed_per60_zScore, Chances_Allowed_per60_percentile
  )


# ==============================================================================
# 5. DZoneExits — rename composite (fixing "Beter" typo), fix _zscore casing,
#    drop extra cols (Sum of 5v5 TOI, Cap_Hit, Agent, raw count columns)
# ==============================================================================

dZoneExits_select <- Dmen_DZoneExits_zScores %>%
  rename(
    Dmen_DZoneExits_Comp_zScore    = Composite_Zscore,
    Dmen_DZoneExits_Comp_Percentile = `Composite_Percentile (Higher = Beter)`,
    DZ_PuckTouches_per60_zScore            = DZ_PuckTouches_per60_zscore,
    Retrievals_per60_zScore                = Retrievals_per60_zscore,
    Botched_Retrievals_per60_zScore        = Botched_Retrievals_per60_zscore,
    Retrievals_leading_toExits_per60_zScore = Retrievals_leading_toExits_per60_zscore,
    Exits_per60_zScore                     = Exits_per60_zscore,
    Exits_wPossession_per60_zScore         = Exits_wPossession_per60_zscore,
    Clears_per60_zScore                    = Clears_per60_zscore,
    Failed_Exits_per60_zScore              = Failed_Exits_per60_zscore,
    Passed_Exits_per60_zScore              = Passed_Exits_per60_zscore,
    Carried_Exits_per60_zScore             = Carried_Exits_per60_zscore
  ) %>%
  select(
    Player,
    Dmen_DZoneExits_Comp_zScore, Dmen_DZoneExits_Comp_Percentile,
    DZ_PuckTouches_per60, DZ_PuckTouches_per60_zScore, DZ_PuckTouches_per60_percentile,
    Retrievals_per60, Retrievals_per60_zScore, Retrievals_per60_percentile,
    Botched_Retrievals_per60, Botched_Retrievals_per60_zScore, Botched_Retrievals_per60_percentile,
    Retrievals_leading_toExits_per60, Retrievals_leading_toExits_per60_zScore, Retrievals_leading_toExits_per60_percentile,
    Exits_per60, Exits_per60_zScore, Exits_per60_percentile,
    Exits_wPossession_per60, Exits_wPossession_per60_zScore, Exits_wPossession_per60_percentile,
    Clears_per60, Clears_per60_zScore, Clears_per60_percentile,
    Failed_Exits_per60, Failed_Exits_per60_zScore, Failed_Exits_per60_percentile,
    Passed_Exits_per60, Passed_Exits_per60_zScore, Passed_Exits_per60_percentile,
    Carried_Exits_per60, Carried_Exits_per60_zScore, Carried_Exits_per60_percentile
  )


# ==============================================================================
# 6. Merge all five on Player
#    onIce is base (Player, Team, Position, GP/TOI)
#    All subsequent joins bring Player as key only — no duplicate identity cols
# ==============================================================================

Dmen_zScores_Master <- onIce_select %>%
  left_join(iRates_select,        by = "Player") %>%
  left_join(entriesFor_select,    by = "Player") %>%
  left_join(entriesAgainst_select, by = "Player") %>%
  left_join(dZoneExits_select,    by = "Player")


# ==============================================================================
# 7. Export
# ==============================================================================

write.csv(Dmen_zScores_Master, "Dmen_zScores_Master.csv", row.names = FALSE)

cat("Dmen master sheet built successfully.\n")
cat("Rows:", nrow(Dmen_zScores_Master), "\n")
cat("Columns:", ncol(Dmen_zScores_Master), "\n")