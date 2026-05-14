library(dplyr)

# ==============================================================================
# FORWARDS MASTER Z-SCORE SHEET
# CAA Hockey Analytics — May 2026
# Merges: Forwards_onIce_zScores, Forwards_iRates_zScores,
#         Forwards_ZoneEntriesFor_zScores
# Join key: Player (consistent across all three files)
# ==============================================================================

# Files already loaded in environment — no read.csv needed

# ==============================================================================
# 1. onIce — select all requested columns (already correctly named)
# ==============================================================================

onIce_select <- Forwards_onIce_zScores %>%
  select(
    Player, Team, Position,
    F_onIce_Positive_Composite_zScore,
    `F_onIce_Positive_Composite_Percentile (Higher = Better)`,
    F_onIce_Negative_Composite_zScore,
    `F_onIce_Negative_Composite_Percentile (Higher = Worse)`,
    GP, GP_zScore, GP_percentile,
    TOI, TOI_zScore, TOI_percentile,
    TOI_perGP, TOI_perGP_zScore, TOI_perGP_percentile,
    CF_per60, CF_per60_zScore, CF_per60_percentile,
    CA_per60, CA_per60_zScore, CA_per60_percentile,
    CF_pct,  CF_pct_zScore,  CF_pct_percentile,
    FF_per60, FF_per60_zScore, FF_per60_percentile,
    FA_per60, FA_per60_zScore, FA_per60_percentile,
    FF_pct,  FF_pct_zScore,  FF_pct_percentile,
    SF_per60, SF_per60_zScore, SF_per60_percentile,
    SA_per60, SA_per60_zScore, SA_per60_percentile,
    SF_pct,  SF_pct_zScore,  SF_pct_percentile,
    GF_per60, GF_per60_zScore, GF_per60_percentile,
    GA_per60, GA_per60_zScore, GA_per60_percentile,
    GF_pct,  GF_pct_zScore,  GF_pct_percentile,
    xGF_per60, xGF_per60_zScore, xGF_per60_percentile,
    xGA_per60, xGA_per60_zScore, xGA_per60_percentile,
    xGF_pct,  xGF_pct_zScore,  xGF_pct_percentile,
    SCF_per60, SCF_per60_zScore, SCF_per60_percentile,
    SCA_per60, SCA_per60_zScore, SCA_per60_percentile,
    SCF_pct,  SCF_pct_zScore,  SCF_pct_percentile,
    HDCF_per60, HDCF_per60_zScore, HDCF_per60_percentile,
    HDCA_per60, HDCA_per60_zScore, HDCA_per60_percentile,
    HDCF_pct,  HDCF_pct_zScore,  HDCF_pct_percentile,
    HDGF_per60, HDGF_per60_zScore, HDGF_per60_percentile,
    HDGA_per60, HDGA_per60_zScore, HDGA_per60_percentile,
    HDGF_pct,  HDGF_pct_zScore,  HDGF_pct_percentile,
    MDCF_per60, MDCF_per60_zScore, MDCF_per60_percentile,
    MDCA_per60, MDCA_per60_zScore, MDCA_per60_percentile,
    MDCF_pct,  MDCF_pct_zScore,  MDCF_pct_percentile,
    MDGF_per60, MDGF_per60_zScore, MDGF_per60_percentile,
    MDGA_per60, MDGA_per60_zScore, MDGA_per60_percentile,
    MDGF_pct,  MDGF_pct_zScore,  MDGF_pct_percentile,
    LDCF_per60, LDCF_per60_zScore, LDCF_per60_percentile,
    LDCA_per60, LDCA_per60_zScore, LDCA_per60_percentile,
    LDCF_pct,  LDCF_pct_zScore,  LDCF_pct_percentile,
    LDGF_per60, LDGF_per60_zScore, LDGF_per60_percentile,
    LDGA_per60, LDGA_per60_zScore, LDGA_per60_percentile,
    LDGF_pct,  LDGF_pct_zScore,  LDGF_pct_percentile,
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
# 2. iRates — rename inconsistent casing, drop duplicate GP/TOI cols, select
# NOTE: IPP_zscore → IPP_zScore (lowercase z fix)
# NOTE: GP, TOI, TOI_perGP already in onIce — dropped here to avoid duplication
# ==============================================================================

iRates_select <- Forwards_iRates_zScores %>%
  rename(IPP_zScore = IPP_zscore) %>%
  select(
    Player,
    F_iRates_Positive_Composite_zScore,
    `F_iRates_Positive_Composite_Percentile (Higher = Better)`,
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
    iShots_Blocked_per60, iShots_Blocked_per60_zScore, iShots_Blocked_per60_percentile,
    iFaceoffs_Won_per60, iFaceoffs_Won_per60_zScore, iFaceoffs_Won_per60_percentile,
    iFaceoffs_Lost_per60, iFaceoffs_Lost_per60_zScore, iFaceoffs_Lost_per60_percentile
  )


# ==============================================================================
# 3. ZoneEntriesFor — rename _zscore → _zScore throughout, drop extra cols
# NOTE: raw count cols, Cap_Hit, Agent, Sum of 5v5 TOI are dropped
# NOTE: Carry_pct, Pass_pct, CarryChance_pct not present in this z-score file
# ==============================================================================

entries_select <- Forwards_ZoneEntriesFor_zScores %>%
  rename(
    Entries_per60_zScore              = Entries_per60_zscore,
    Carries_per60_zScore              = Carries_per60_zscore,
    Failed_Entries_per60_zScore       = Failed_Entries_per60_zscore,
    Entries_wPassingPlay_per60_zScore = Entries_wPassingPlay_per60_zscore,
    Recoveries_per60_zScore           = Recoveries_per60_zscore,
    Carries_wChances_per60_zScore     = Carries_wChances_per60_zscore,
    Dumps_wChances_per60_zScore       = Dumps_wChances_per60_zscore,
    Entries_wChances_per60_zScore     = Entries_wChances_per60_zscore,
    Pressures_per60_zScore            = Pressures_per60_zscore
  ) %>%
  select(
    Player,
    F_EntriesFor_Positive_Composite_zScore,
    `F_EntriesFor_Positive_Composite_Percentile (Higher = Better)`,
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
# 4. Merge all three on Player
# onIce is the base (has Player, Team, Position)
# iRates and ZoneEntriesFor join by Player only
# ==============================================================================

Forwards_zScores_Master <- onIce_select %>%
  left_join(iRates_select,  by = "Player") %>%
  left_join(entries_select, by = "Player")


# ==============================================================================
# 5. Export
# ==============================================================================

write.csv(Forwards_zScores_Master, "Forwards_zScores_Master.csv", row.names = FALSE)

cat("Master sheet built successfully.\n")
cat("Rows:", nrow(Forwards_zScores_Master), "\n")
cat("Columns:", ncol(Forwards_zScores_Master), "\n")