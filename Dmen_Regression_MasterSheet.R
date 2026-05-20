library(dplyr)

Dmen_Regression <- Dmen_EntriesFor %>%
  select(Player, Cap_Hit, Agent,
         Entries_per60, Carries_per60, Failed_Entries_per60,
         Entries_wPassingPlay_per60, Recoveries_per60, Carries_wChances_per60,
         Dumps_wChances_per60, Entries_wChances_per60, Pressures_per60,
         Carry_pct, Pass_pct, CarryChance_pct) %>%
  
  left_join(
    Dmen_iRates %>%
      select(Player, GP, TOI, TOI_perGP,
             Goals_per60, Total_Assists_per60, First_Assists_per60,
             Second_Assists_per60, Total_Points_per60, IPP,
             Shots_per60, SH_pct, ixG_per60, iCF_per60, iFF_per60,
             iSCF_per60, iHDCF_per60, Rush_Attempts_per60, Rebounds_Created_per60,
             PIM_per60, Total_Penalties_per60, Minor_per60, Major_per60,
             Misconduct_per60, Penalties_Drawn_per60, Giveaways_per60,
             Takeaways_per60, Hits_per60, Hits_Taken_per60, Shots_Blocked_per60),
    by = "Player") %>%
  
  left_join(
    Dmen_onIce %>%
      select(Player,
             CF_per60, CA_per60, CF_pct, FF_per60, FA_per60, FF_pct,
             SF_per60, SA_per60, SF_pct, GF_per60, GA_per60, GF_pct,
             xGF_per60, xGA_per60, xGF_pct, SCF_per60, SCA_per60, SCF_pct,
             HDCF_per60, HDCA_per60, HDCF_pct, HDGF_per60, HDGA_per60, HDGF_pct,
             MDCF_per60, MDCA_per60, MDCF_pct, MDGF_per60, MDGA_per60, MDGF_pct,
             LDCF_per60, LDCA_per60, LDCF_pct, LDGF_per60, LDGA_per60, LDGF_pct,
             OnIce_SH_pct, OnIce_SV_pct, PDO,
             Off_Zone_Starts_per60, Neu_Zone_Starts_per60, Def_Zone_Starts_per60,
             On_The_Fly_Starts_per60, Off_Zone_Start_pct,
             Off_Zone_Faceoffs_per60, Neu_Zone_Faceoffs_per60,
             Def_Zone_Faceoffs_per60, Off_Zone_Faceoff_pct),
    by = "Player") %>%
  
  left_join(
    Dmen_ZoneEntriesAgainst %>%
      select(Player,
             Targets_per60, Carries_Against_per60, Denials_per60,
             Entry_Passes_Allowed_per60, Carries_wChance_Allowed_per60,
             Dump_wChance_Allowed_per60, Chances_Allowed_per60,
             Carry_Against_pct, Denial_pct, Entry_Passes_Allowed_pct,
             Chance_Against_pct),
    by = "Player") %>%
  
  left_join(
    DzoneExits_Dmen %>%
      select(Player,
             DZ_PuckTouches_per60, Retrievals_per60, Botched_Retrievals_per60,
             Retrievals_leading_toExits_per60, Exits_per60, Exits_wPossession_per60,
             Clears_per60, Failed_Exits_per60, Passed_Exits_per60, Carried_Exits_per60,
             Successful_Retrieval_pct, Successful_Exit_pct, Exits_wPossession_pct,
             Exits_off_Retrieval_pct, Failed_Exit_pct),
    by = "Player")

# Export to CSV
write.csv(Dmen_Regression, "Dmen_Regression.csv", row.names = FALSE)