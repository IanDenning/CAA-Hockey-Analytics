library(dplyr)

Forwards_Regression <- Forwards_EntriesFor %>%
  select(Player, Cap_Hit, Agent,
         Entries_per60, Carries_per60, Failed_Entries_per60,
         Entries_wPassingPlay_per60, Recoveries_per60, Carries_wChances_per60,
         Dumps_wChances_per60, Entries_wChances_per60, Pressures_per60,
         Carry_pct, Pass_pct, CarryChance_pct) %>%
  
  left_join(
    Forwards_iRates %>%
      select(Player, GP, TOI, TOI_perGP,
             Goals_per60, Total_Assists_per60, First_Assists_per60,
             Second_Assists_per60, Total_Points_per60, IPP,
             Shots_per60, Shooting_pct, ixG_per60, iCF_per60, iFF_per60,
             iSCF_per60, iHDCF_per60, Rush_Attempts_per60, Rebounds_Created_per60,
             PIM_per60, Total_Penalties_per60, Minor_per60, Major_per60,
             Misconduct_per60, Penalties_Drawn_per60, Giveaways_per60,
             Takeaways_per60, Hits_per60, Hits_Taken_per60, Shots_Blocked_per60,
             Faceoffs_Won_per60, Faceoffs_Lost_per60),
    by = "Player") %>%
  
  left_join(
    Forwards_onIce %>%
      select(Player,
             CF_per60, CA_per60, CF_pct, FF_per60, FA_per60, FF_pct,
             SF_per60, SA_per60, SF_pct, GF_per60, GA_per60, GF_pct,
             xGF_per60, xGA_per60, xGF_pct, SCF_per60, SCA_per60, SCF_pct,
             HDCF_per60, HDCA_per60, HDCF_pct, HDGF_per60, HDGA_per60, HDGF_pct,
             MDCF_per60, MDCA_per60, MDCF_pct, MDGF_per60, MDGA_per60, MDGF_pct,
             LDCF_per60, LDCA_per60, LDCF_pct, LDGF_per60, LDGA_per60, LDGF_pct,
             OnIce_SH_pct, OnIce_SV_pct, PDO,
             Off_ZoneStarts_per60, Neu_ZoneStarts_per60, Def_ZoneStarts_per60,
             On_theFly_ZoneStarts_per60, Off_ZoneStart_pct,
             Off_ZoneFaceoffs_per60, Neu_ZoneFaceoffs_per60,
             Def_ZoneFaceoffs_per60, Off_ZoneFaceoffs_pct),
    by = "Player")

# Export to CSV
write.csv(Forwards_Regression, "Forwards_Regression.csv", row.names = FALSE)
