# ============================================================
# File:    Dmen_ZoneEntriesAgainst_Z.R
# Author:  Ian Denning
# Project: CAA Hockey 2026 Summer Cap Project (Z-Scores & Regression Model)
#
# Purpose: Take the player's individual statistics for their Zone Entries
#          Against (defending attackers), and calculate their z-scores, and
#          create a rank-based percentile system for each zone entry metric. 
#          Build a "Zone Entries Against" composite z-score, and export final
#          data set to CSV. This file is for Defenceman. 
# ============================================================
library(dplyr)

# Isolate columns I want a Z-Score in (Targets_per60 through Chances_Allowed_per60)
metric_cols <- names(Dmen_ZoneEntriesAgainst)[13:19]

# Convert Original data into new data frame (Keep OG data intact)
df <- Dmen_ZoneEntriesAgainst

# Build a new joined data frame - keep all original columns intact first
# Keep columns A through L as-is (tallied stats, pre Per 60 math)
result <- df[, 1:12]

# Z-Score Loop - Scale() function doing math (x - mean(x)) / sd(x)
for (col in metric_cols) {
  result[[col]] <- df[[col]]
  result[[paste0(col, "_zscore")]] <- as.numeric(scale(df[[col]]))
}

Dmen_ZoneEntriesAgainst_z <- result # Create new data frame with Z-Scores

# Create rank-based percentile column // add right after Z-Score column 
zscore_cols <- names(Dmen_ZoneEntriesAgainst_z)[grepl("_zscore", names(Dmen_ZoneEntriesAgainst_z))]

# Creating rank-based percentile system
# rank() assigns each player a rank from 1 (lowest z-score) to N (highest)
# Ties get averaged ranks automatically
# nrow() - Defenceman, n = 218 - if Dman has 180th highest Z-Score
# 180/218 * 100 = 83rd percentile 
for (col in zscore_cols) {
  perc_col <- sub("_zscore", "_percentile", col)
  idx <- which(names(Dmen_ZoneEntriesAgainst_z) == col)
  
  left  <- Dmen_ZoneEntriesAgainst_z[, 1:idx, drop = FALSE]
  middle <- setNames(data.frame(rank(Dmen_ZoneEntriesAgainst_z[[col]]) / nrow(Dmen_ZoneEntriesAgainst_z) * 100), perc_col)
  
  if (idx < ncol(Dmen_ZoneEntriesAgainst_z)) {
    right <- Dmen_ZoneEntriesAgainst_z[, (idx+1):ncol(Dmen_ZoneEntriesAgainst_z), drop = FALSE]
    Dmen_ZoneEntriesAgainst_z <- cbind(left, middle, right)
  } else {
    Dmen_ZoneEntriesAgainst_z <- cbind(left, middle)
  }
}

# Creating Zone Entries Against - Composite z-score

# All four metrics are negative stats (higher = worse) — signs NOT flipped
# A high composite score = this player gets beaten more often
# A low composite score = this player defends zone entries well
# Dmen Zone Entries Against - Formula: 
# Entry_Passes_Allowed_per60 + Carries_wChance_Allowed_per60 +
# Dump_wChance_Allowed_per60 + Chances_Allowed_per60 / 4

composite <- rowMeans(
  data.frame(
    EntryPassAllowed  = Dmen_ZoneEntriesAgainst_z[, "Entry_Passes_Allowed_per60_zscore"],
    CarryChanceAllowed = Dmen_ZoneEntriesAgainst_z[, "Carries_wChance_Allowed_per60_zscore"],
    DumpChanceAllowed = Dmen_ZoneEntriesAgainst_z[, "Dump_wChance_Allowed_per60_zscore"],
    ChancesAllowed    = Dmen_ZoneEntriesAgainst_z[, "Chances_Allowed_per60_zscore"]
  ),
  na.rm = TRUE
)

# Insert composite column between Column F (col 6) and Column G (col 7)
Dmen_ZoneEntriesAgainst_z <- cbind(
  Dmen_ZoneEntriesAgainst_z[, 1:6],
  Composite_Zscore = composite,
  Dmen_ZoneEntriesAgainst_z[, 7:ncol(Dmen_ZoneEntriesAgainst_z)]
)

# Rank-based percentile for Composite Zscore
Dmen_ZoneEntriesAgainst_z$Composite_Percentile <-
  rank(Dmen_ZoneEntriesAgainst_z$Composite_Zscore) / nrow(Dmen_ZoneEntriesAgainst_z) * 100

# Move percentile column to sit right after composite zscore (col 7)
Dmen_ZoneEntriesAgainst_z <- Dmen_ZoneEntriesAgainst_z[, c(
  names(Dmen_ZoneEntriesAgainst_z)[1:7],
  "Composite_Percentile",
  names(Dmen_ZoneEntriesAgainst_z)[8:(ncol(Dmen_ZoneEntriesAgainst_z)-1)]
)]

# Round all zscore, percentile, and composite columns to 2 decimal places
Dmen_ZoneEntriesAgainst_z <- Dmen_ZoneEntriesAgainst_z %>%
  mutate(across(c(contains("_zscore"), contains("_percentile"),
                  contains("Composite")), ~ round(., 2)))

# Export
write.csv(Dmen_ZoneEntriesAgainst_z, "Dmen_ZoneEntriesAgainst_Z.csv", row.names = FALSE)