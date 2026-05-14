# ============================================================
# File:    Dmen_ZoneEntriesFor_Z.R
# Author:  Ian Denning
# Project: CAA Hockey 2026 Summer Cap Project (Z-Scores & Regression Model)
#
# Purpose: Take the player's individual statistics for their offensive zone
#          entries, and calculate their z-scores and create a rank-based
#          percentile system for each zone entry metric. Build a "Zone Entry For"
#          composite z-score, and export final data set to CSV. 
#          This file is for Defenceman. 
# ============================================================
library(dplyr)

# Isolate columns I want a Z-Score in (Entries_per60 through Pressures_per60)
metric_cols <- names(Dmen_Zone_Entries)[15:23]

# Convert Original data into new data frame (Keep OG data intact)
df <- Dmen_Zone_Entries

# Build a new joined data frame - keep all original columns intact first
# Keep columns A through N as-is (tallied stats, pre Per 60 math)
result <- df[, 1:14]  

# Z-Score Loop - Scale() function doing math (x - mean(x)) / sd(x)
for (col in metric_cols) {
  result[[col]] <- df[[col]]
  result[[paste0(col, "_zscore")]] <- as.numeric(scale(df[[col]]))
}

Dmen_Zone_Entries_z <- result # Create new data frame with Z-Scores

# Create rank-based percentile column // add right after Z-Score column 
zscore_cols <- names(Dmen_Zone_Entries_z)[grepl("_zscore", names(Dmen_Zone_Entries_z))]

# Creating rank-based percentile system
# rank() assigns each player a rank from 1 (lowest z-score) to N (highest)
# Ties get averaged ranks automatically
# nrow() - Defenceman, n = 218 - if Dman has 180th highest Z-Score
# 180/218 * 100 = 83rd percentile 
for (col in zscore_cols) {
  perc_col <- sub("_zscore", "_percentile", col)
  idx <- which(names(Dmen_Zone_Entries_z) == col)
  
  left  <- Dmen_Zone_Entries_z[, 1:idx, drop = FALSE]
  middle <- setNames(data.frame(rank(Dmen_Zone_Entries_z[[col]]) / nrow(Dmen_Zone_Entries_z) * 100), perc_col)
  
  if (idx < ncol(Dmen_Zone_Entries_z)) {
    right <- Dmen_Zone_Entries_z[, (idx+1):ncol(Dmen_Zone_Entries_z), drop = FALSE]
    Dmen_Zone_Entries_z <- cbind(left, middle, right)
  } else {
    Dmen_Zone_Entries_z <- cbind(left, middle)
  }
}

# Creating Zone Entries For - Composite z-score
# Higher = Better (Positive Composite z-score with negative metric baked in)

# NOTE: Failed_Entries_per60 z-score is FLIPPED (multiplied by -1) so that
# more failed entries = lower composite score (penalized)
# fewer failed entries = higher composite score (rewarded)

# Dmen Zone Entries For - Formula: 
# Entries/60 + Carries/60 + (-Failed Entries/60) + Entries w/ Passing Play/60 +
# Carries w/Chances/60 + Dump-ins w/Chances/60 / 6 

positive_composite <- rowMeans(
  data.frame(
    Entries      = Dmen_Zone_Entries_z[, "Entries_per60_zscore"],
    Failed       = -Dmen_Zone_Entries_z[, "Failed_Entries_per60_zscore"],  # FLIPPED
    Carries      = Dmen_Zone_Entries_z[, "Carries_per60_zscore"],
    EntryPass    = Dmen_Zone_Entries_z[, "Entries_wPassingPlay_per60_zscore"],
    CarryChances = Dmen_Zone_Entries_z[, "Carries_wChances_per60_zscore"],
    DumpChances  = Dmen_Zone_Entries_z[, "Dumps_wChances_per60_zscore"]
  ),
  na.rm = TRUE
)

# Insert composite column between Column F (col 6) and Column G (col 7)
Dmen_Zone_Entries_z <- cbind(
  Dmen_Zone_Entries_z[, 1:6],
  Positive_Composite_Zscore = positive_composite,
  Dmen_Zone_Entries_z[, 7:ncol(Dmen_Zone_Entries_z)]
)

# Rank-based percentile for Positive Composite z-score
Dmen_Zone_Entries_z$Positive_Composite_Percentile <-
  rank(Dmen_Zone_Entries_z$Positive_Composite_Zscore) / nrow(Dmen_Zone_Entries_z) * 100

# Move percentile column to sit right after composite z-score (col 7)
Dmen_Zone_Entries_z <- Dmen_Zone_Entries_z[, c(
  names(Dmen_Zone_Entries_z)[1:7],
  "Positive_Composite_Percentile",
  names(Dmen_Zone_Entries_z)[8:(ncol(Dmen_Zone_Entries_z)-1)]
)]

# Round all z-score, percentile, and composite columns to 2 decimal places
Dmen_Zone_Entries_z <- Dmen_Zone_Entries_z %>%
  mutate(across(c(contains("_zscore"), contains("_percentile"),
                  contains("Composite")), ~ round(., 2)))

# Export
write.csv(Dmen_Zone_Entries_z, "Dmen_Zone_Entries_z.csv", row.names = FALSE)
