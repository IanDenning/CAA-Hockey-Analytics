# ============================================================
# File:    Forward_ZoneEntriesFor_Z.R
# Author:  Ian Denning
# Project: CAA Hockey 2026 Summer Cap Project (Z-Scores & Regression Model)
#
# Purpose: Take the player's individual statistics for their offensive zone
#          entries, and calculate their z-scores and create a rank-based
#          percentile system for each zone entry metric. Build a "Zone Entry For"
#          composite z-score, and export final data set to CSV. 
#          This file is for Forwards 
# ============================================================
library(dplyr)

# Isolate columns I want a Z-Score in (Entries_per60 through Pressures_per60)
metric_cols <- names(Forwards_Zone_Entries)[15:23]

# Convert Original data into new data frame (Keep OG data intact)
df <- Forwards_Zone_Entries

# Build a new joined data frame - keep all original columns intact first
# Keep columns A through N as-is (tallied stats, pre Per 60 math)
result <- df[, 1:14]

# Z-Score Loop - Scale() function doing math (x - mean(x)) / sd(x)
for (col in metric_cols) {
  result[[col]] <- df[[col]]
  result[[paste0(col, "_zscore")]] <- as.numeric(scale(df[[col]]))
}

Forwards_Zone_Entries_z <- result # Create new data frame with Z-Scores

# Create rank-based percentile column // add right after Z-Score column 
zscore_cols <- names(Forwards_Zone_Entries_z)[grepl("_zscore", names(Forwards_Zone_Entries_z))]

# Creating rank-based percentile system
# rank() assigns each player a rank from 1 (lowest z-score) to N (highest)
# Ties get averaged ranks automatically
# nrow() - Forwards, n = 404 - if Forward has 380th highest Z-Score
# 380/404 * 100 = 94th percentile 
for (col in zscore_cols) {
  perc_col <- sub("_zscore", "_percentile", col)
  idx <- which(names(Forwards_Zone_Entries_z) == col)
  
  left  <- Forwards_Zone_Entries_z[, 1:idx, drop = FALSE]
  middle <- setNames(data.frame(rank(Forwards_Zone_Entries_z[[col]]) / nrow(Forwards_Zone_Entries_z) * 100), perc_col)
  
  if (idx < ncol(Forwards_Zone_Entries_z)) {
    right <- Forwards_Zone_Entries_z[, (idx+1):ncol(Forwards_Zone_Entries_z), drop = FALSE]
    Forwards_Zone_Entries_z <- cbind(left, middle, right)
  } else {
    Forwards_Zone_Entries_z <- cbind(left, middle)
  }
}

# Creating Zone Entries For - Composite z-score
# Higher = Better (Positive Composite z-score with negative metric baked in)

# NOTE: Failed_Entries_per60 z-score is FLIPPED (multiplied by -1) so that
# more failed entries = lower composite score (penalized)
# fewer failed entries = higher composite score (rewarded)

# Forwards Zone Entries For - Formula: 
# Entries/60 + Carries/60 + (-Failed Entries/60) + Entries w/ Passing Play/60 +
# Recoveries/60 + Carries w/Chances/60 + Dumps w/Chances/60 + Pressures/60 / 9

positive_composite <- rowMeans(
  data.frame(
    Entries      = Forwards_Zone_Entries_z[, "Entries_per60_zscore"],
    Carries      = Forwards_Zone_Entries_z[, "Carries_per60_zscore"],
    Failed       = -Forwards_Zone_Entries_z[, "Failed_Entries_per60_zscore"],  # FLIPPED
    EntryPass    = Forwards_Zone_Entries_z[, "Entries_wPassingPlay_per60_zscore"],
    Recoveries   = Forwards_Zone_Entries_z[, "Recoveries_per60_zscore"],
    CarryChances = Forwards_Zone_Entries_z[, "Carries_wChances_per60_zscore"],
    DumpChances  = Forwards_Zone_Entries_z[, "Dumps_wChances_per60_zscore"],
    EntryChances = Forwards_Zone_Entries_z[, "Entries_wChances_per60_zscore"],
    Pressures    = Forwards_Zone_Entries_z[, "Pressures_per60_zscore"]
  ),
  na.rm = TRUE
)

# Insert composite column between Column F (col 6) and Column G (col 7)
Forwards_Zone_Entries_z <- cbind(
  Forwards_Zone_Entries_z[, 1:6],
  Positive_Composite_Zscore = positive_composite,
  Forwards_Zone_Entries_z[, 7:ncol(Forwards_Zone_Entries_z)]
)

# Rank-based percentile for Positive Composite Zscore
Forwards_Zone_Entries_z$Positive_Composite_Percentile <-
  rank(Forwards_Zone_Entries_z$Positive_Composite_Zscore) / nrow(Forwards_Zone_Entries_z) * 100

# Move percentile column to sit right after composite zscore (col 7)
Forwards_Zone_Entries_z <- Forwards_Zone_Entries_z[, c(
  names(Forwards_Zone_Entries_z)[1:7],
  "Positive_Composite_Percentile",
  names(Forwards_Zone_Entries_z)[8:(ncol(Forwards_Zone_Entries_z)-1)]
)]

# Round all zscore, percentile, and composite columns to 2 decimal places
Forwards_Zone_Entries_z <- Forwards_Zone_Entries_z %>%
  mutate(across(c(contains("_zscore"), contains("_percentile"),
                  contains("Composite")), ~ round(., 2)))

# Export
write.csv(Forwards_Zone_Entries_z, "Forwards_Zone_Entries_z.csv", row.names = FALSE)