# ============================================================
# File:    Dmen_DzoneExits_Z.R
# Author:  Ian Denning
# Project: CAA Hockey 2026 Summer Cap Project (Z-Scores & Regression Model)
#
# Purpose: Take the player's individual statistics for their Defensive Zone
#          Exits, and calculate their z-scores, and create a rank-based
#          percentile system for each zone entry metric.
#          Build a "Defensive Zone Exits" composite z-score, and export final 
#          data set to CSV. This file is for Defenceman. 
# ============================================================
library(dplyr)

# Isolate columns I want a Z-Score in (cols 21 through 30)
metric_cols <- names(DzoneExits_Dmen)[21:30]

# Convert Original data into new data frame (Keep OG data intact)
df <- DzoneExits_Dmen

# Build a new joined data frame - keep all original columns intact first
# Keep columns A through T as-is (tallied stats, pre Per 60 math)
result <- df[, 1:20]

# Z-Score Loop - Scale() function doing math (x - mean(x)) / sd(x)
for (col in metric_cols) {
  result[[col]] <- df[[col]]
  result[[paste0(col, "_zscore")]] <- as.numeric(scale(df[[col]]))
}

DzoneExits_Dmen_z <- result # Create new data frame with Z-Scores

# Create rank-based percentile column // add right after Z-Score column 
zscore_cols <- names(DzoneExits_Dmen_z)[grepl("_zscore", names(DzoneExits_Dmen_z))]

# Creating rank-based percentile system
# rank() assigns each player a rank from 1 (lowest z-score) to N (highest)
# Ties get averaged ranks automatically
# nrow() - Defenceman, n = 218 - if Dman has 180th highest Z-Score
# 180/218 * 100 = 83rd percentile 
for (col in zscore_cols) {
  perc_col <- sub("_zscore", "_percentile", col)
  idx <- which(names(DzoneExits_Dmen_z) == col)
  
  left  <- DzoneExits_Dmen_z[, 1:idx, drop = FALSE]
  middle <- setNames(data.frame(rank(DzoneExits_Dmen_z[[col]]) / nrow(DzoneExits_Dmen_z) * 100), perc_col)
  
  if (idx < ncol(DzoneExits_Dmen_z)) {
    right <- DzoneExits_Dmen_z[, (idx+1):ncol(DzoneExits_Dmen_z), drop = FALSE]
    DzoneExits_Dmen_z <- cbind(left, middle, right)
  } else {
    DzoneExits_Dmen_z <- cbind(left, middle)
  }
}

# Creating Zone Entries Against - Composite z-score
# D-Zone Exits is 1 composite z-score (with negative metrics baked in)
# Meaning: negative metrics like Failed Exits, a higher raw amount = worse
# Composite Calculation:
# Keep sign (+) of positive metrics 
# Flip sign (-) of negative metrics
# A player with a higher amount of Failed Exits/Botched retrievals will be
# penalized appropriately in their final composite z-score. 

# D-Zone Exits Formula: 
# Retrievals/60 + Retrievals_leading_toExits/60 + Exits/60 +
# Exits_wPossession/60 + Clears/60 + (-Botched_Retrievals/60) + (-Failed_Exits/60)

# Positive metrics (keep sign — higher = better)
# Negative metrics (flip sign — higher raw = worse, so we penalize)

composite <- rowMeans(
  data.frame(
    Retrievals        = DzoneExits_Dmen_z[, "Retrievals_per60_zscore"],
    RetrievalsToExits = DzoneExits_Dmen_z[, "Retrievals_leading_toExits_per60_zscore"],
    Exits             = DzoneExits_Dmen_z[, "Exits_per60_zscore"],
    ExitsPossession   = DzoneExits_Dmen_z[, "Exits_wPossession_per60_zscore"],
    Clears            = DzoneExits_Dmen_z[, "Clears_per60_zscore"],
    BotchedRetrievals = -DzoneExits_Dmen_z[, "Botched_Retrievals_per60_zscore"],  # FLIPPED
    FailedExits       = -DzoneExits_Dmen_z[, "Failed_Exits_per60_zscore"]          # FLIPPED
  ),
  na.rm = TRUE
)

# Insert composite column between Column F (col 6) and Column G (col 7)
DzoneExits_Dmen_z <- cbind(
  DzoneExits_Dmen_z[, 1:6], 
  Composite_Zscore = composite,
  DzoneExits_Dmen_z[, 7:ncol(DzoneExits_Dmen_z)]
)

# Rank-based percentile for Composite Zscore
DzoneExits_Dmen_z$Composite_Percentile <-
  rank(DzoneExits_Dmen_z$Composite_Zscore) / nrow(DzoneExits_Dmen_z) * 100

# Move percentile column to sit right after composite zscore (col 7)
DzoneExits_Dmen_z <- DzoneExits_Dmen_z[, c(
  names(DzoneExits_Dmen_z)[1:7],
  "Composite_Percentile",
  names(DzoneExits_Dmen_z)[8:(ncol(DzoneExits_Dmen_z)-1)]
)]

# Round all zscore, percentile, and composite columns to 2 decimal places
DzoneExits_Dmen_z <- DzoneExits_Dmen_z %>%
  mutate(across(c(contains("_zscore"), contains("_percentile"),
                  contains("Composite")), ~ round(., 2)))

# Export
write.csv(DzoneExits_Dmen_z, "Dmen_DzoneExits_Z.csv", row.names = FALSE)