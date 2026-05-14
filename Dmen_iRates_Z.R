# ============================================================
# File:    Dmen_iRates_zScores.R
# Author:  Ian Denning
# Project: CAA Hockey 2026 Summer Cap Project (Z-Scores & Regression Model)
#
# Purpose: Take the player's individual statistics while they are on the ice,
#          and calculate their z-scores and create a rank-based percentile
#          system for each metric. Build an iRate composite z-score, and 
#          export the final data set to CSV. This file is for the Defencemen group.
# ============================================================
library(dplyr)

# Isolate columns I want a Z-Score in (GP through Shots Blocked/60)
metric_cols <- names(Dmen_iRates)[4:ncol(Dmen_iRates)]

# Convert Original data into new data frame (Keep OG data intact)
df <- Dmen_iRates

# Build a new joined data frame
result <- df[, 1:3]  # Start with Player, Team, Position

# Z-Score Loop - Scale() function doing math (x - mean(x)) / sd(x)
for (col in metric_cols) {
  result[[col]] <- df[[col]]
  result[[paste0(col, "_zscore")]] <- as.numeric(scale(df[[col]]))
}

Dmen_iRates_z <- result # Create new data frame with Z-Scores

# Create rank-based percentile column // add right after Z-Score column 
zscore_cols <- names(Dmen_iRates_z)[grepl("_zscore", names(Dmen_iRates_z))]

# Creating rank-based percentile system
# rank() assigns each player a rank from 1 (lowest z-score) to N (highest)
# Ties get averaged ranks automatically
# nrow() - Defenceman, n = 218 - if Dman has 180th highest Z-Score
# 180/218 * 100 = 83rd percentile 
for (col in zscore_cols) {
  perc_col <- sub("_zscore", "_percentile", col)
  idx <- which(names(Dmen_iRates_z) == col)
  
  left  <- Dmen_iRates_z[, 1:idx, drop = FALSE]
  middle <- setNames(data.frame(rank(Dmen_iRates_z[[col]]) / nrow(Dmen_iRates_z) * 100), perc_col)
  
  if (idx < ncol(Dmen_iRates_z)) {
    right <- Dmen_iRates_z[, (idx+1):ncol(Dmen_iRates_z), drop = FALSE]
    Dmen_iRates_z <- cbind(left, middle, right)
  } else {
    Dmen_iRates_z <- cbind(left, middle)
  }
}

# Creating iRate Composite Z-Scores
# Identify most important metrics to quantify offensive value/production
# Take the average of those identified Z-Scores
# Higher = Better 

# Note: No Negative Composite Z-Score for iRates (Both Forwards and Dmen)
# Reasoning: Most of the stats tracked in iRates are positive individual 
# contributions, only penalties, giveaways, and faceoffs lost are tracked 
# negative metrics. Decided to make composite a positive individual-level score.

# Positive iRate Formula: 
# Total Assists/60 + ixG/60 + iSCF/60 + Rebound Created/60 +
# Penalties Drawn/60 + Shots Blocked/60 / 6 
positive_metrics <- c("Total Assists/60_zscore", "ixG/60_zscore", "iSCF/60_zscore",
                      "Rebounds Created/60_zscore", "Penalties Drawn/60_zscore",
                      "Shots Blocked/60_zscore")

# Calculate composite score for each player (row-wise mean of selected z-scores)
positive_composite <- rowMeans(Dmen_iRates_z[, positive_metrics], na.rm = TRUE)

# Insert composite column between Position (col 3) and GP (col 4)
Dmen_iRates_z <- cbind(
  Dmen_iRates_z[, 1:3],
  Positive_Composite_Zscore = positive_composite,
  Dmen_iRates_z[, 4:ncol(Dmen_iRates_z)]
)

# Rank-based percentile for Positive Composite Z-Score
Dmen_iRates_z$Positive_Composite_Percentile <-
  rank(Dmen_iRates_z$Positive_Composite_Zscore) / nrow(Dmen_iRates_z) * 100

# Move composite columns to front — reference by NAME not position
remaining_cols <- setdiff(
  names(Dmen_iRates_z),
  c("Player", "Team", "Position",
    "Positive_Composite_Zscore", "Positive_Composite_Percentile")
)

Dmen_iRates_z <- Dmen_iRates_z[, c(
  "Player", "Team", "Position",
  "Positive_Composite_Zscore", "Positive_Composite_Percentile",
  remaining_cols
)]

# Round all zscore, percentile, and composite columns to 2 decimal places
Dmen_iRates_z <- Dmen_iRates_z %>%
  mutate(across(c(contains("_zscore"), contains("_percentile"),
                  contains("Composite")), ~ round(., 2)))

# Export
write.csv(Dmen_iRates_z, "Dmen_iRates_z.csv", row.names = FALSE)