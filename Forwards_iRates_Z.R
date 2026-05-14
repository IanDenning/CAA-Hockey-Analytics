# ============================================================
# File:    Forwards_iRates_Z.R
# Author:  Ian Denning
# Project: CAA Hockey 2026 Summer Cap Project (Z-Scores & Regression Model)
#
# Purpose: Take the player's individual statistics while they are on the ice,
#          and calculate their z-scores and create a rank-based percentile
#          system for each metric. Build an iRate composite z-score, and 
#          export the final data set to CSV. This file is for the forward group.
# ============================================================
library(dplyr)

# Isolate columns I want a Z-Score in (GP through Faceoffs Lost/60)
metric_cols <- names(Forwards_iRates)[4:ncol(Forwards_iRates)]

# Convert Original data into new data frame (Keep OG data intact)
df <- Forwards_iRates

# Build a new joined data frame
result <- df[, 1:3]  # Start with Player, Team, Position

# Z-Score Loop - Scale() function doing math (x - mean(x)) / sd(x)
for (col in metric_cols) {
  result[[col]] <- df[[col]]
  result[[paste0(col, "_zscore")]] <- as.numeric(scale(df[[col]]))
}

Forwards_iRates_z <- result # Create new data frame with Z-Scores 

# Create rank-based percentile column // add right after Z-Score column 
zscore_cols <- names(Forwards_iRates_z)[grepl("_zscore", names(Forwards_iRates_z))]

# Creating rank-based percentile system
# rank() assigns each player a rank from 1 (lowest z-score) to N (highest)
# Ties get averaged ranks automatically
# nrow() - forwards, n = 404 - if Forward has 380th highest Z-Score
# 380/404 * 100 = 94th percentile 
for (col in zscore_cols) {
  perc_col <- sub("_zscore", "_percentile", col)
  idx <- which(names(Forwards_iRates_z) == col)
  
  left  <- Forwards_iRates_z[, 1:idx, drop = FALSE]
  middle <- setNames(data.frame(rank(Forwards_iRates_z[[col]]) / nrow(Forwards_iRates_z) * 100), perc_col)
  
  if (idx < ncol(Forwards_iRates_z)) {
    right <- Forwards_iRates_z[, (idx+1):ncol(Forwards_iRates_z), drop = FALSE]
    Forwards_iRates_z <- cbind(left, middle, right)
  } else {
    Forwards_iRates_z <- cbind(left, middle)
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
# Goals/60 + First Assists/60 + ixG/60 + iHDCF/60 +
# Shots/60 + Penalties Drawn/60 / 6
positive_metrics <- c("Goals/60_zscore", "First Assists/60_zscore", "ixG/60_zscore",
                      "iHDCF/60_zscore", "Shots/60_zscore", "Penalties Drawn/60_zscore")

# Calculate composite score for each player (row-wise mean of selected z-scores)
positive_composite <- rowMeans(Forwards_iRates_z[, positive_metrics], na.rm = TRUE)

# Insert composite column between Position (col 3) and GP (col 4)
Forwards_iRates_z <- cbind(
  Forwards_iRates_z[, 1:3],
  Positive_Composite_Zscore = positive_composite,
  Forwards_iRates_z[, 4:ncol(Forwards_iRates_z)]
)

# Rank-based percentile for Positive Composite Z-Score
Forwards_iRates_z$Positive_Composite_Percentile <-
  rank(Forwards_iRates_z$Positive_Composite_Zscore) / nrow(Forwards_iRates_z) * 100

# Move composite columns to front - reference by NAME not position
remaining_cols <- setdiff(
  names(Forwards_iRates_z),
  c("Player", "Team", "Position",
    "Positive_Composite_Zscore", "Positive_Composite_Percentile")
)

Forwards_iRates_z <- Forwards_iRates_z[, c(
  "Player", "Team", "Position",
  "Positive_Composite_Zscore", "Positive_Composite_Percentile",
  remaining_cols
)]

# Round all zscore, percentile, and composite columns to 2 decimal places
Forwards_iRates_z <- Forwards_iRates_z %>%
  mutate(across(c(contains("_zscore"), contains("_percentile"),
                  contains("Composite")), ~ round(., 2)))

# Export
write.csv(Forwards_iRates_z, "Forwards_iRates_z.csv", row.names = FALSE)
