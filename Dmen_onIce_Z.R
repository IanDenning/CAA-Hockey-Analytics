# ============================================================
# File:    Dmen_onIce_Z.R
# Author:  Ian Denning
# Project: CAA Hockey 2026 Summer Cap Project (Z-Scores & Regression Model)
#
# Purpose: Take the team statistics while that player is on the ice, and
#          calculate z-scores and create a rank-based percentiles for each
#          metric. Build positive/negative composite scores, and exports
#          the final data set to CSV. This file is for the defenceman group. 
# ============================================================
library(dplyr)

# Isolate columns I want a Z-Score in (GP through Off. Zone Faceoff %)
metric_cols <- names(Dmen_onIce)[4:ncol(Dmen_onIce)]

# Convert Original data into new data frame (Keep OG data intact)
df <- Dmen_onIce

# Build a new joined data frame
result <- df[, 1:3]  # Start with Player, Team, Position

# Z-Score Loop - Scale() function doing math (x - mean(x)) / sd(x)
for (col in metric_cols) {
  result[[col]] <- df[[col]]
  result[[paste0(col, "_zscore")]] <- as.numeric(scale(df[[col]]))
}

Dmen_onIce_z <- result # Create new data frame with Z-Scores 

# Create rank-based percentile column // add right after Z-Score column 
zscore_cols <- names(Dmen_onIce_z)[grepl("_zscore", names(Dmen_onIce_z))]

# Creating rank-based percentile system
# rank() assigns each player a rank from 1 (lowest z-score) to N (highest)
# Ties get averaged ranks automatically
# nrow() - defenceman, n = 218 - if dman has 180th highest Z-Score
# 180/218 * 100 = 83rd percentile 
for (col in zscore_cols) {
  perc_col <- sub("_zscore", "_percentile", col)
  idx <- which(names(Dmen_onIce_z) == col)
  
  left  <- Dmen_onIce_z[, 1:idx, drop = FALSE]
  middle <- setNames(data.frame(rank(Dmen_onIce_z[[col]]) / nrow(Dmen_onIce_z) * 100), perc_col)
  
  if (idx < ncol(Dmen_onIce_z)) {
    right <- Dmen_onIce_z[, (idx+1):ncol(Dmen_onIce_z), drop = FALSE]
    Dmen_onIce_z <- cbind(left, middle, right)
  } else {
    Dmen_onIce_z <- cbind(left, middle)
  }
}

# Creating onIce Composite Z-Scores
# Identify most important metrics to quantify offensive/defensive value or production
# Take the average of those identified Z-Scores

# Positive onIce Composite Z-Score (F & D) = 
# TOI/GP + CF% + FF% + xGF% + SCF% + HDCF% + GF% + xGF/60 / 8
# Higher = Better 
positive_metrics <- c("TOI/GP_zscore", "CF%_zscore", "FF%_zscore", "xGF%_zscore",
                      "SCF%_zscore", "HDCF%_zscore", "GF%_zscore", "xGF/60_zscore")

# Negative onIce Composite Z-Score (F & D) = 
# CA/60 + FA/60 + xGA/60 + HDCA/60 + GA/60 / 5
# Higher = Worse
negative_metrics <- c("CA/60_zscore", "FA/60_zscore", "xGA/60_zscore",
                      "HDCA/60_zscore", "GA/60_zscore")

# Calculate composite scores for each player (row-wise mean of selected z-scores)
positive_composite <- rowMeans(Dmen_onIce_z[, positive_metrics], na.rm = TRUE)
negative_composite <- rowMeans(Dmen_onIce_z[, negative_metrics], na.rm = TRUE)

# Insert both composite columns between Position (col 3) and GP (col 4)
Dmen_onIce_z <- cbind(
  Dmen_onIce_z[, 1:3],
  Positive_Composite_Zscore = positive_composite,
  Negative_Composite_Zscore = negative_composite,
  Dmen_onIce_z[, 4:ncol(Dmen_onIce_z)]
)

# Rank-based percentile for Positive Composite Zscore
Dmen_onIce_z$Positive_Composite_Percentile <-
  rank(Dmen_onIce_z$Positive_Composite_Zscore) / nrow(Dmen_onIce_z) * 100

# Rank-based percentile for Negative Composite Zscore
Dmen_onIce_z$Negative_Composite_Percentile <-
  rank(Dmen_onIce_z$Negative_Composite_Zscore) / nrow(Dmen_onIce_z) * 100

# Move composite columns to front — reference by NAME not position
remaining_cols <- setdiff(
  names(Dmen_onIce_z),
  c("Player", "Team", "Position",
    "Positive_Composite_Zscore", "Positive_Composite_Percentile",
    "Negative_Composite_Zscore", "Negative_Composite_Percentile")
)

Dmen_onIce_z <- Dmen_onIce_z[, c(
  "Player", "Team", "Position",
  "Positive_Composite_Zscore", "Positive_Composite_Percentile",
  "Negative_Composite_Zscore", "Negative_Composite_Percentile",
  remaining_cols
)]

# Round all zscore and percentile columns to 2 decimal places
Dmen_onIce_z <- Dmen_onIce_z %>%
  mutate(across(c(contains("_zscore"), contains("_percentile"),
                  contains("Composite")), ~ round(., 2)))

# Export
write.csv(Dmen_onIce_z, "Dmen_onIce_z.csv", row.names = FALSE)