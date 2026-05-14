# ============================================================
# File:    Forwards_onIce_Z.R
# Author:  Ian Denning
# Project: CAA Hockey 2026 Summer Cap Project (Z-Scores & Regression Model)
#
# Purpose: Take the team statistics while that player is on the ice, and
#          calculate z-scores and create a rank-based percentiles for each
#          metric. Build positive/negative composite scores, and exports
#          the final data set to CSV. This is for the forward group. 
# ============================================================
library(dplyr)

# Isolate columns I want a Z-Score in (GP through Off. Zone Faceoff %)
metric_cols <- names(all_strengths_Forwards_onIce)[4:ncol(all_strengths_Forwards_onIce)]
print(metric_cols[1:5])

# Convert Original data into new data frame (Keep OG data intact)
df <- all_strengths_Forwards_onIce

# Build a new joined data frame
result <- df[, 1:3]  # Start with Player, Team, Position

# Z-Score Loop - Scale() function doing math (x - mean(x)) / sd(x)
for (col in metric_cols) {
  result[[col]] <- df[[col]]
  result[[paste0(col, "_zscore")]] <- as.numeric(scale(df[[col]]))
}

all_strengths_Forwards_onIce_z <- result # Create new dataframe with Z-Scores 

# Create rank-based percentile column // add right after Z-Score column 
zscore_cols <- names(all_strengths_Forwards_onIce_z)[grepl("_zscore",names(all_strengths_Forwards_onIce_z))]

# Creating rank-based percentile system
# rank() assigns each player a rank from 1 (lowest z-score) to N (highest)
# Ties get averaged ranks automatically
# nrow() - forwards, n = 404 - if forward has 380th highest Z-Score
# 380/404 * 100 = 94th percentile 

for (col in zscore_cols) {
  perc_col <- sub("_zscore", "_percentile", col)
  idx <- which(names(all_strengths_Forwards_onIce_z) == col)
  
  left  <- all_strengths_Forwards_onIce_z[, 1:idx, drop = FALSE]
  middle <- setNames(data.frame(rank(all_strengths_Forwards_onIce_z[[col]]) / nrow(all_strengths_Forwards_onIce_z) * 100), perc_col)
  
  if (idx < ncol(all_strengths_Forwards_onIce_z)) {
    right <- all_strengths_Forwards_onIce_z[, (idx+1):ncol(all_strengths_Forwards_onIce_z), drop = FALSE]
    all_strengths_Forwards_onIce_z <- cbind(left, middle, right)
  } else {
    all_strengths_Forwards_onIce_z <- cbind(left, middle)
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
# Higher = Better 
negative_metrics <- c("CA/60_zscore", "FA/60_zscore", "xGA/60_zscore",
                      "HDCA/60_zscore", "GA/60_zscore")

# Calculate composite scores for each player (row-wise mean of selected z-scores)
positive_composite <- rowMeans(all_strengths_Forwards_onIce_z[, positive_metrics], na.rm = TRUE)
negative_composite <- rowMeans(all_strengths_Forwards_onIce_z[, negative_metrics], na.rm = TRUE)

# Insert both composite columns between Position (col 3) and GP (col 4)
all_strengths_Forwards_onIce_z <- cbind(
  all_strengths_Forwards_onIce_z[, 1:3],
  Positive_Composite_Zscore = positive_composite,
  Negative_Composite_Zscore = negative_composite,
  all_strengths_Forwards_onIce_z[, 4:ncol(all_strengths_Forwards_onIce_z)]
)

# Rank-based percentiles for composite scores
all_strengths_Forwards_onIce_z$Positive_Composite_Percentile <- 
  rank(all_strengths_Forwards_onIce_z$Positive_Composite_Zscore) / nrow(all_strengths_Forwards_onIce_z) * 100

all_strengths_Forwards_onIce_z$Negative_Composite_Percentile <- 
  rank(all_strengths_Forwards_onIce_z$Negative_Composite_Zscore) / nrow(all_strengths_Forwards_onIce_z) * 100

# Move composite columns to front — reference by NAME not position
remaining_cols <- setdiff(
  names(all_strengths_Forwards_onIce_z),
  c("Player", "Team", "Position",
    "Positive_Composite_Zscore", "Positive_Composite_Percentile",
    "Negative_Composite_Zscore", "Negative_Composite_Percentile")
)

all_strengths_Forwards_onIce_z <- all_strengths_Forwards_onIce_z[, c(
  "Player", "Team", "Position",
  "Positive_Composite_Zscore", "Positive_Composite_Percentile",
  "Negative_Composite_Zscore", "Negative_Composite_Percentile",
  remaining_cols
)]

# Round all Z-Score and percentile columns to 2 decimal places
all_strengths_Forwards_onIce_z <- all_strengths_Forwards_onIce_z %>%
  mutate(across(c(contains("_zscore"), contains("_percentile"), 
                  contains("Composite")), ~ round(., 2)))

# Export 
write.csv(all_strengths_Forwards_onIce_z, "Forwards_onIce_Z.csv", row.names = FALSE)

