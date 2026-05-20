library(dplyr)

# Isolate only numeric columns (drops Player, Agent)
dmen_cor_metrics <- Dmen_Regression %>%
  select(-Player, -Agent) %>%
  select(where(is.numeric))

# Build the correlation matrix
dmen_cor_matrix <- cor(dmen_cor_metrics, use = "complete.obs")

# Pull only correlations against Cap_Hit, sorted highest to lowest
dmen_cap_hit_cors <- dmen_cor_matrix["Cap_Hit", ]
dmen_cap_hit_cors_sorted <- sort(dmen_cap_hit_cors, decreasing = TRUE)
print(dmen_cap_hit_cors_sorted)

write.csv(dmen_cor_matrix, "dmen_cor_matrix.csv")