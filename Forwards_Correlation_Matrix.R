 
# Isolate only numeric columns (drops Player, Agent)
forwards_cor_metrics <- Forwards_Regression %>%
  select(-Player, -Agent) %>%
  select(where(is.numeric))

# Build the correlation matrix
forwards_cor_matrix <- cor(forwards_cor_metrics, use = "complete.obs")

# Pull only correlations against Cap_Hit, sorted highest to lowest
forwards_cap_hit_cors <- forwards_cor_matrix["Cap_Hit", ]
forwards_cap_hit_cors_sorted <- sort(forwards_cap_hit_cors, decreasing = TRUE)
print(forwards_cap_hit_cors_sorted)

write.csv(forwards_cor_matrix, "forwards_cor_matrix.csv")

