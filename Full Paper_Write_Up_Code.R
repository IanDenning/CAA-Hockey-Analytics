library(ggplot2)
library(scales)
library(dplyr)
library(scales)
# Forwards
summary(Forwards_Regression[, c("Cap_Hit", "TOI_perGP", "iTotal_Points_per60")])
sd(Forwards_Regression$Cap_Hit)
sd(Forwards_Regression$TOI_perGP)
sd(Forwards_Regression$iTotal_Points_per60)

# Defensemen
summary(Dmen_Regression[, c("Cap_Hit", "TOI_perGP", "xGF_per60")])
sd(Dmen_Regression$Cap_Hit)
sd(Dmen_Regression$TOI_perGP)
sd(Dmen_Regression$xGF_per60)

# ============================================================
# FORWARDS
# ============================================================

# Cap Hit — Forwards
ggplot(Forwards_Regression, aes(x = Cap_Hit)) +
  geom_histogram(bins = 30, fill = "dodgerblue", color = "white", linewidth = 0.3) +
  geom_vline(aes(xintercept = mean(Cap_Hit)), color = "green", linewidth = 0.8, linetype = "solid") +
  geom_vline(aes(xintercept = median(Cap_Hit)), color = "orange", linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = mean(Forwards_Regression$Cap_Hit) + 300000, y = Inf,
           label = "Mean", color = "black", size = 3, hjust = 0, vjust = 4) +
  annotate("text", x = median(Forwards_Regression$Cap_Hit) + 300000, y = Inf,
           label = "Median", color = "black", size = 3, hjust = 1.5, vjust = 3) +
  scale_x_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  labs(title = "Distribution of Cap Hit (Forwards)",
       subtitle = "n = 404 | 2023-2026",
       x = "Cap Hit (AAV)", y = "Count",
       caption = "CAA Hockey Analytics - Ian Denning") +
  theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = "#F7F7F7", color = NA),
        panel.grid.minor = element_blank())

ggsave("Hist_CapHit_Forwards.png", width = 6, height = 4, dpi = 300, bg = "#F7F7F7")

# TOI/GP — Forwards
ggplot(Forwards_Regression, aes(x = TOI_perGP)) +
  geom_histogram(bins = 30, fill = "dodgerblue", color = "white", linewidth = 0.3) +
  geom_vline(aes(xintercept = mean(TOI_perGP)), color = "green", linewidth = 0.8, linetype = "solid") +
  geom_vline(aes(xintercept = median(TOI_perGP)), color = "orange", linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = mean(Forwards_Regression$TOI_perGP) + 0.1, y = Inf,
           label = "Mean", color = "black", size = 3, hjust = 0, vjust = 1.5) +
  annotate("text", x = median(Forwards_Regression$TOI_perGP) + 0.1, y = Inf,
           label = "Median", color = "black", size = 3, hjust = 1.3, vjust = 2.5) +
  labs(title = "Distribution of TOI/GP (Forwards)",
       subtitle = "n = 404 | 2023-2026",
       x = "Time on Ice per Game (minutes)", y = "Count",
       caption = "CAA Hockey Analytics - Ian Denning") +
  theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = "#F7F7F7", color = NA),
        panel.grid.minor = element_blank())

ggsave("Hist_TOI_Forwards.png", width = 6, height = 4, dpi = 300, bg = "#F7F7F7")

# Total Points/60 — Forwards
ggplot(Forwards_Regression, aes(x = iTotal_Points_per60)) +
  geom_histogram(bins = 30, fill = "dodgerblue", color = "white", linewidth = 0.3) +
  geom_vline(aes(xintercept = mean(iTotal_Points_per60)), color = "green", linewidth = 0.8, linetype = "solid") +
  geom_vline(aes(xintercept = median(iTotal_Points_per60)), color = "orange", linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = mean(Forwards_Regression$iTotal_Points_per60) + 0.02, y = Inf,
           label = "Mean", color = "black", size = 3, hjust = 0, vjust = 1.5) +
  annotate("text", x = median(Forwards_Regression$iTotal_Points_per60) + 0.02, y = Inf,
           label = "Median", color = "black", size = 3, hjust = 1.2, vjust = 2) +
  labs(title = "Distribution of Total Points/60 (Forwards)",
       subtitle = "n = 404 | 2023-2026",
       x = "Total Points per 60 Minutes", y = "Count",
       caption = "CAA Hockey Analytics - Ian Denning") +
  theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = "#F7F7F7", color = NA),
        panel.grid.minor = element_blank())

ggsave("Hist_Points_Forwards.png", width = 6, height = 4, dpi = 300, bg = "#F7F7F7")

# ============================================================
# DEFENSEMEN
# ============================================================

# Cap Hit — Defensemen
ggplot(Dmen_Regression, aes(x = Cap_Hit)) +
  geom_histogram(bins = 30, fill = "dodgerblue", color = "white", linewidth = 0.3) +
  geom_vline(aes(xintercept = mean(Cap_Hit)), color = "green", linewidth = 0.8, linetype = "solid") +
  geom_vline(aes(xintercept = median(Cap_Hit)), color = "orange", linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = mean(Dmen_Regression$Cap_Hit) + 300000, y = Inf,
           label = "Mean", color = "black", size = 3, hjust = 0.4, vjust = 2.0) +
  annotate("text", x = median(Dmen_Regression$Cap_Hit) + 300000, y = Inf,
           label = "Median", color = "black", size = 3, hjust = 1.5, vjust = 3) +
  scale_x_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  labs(title = "Distribution of Cap Hit (Defenseman)",
       subtitle = "n = 218 | 2023-2026",
       x = "Cap Hit (AAV)", y = "Count",
       caption = "CAA Hockey Analytics - Ian Denning") +
  theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = "#F7F7F7", color = NA),
        panel.grid.minor = element_blank())

ggsave("Hist_CapHit_Dmen.png", width = 6, height = 4, dpi = 300, bg = "#F7F7F7")

# TOI/GP — Defensemen
ggplot(Dmen_Regression, aes(x = TOI_perGP)) +
  geom_histogram(bins = 30, fill = "dodgerblue", color = "white", linewidth = 0.3) +
  geom_vline(aes(xintercept = mean(TOI_perGP)), color = "green", linewidth = 0.8, linetype = "solid") +
  geom_vline(aes(xintercept = median(TOI_perGP)), color = "orange", linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = mean(Dmen_Regression$TOI_perGP) + 0.1, y = Inf,
           label = "Mean", color = "black", size = 3, hjust = 0, vjust = 1.5) +
  annotate("text", x = median(Dmen_Regression$TOI_perGP) + 0.1, y = Inf,
           label = "Median", color = "black", size = 3, hjust = 1.2, vjust = 2.5) +
  labs(title = "Distribution of TOI/GP (Defensemen)",
       subtitle = "n = 218 | 2023-2026",
       x = "Time on Ice per Game (minutes)", y = "Count",
       caption = "CAA Hockey Analytics - Ian Denning") +
  theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = "#F7F7F7", color = NA),
        panel.grid.minor = element_blank())

ggsave("Hist_TOI_Dmen.png", width = 6, height = 4, dpi = 300, bg = "#F7F7F7")

# xGF/60 — Defensemen
ggplot(Dmen_Regression, aes(x = xGF_per60)) +
  geom_histogram(bins = 30, fill = "dodgerblue", color = "white", linewidth = 0.3) +
  geom_vline(aes(xintercept = mean(xGF_per60)), color = "green", linewidth = 0.8, linetype = "solid") +
  geom_vline(aes(xintercept = median(xGF_per60)), color = "orange", linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = mean(Dmen_Regression$xGF_per60) + 0.02, y = Inf,
           label = "Mean", color = "black", size = 3, hjust = 0, vjust = 1.5) +
  annotate("text", x = median(Dmen_Regression$xGF_per60) + 0.02, y = Inf,
           label = "Median", color = "black", size = 3, hjust = 1.2, vjust = 3) +
  labs(title = "Distribution of xGF/60 (Defensemen)",
       subtitle = "n = 218 | 2023-2026",
       x = "Expected Goals For per 60 Minutes (On-Ice)", y = "Count",
       caption = "CAA Hockey Analytics - Ian Denning") +
  theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = "#F7F7F7", color = NA),
        panel.grid.minor = element_blank())

ggsave("Hist_xGF_Dmen.png", width = 6, height = 4, dpi = 300, bg = "#F7F7F7")


# TOI/GP vs Cap Hit — Defensemen (Simple Linear Regression)
ggplot(Dmen_Regression, aes(x = TOI_perGP, y = Cap_Hit)) +
  geom_point(color = "dodgerblue", alpha = 0.6, size = 1.8) +
  geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 0.9) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("r = ", round(cor(Dmen_Regression$TOI_perGP, Dmen_Regression$Cap_Hit), 3)),
           hjust = 5.5, vjust = 2.0, size = 5, fontface = "bold") +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "TOI per GP vs. Cap Hit (Defensemen)",
       subtitle = "n = 218 | 2023-2026",
       x = "Time on Ice per Game (Minutes)",
       y = "Cap Hit ($)",
       caption = "CAA Hockey Analytics - Ian Denning") +
  theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = "#F7F7F7", color = NA),
        panel.grid.minor = element_blank())

ggsave("Scatter_TOI_CapHit_Dmen.png", width = 6, height = 4, dpi = 300, bg = "#F7F7F7")

# TOI/GP vs Cap Hit — Forwards
ggplot(Forwards_Regression, aes(x = TOI_perGP, y = Cap_Hit)) +
  geom_point(color = "dodgerblue", alpha = 0.6, size = 1.8) +
  geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 0.9) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("r = ", round(cor(Forwards_Regression$TOI_perGP, Forwards_Regression$Cap_Hit), 3)),
           hjust = 5.5, vjust = 2, size = 5, fontface = "bold") +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "TOI per GP vs. Cap Hit (Forwards)",
       subtitle = "n = 404 | 2023-2026",
       x = "Time on Ice per Game (Minutes)",
       y = "Cap Hit ($)",
       caption = "CAA Hockey Analytics - Ian Denning") +
  theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = "#F7F7F7", color = NA),
        panel.grid.minor = element_blank())

ggsave("Scatter_TOI_CapHit_Forwards.png", width = 6, height = 4, dpi = 300, bg = "#F7F7F7")