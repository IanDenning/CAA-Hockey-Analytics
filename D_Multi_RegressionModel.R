# --- Dmen Multiple Linear Regression ---
library(car)
library(ggplot2)
library(scales)
library(ggrepel)

dmen_multimodel <- lm(Cap_Hit ~ TOI_perGP + xGF_per60,
                      data = Dmen_Regression)

# Assessing Assumptions
# Residuals Histogram - Defensemen
png("Hist_Residuals_Dmen.png", width = 800, height = 600, res = 120)
options(scipen = 999)
hist(dmen_multimodel$residuals, 
     main = "Dmen Regression Model - Residuals",
     xlab = "Residuals",
     col = "Dodgerblue", right = F,
     xaxt = "n")
axis(side = 1, 
     at = seq(-6e6, 4e6, by = 2e6),
     labels = paste0("$", seq(-6, 4, by = 2), "M"))
mtext("CAA Hockey Analytics - Ian Denning", 
      side = 1, line = 4, adj = 1, cex = 0.75, col = "gray50")
dev.off()
options(scipen = 0)

# Confirm Equal Variance and Linearity
# Residual Plot — Defensemen
png("Residual_Plot_Dmen.png", width = 800, height = 600, res = 120)
options(scipen = 999)
plot(dmen_multimodel$fitted.values, dmen_multimodel$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Dmen Regression Model - Residual Plot",
     pch = 20,
     xaxt = "n", yaxt = "n")
axis(side = 1,
     at = seq(0, 10e6, by = 2e6),
     labels = paste0("$", seq(0, 10, by = 2), "M"))
axis(side = 2,
     at = seq(-6e6, 4e6, by = 2e6),
     labels = paste0("$", seq(-6, 4, by = 2), "M"))
abline(h = 0, col = "Dodgerblue")
mtext("CAA Hockey Analytics - Ian Denning", 
      side = 1, line = 4, adj = 1, cex = 0.75, col = "gray50")
dev.off()
options(scipen = 0)

# Run the Model and check for Multi-Collinearity
summary(dmen_multimodel)
vif(dmen_multimodel)

# Generate predicted values
Dmen_Regression$Predicted_Cap_Hit <- predict(dmen_multimodel, Dmen_Regression)

# R² from the multiple model
dmen_r2 <- summary(dmen_multimodel)$r.squared

# Define label groups — each player appears only once
dmen_underpaid <- subset(Dmen_Regression, Predicted_Cap_Hit - Cap_Hit >= 2.5e6)

dmen_overpaid <- subset(Dmen_Regression, Cap_Hit - Predicted_Cap_Hit >= 2.5e6 &
                          !Player %in% dmen_underpaid$Player)

dmen_elite <- subset(Dmen_Regression, Cap_Hit >= 7.5e6 &
                       Predicted_Cap_Hit >= 7.5e6 &
                       !Player %in% dmen_underpaid$Player &
                       !Player %in% dmen_overpaid$Player)

# --- Dmen Multivariate Plot: Actual vs. Predicted ---
ggplot(Dmen_Regression, aes(x = Predicted_Cap_Hit, y = Cap_Hit)) +
  geom_point(aes(color = Cap_Hit), size = 2.5, alpha = 0.75) +
  scale_color_gradient(
    low = "dodgerblue",
    high = "orange",
    labels = dollar_format(scale = 1e-6, suffix = "M"),
    name = "Cap Hit"
  ) +
  scale_y_continuous(
    labels = dollar_format(scale = 1e-6, suffix = "M"),
    breaks = seq(0, 15e6, by = 2.5e6)
  ) +
  scale_x_continuous(
    labels = dollar_format(scale = 1e-6, suffix = "M"),
    breaks = seq(0, 15e6, by = 2.5e6)
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black",
              linewidth = 0.8, linetype = "dashed") +
  geom_text_repel(data = dmen_underpaid, aes(label = Player),
                  size = 2.5, max.overlaps = 20,
                  box.padding = 0.5, point.padding = 0.3,
                  segment.size = 0.3, segment.color = "gray50",
                  min.segment.length = 0) +
  geom_text_repel(data = dmen_overpaid, aes(label = Player),
                  size = 2.5, max.overlaps = 20,
                  box.padding = 0.5, point.padding = 0.3,
                  segment.size = 0.3, segment.color = "gray50",
                  min.segment.length = 0) +
  geom_text_repel(data = dmen_elite, aes(label = Player),
                  size = 2.5, max.overlaps = 20,
                  box.padding = 0.5, point.padding = 0.3,
                  segment.size = 0.3, segment.color = "gray50",
                  min.segment.length = 0) +
  annotate("text",
           x = -Inf, y = Inf,
           hjust = -0.1, vjust = 1.5,
           label = paste0("R² = ", round(dmen_r2, 3)),
           size = 4.0, fontface = "bold", color = "gray20") +
  labs(
    title = "NHL Defensemen: Actual vs. Predicted Cap Hit",
    subtitle = "Above dashed line = overpaid | Below dashed line = underpaid",
    x = "Predicted Cap Hit ($ Millions)",
    y = "Actual Cap Hit ($ Millions)",
    caption = "CAA Hockey Analytics - Ian Denning"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray50", size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

ggsave("Dmen_Actual_vs_Predicted.png", width = 10, height = 8, dpi = 300, bg = "white")