# --- Multiple Linear Regression ---
library(car)
library(ggplot2)
library(scales)
library(ggrepel)

forwards_multimodel <- lm(Cap_Hit ~ TOI_perGP + iTotal_Points_per60, 
                  data = Forwards_Regression)

# Assessing Assumptions 
# Residuals Histogram - Forwards
png("Hist_Residuals_Forwards.png", width = 800, height = 600, res = 120)

options(scipen = 999)  # suppress scientific notation

hist(forwards_multimodel$residuals, 
     main = "Forwards Regression Model - Residuals",
     xlab = "Residuals",
     col = "Dodgerblue", right = F,
     xaxt = "n")

# Custom x-axis with $M labels
axis(side = 1, 
     at = seq(-8e6, 6e6, by = 2e6),
     labels = paste0("$", seq(-8, 6, by = 2), "M"))

mtext("CAA Hockey Analytics - Ian Denning", 
      side = 1, line = 4, adj = 1, cex = 0.75, col = "gray50")
dev.off()

options(scipen = 0)

# Confirm Equal Variance and Linearity
# Residual Plot - Forwards
png("Residual_Plot_Forwards.png", width = 800, height = 600, res = 120)
options(scipen = 999)
plot(forwards_multimodel$fitted.values, forwards_multimodel$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Forwards Regression Model - Residual Plot",
     pch = 20,
     xaxt = "n", yaxt = "n")
axis(side = 1,
     at = seq(0, 10e6, by = 2e6),
     labels = paste0("$", seq(0, 10, by = 2), "M"))
axis(side = 2,
     at = seq(-8e6, 6e6, by = 2e6),
     labels = paste0("$", seq(-8, 6, by = 2), "M"))
abline(h = 0, col = "Dodgerblue")
mtext("CAA Hockey Analytics - Ian Denning", 
      side = 1, line = 4, adj = 1, cex = 0.75, col = "gray50")
dev.off()
options(scipen = 0)

# Run the Model and check for Multi-Collinearity
summary(forwards_multimodel)
vif(forwards_multimodel)

# Generate predicted values
Forwards_Regression$Predicted_Cap_Hit <- predict(forwards_multimodel, Forwards_Regression)

# R² from the multiple model
f_multi_r2 <- summary(forwards_multimodel)$r.squared

# Define label groups — each player appears only once
underpaid <- subset(Forwards_Regression, Cap_Hit <= 2.5e6 & Predicted_Cap_Hit >= 5e6)

overpaid <- subset(Forwards_Regression, Cap_Hit - Predicted_Cap_Hit >= 3e6 &
                     !Player %in% underpaid$Player)

elite <- subset(Forwards_Regression, Cap_Hit >= 9e6 &
                  !Player %in% underpaid$Player &
                  !Player %in% overpaid$Player)

# --- Multivariate Plot: Actual vs. Predicted ---
ggplot(Forwards_Regression, aes(x = Predicted_Cap_Hit, y = Cap_Hit)) +
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
  geom_text_repel(data = underpaid, aes(label = Player), 
                  size = 2.5, max.overlaps = 20,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.size = 0.3,
                  segment.color = "gray50",
                  min.segment.length = 0) +
  geom_text_repel(data = overpaid, aes(label = Player), 
                  size = 2.5, max.overlaps = 20,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.size = 0.3,
                  segment.color = "gray50",
                  min.segment.length = 0) +
  geom_text_repel(data = elite, aes(label = Player), 
                  size = 2.5, max.overlaps = 20,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.size = 0.3,
                  segment.color = "gray50",
                  min.segment.length = 0) +
  annotate("text",
           x = -Inf, y = Inf,
           hjust = -0.1, vjust = 1.5,
           label = paste0("R² = ", round(f_multi_r2, 3)),
           size = 4.0, fontface = "bold", color = "gray20") +
  labs(
    title = "NHL Forwards: Actual vs. Predicted Cap Hit",
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
ggsave("Forwards_Actual_vs_Predicted.png", width = 10, height = 8, dpi = 300, bg = "white")