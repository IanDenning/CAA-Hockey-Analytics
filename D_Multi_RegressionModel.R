# --- Dmen Multiple Linear Regression ---
library(car)
library(ggplot2)
library(scales)
library(ggrepel)

dmen_multimodel <- lm(Cap_Hit ~ TOI_perGP + xGF_per60,
                      data = Dmen_Regression)

# Assessing Assumptions
hist(dmen_multimodel$residuals, main = "Dmen Regression Model - Residuals",
     xlab = "Residuals",
     col = "Dodgerblue", right = F)

# Confirm Equal Variance and Linearity
plot(dmen_multimodel$fitted.values, dmen_multimodel$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Dmen Regression Model - Residual Plot",
     pch = 20)
abline(h = 0, col = "Dodgerblue")

# Run the Model and check for Multi-Collinearity
summary(dmen_multimodel)
vif(dmen_multimodel)

# Generate predicted values
Dmen_Regression$Predicted_Cap_Hit <- predict(dmen_multimodel, Dmen_Regression)

# R² from the multiple model
dmen_r2 <- summary(dmen_multimodel)$r.squared

# --- Dmen Multivariate Plot: Actual vs. Predicted ---
ggplot(Dmen_Regression, aes(x = Predicted_Cap_Hit, y = Cap_Hit)) +
  geom_point(aes(color = Cap_Hit), size = 2.5, alpha = 0.75) +
  scale_color_gradient(
    low = "#56B4E9",
    high = "#D55E00",
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
  # Underpaid — actual salary is $2.5M+ below predicted (below the line)
  geom_text_repel(
    data = subset(Dmen_Regression, Predicted_Cap_Hit - Cap_Hit >= 2.5e6),
    aes(label = Player),
    size = 2.5, max.overlaps = 20
  ) +
  # Overpaid — actual salary is $2.5M+ above predicted (above the line)
  geom_text_repel(
    data = subset(Dmen_Regression, Cap_Hit - Predicted_Cap_Hit >= 2.5e6),
    aes(label = Player),
    size = 2.5, max.overlaps = 20
  ) +
  # Elite tier — both actual and predicted ≥$7.5M, not already labeled above
  geom_text_repel(
    data = subset(Dmen_Regression, Cap_Hit >= 7.5e6 & 
                    Predicted_Cap_Hit >= 7.5e6 &
                    abs(Cap_Hit - Predicted_Cap_Hit) < 2.5e6),
    aes(label = Player),
    size = 2.5, max.overlaps = 20
  ) +
  # Specific player labels
  geom_text_repel(
    data = subset(Dmen_Regression, Player %in% c("Ceci, Cody", "Burns, Brent", "Nurse, Darnell")),
    aes(label = Player),
    size = 2.5, max.overlaps = 20
  ) +
  annotate("text",
           x = -Inf, y = Inf,
           hjust = -0.1, vjust = 1.5,
           label = paste0("R² = ", round(dmen_r2, 3)),
           size = 4.0, fontface = "bold", color = "gray20") +
  labs(
    title = "NHL Defensemen — Actual vs. Predicted Cap Hit",
    subtitle = "Above dashed line = overpaid | Below dashed line = underpaid",
    x = "Predicted Cap Hit ($ Millions)",
    y = "Actual Cap Hit ($ Millions)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray50", size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )