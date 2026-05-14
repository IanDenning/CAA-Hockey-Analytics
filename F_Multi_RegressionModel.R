# --- Multiple Linear Regression ---
library(car)
library(ggplot2)
library(scales)
library(ggrepel)

forwards_multimodel <- lm(Cap_Hit ~ TOI_perGP + Total_Points_per60, 
                  data = Forwards_Regression)

# Assessing Assumptions 
hist(forwards_multimodel$residuals, main = "Forwards Regression Model - Residuals",
     xlab = "Residuals",
     col = "Dodgerblue",right = F)

# Confirm Equal Variance and Linearity
plot(forwards_multimodel$fitted.values, forwards_multimodel$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Forwards Regression Model - Residual Plot",
     pch = 20)
abline(h = 0, col = "Dodgerblue")

# Run the Model and check for Multi-Collinearity
summary(model_multi)
vif(model_multi)

# Generate predicted values
Forwards_Regression$Predicted_Cap_Hit <- predict(forwards_multimodel, Forwards_Regression)

# R² from the multiple model
multi_r2 <- summary(forwards_multimodel)$r.squared

# --- Multivariate Plot: Actual vs. Predicted ---
ggplot(Forwards_Regression, aes(x = Predicted_Cap_Hit, y = Cap_Hit)) +
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
  # Underpaid — actual salary ≤$2.5M, predicted between $5M and $7.5M
  geom_text_repel(
    data = subset(Forwards_Regression, Cap_Hit <= 2.5e6 & 
                    Predicted_Cap_Hit >= 5e6 &
                    Predicted_Cap_Hit < 7.5e6),
    aes(label = Player),
    size = 2.5, max.overlaps = 20
  ) +
  # Underpaid — predicted ≥$7.5M but earning less than predicted
  geom_text_repel(
    data = subset(Forwards_Regression, Predicted_Cap_Hit >= 7.5e6 & 
                    Cap_Hit < Predicted_Cap_Hit),
    aes(label = Player),
    size = 2.5, max.overlaps = 20
  ) +
  # Overpaid — actual salary is $3M+ above predicted
  geom_text_repel(
    data = subset(Forwards_Regression, Cap_Hit - Predicted_Cap_Hit >= 3e6),
    aes(label = Player),
    size = 2.5, max.overlaps = 20
  ) +
  # High earners ≥$9M not already captured by overpaid layer
  geom_text_repel(
    data = subset(Forwards_Regression, Cap_Hit >= 9e6 & 
                    Cap_Hit - Predicted_Cap_Hit < 3e6),
    aes(label = Player),
    size = 2.5, max.overlaps = 20
  ) +
  annotate("text",
           x = -Inf, y = Inf,
           hjust = -0.1, vjust = 1.5,
           label = paste0("R² = ", round(multi_r2, 3)),
           size = 4.0, fontface = "bold", color = "gray20") +
  labs(
    title = "NHL Forwards — Actual vs. Predicted Cap Hit",
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