library(ggplot2)
library(ggrepel)
library(scales)

# ============================================================
# CELEBRINI — Actual vs. Predicted Cap Hit
# Named comp groups for write-up
# ============================================================

# R² from the model (already run)
f_multi_r2 <- summary(forwards_multimodel)$r.squared

# ============================================================
# COMP GROUPS — all labels pulled from these lists
# ============================================================

draft_class_peers <- c(
  "Bedard, Connor",
  "Slafkovský, Juraj",
  "Hughes, Jack",
  "Matthews, Auston",
  "McDavid, Connor"
)

young_guns <- c(
  "Carlsson, Leo",
  "Fantilli, Adam",
  "Michkov, Matvei",
  "Benson, Zach",
  "Cooley, Logan",
  "Gauthier, Cutter",
  "Kasper, Marco",
  "Nazar, Frank"
)

team_canada <- c(
  "MacKinnon, Nathan",
  "Crosby, Sidney",
  "Marner, Mitch",
  "Stone, Mark",
  "Suzuki, Nick",
  "Reinhart, Sam"
)

superstars <- c(
  "Kucherov, Nikita",
  "Scheifele, Mark",
  "Pastrnak, David",
  "Necas, Martin",
  "Draisaitl, Leon",
  "Robertson, Jason",
  "Eichel, Jack",
  "Kaprizov, Kirill",
  "Nylander, William",
  "Rantanen, Mikko"
)

celebrini <- c("Celebrini, Macklin")

all_labeled <- c(celebrini, draft_class_peers, young_guns, team_canada, superstars)

# ============================================================
# SUBSET LABELED PLAYERS
# ============================================================

labeled_df <- subset(Forwards_Regression, Player %in% all_labeled)

# Assign group for color
labeled_df$group <- ifelse(labeled_df$Player %in% celebrini,        "Celebrini",
                           ifelse(labeled_df$Player %in% draft_class_peers, "Draft Class / 1st Overalls",
                                  ifelse(labeled_df$Player %in% young_guns,         "Young Guns",
                                         ifelse(labeled_df$Player %in% team_canada,        "Team Canada",
                                                ifelse(labeled_df$Player %in% superstars,         "Superstars", "Other")))))

# Unlabeled background points
unlabeled_df <- subset(Forwards_Regression, !Player %in% all_labeled)

# ============================================================
# PLOT
# ============================================================

group_colors <- c(
  "Celebrini"                  = "#E74C3C",
  "Draft Class / 1st Overalls" = "#2980B9",
  "Young Guns"                 = "#27AE60",
  "Team Canada"                = "#E67E22",
  "Superstars"                 = "#8E44AD"
)

ggplot() +
  
  # Background unlabeled players — grey
  geom_point(data = unlabeled_df,
             aes(x = Predicted_Cap_Hit, y = Cap_Hit),
             color = "#CCCCCC", size = 2, alpha = 0.6) +
  
  # Labeled players — colored by group
  geom_point(data = labeled_df,
             aes(x = Predicted_Cap_Hit, y = Cap_Hit, color = group),
             size = 3, alpha = 0.9) +
  
  # 45-degree perfect prediction line
  geom_abline(intercept = 0, slope = 1,
              color = "black", linewidth = 0.8, linetype = "dashed") +
  
  # Labels
  geom_text_repel(data = labeled_df,
                  aes(x = Predicted_Cap_Hit, y = Cap_Hit,
                      label = Player, color = group),
                  size = 2.6,
                  max.overlaps = 30,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.size = 0.3,
                  segment.color = "gray60",
                  min.segment.length = 0,
                  show.legend = FALSE) +
  
  # R² annotation
  annotate("text",
           x = -Inf, y = Inf,
           hjust = -0.1, vjust = 1.5,
           label = paste0("R² = ", round(f_multi_r2, 2)),
           size = 4.0, fontface = "bold", color = "gray20") +
  
  annotate("text",
           x = -Inf, y = Inf,
           hjust = 0.002, vjust = 4.0,
           label = "Cap Hit = -$6,505,372 + ($512,240 × TOI/GP) + ($1,312,925 × Total Points/60)",
           size = 3.2, fontface = "italic", color = "gray40") +
  
  scale_color_manual(values = group_colors, name = "") +
  
  scale_y_continuous(
    labels = dollar_format(scale = 1e-6, suffix = "M"),
    breaks = seq(0, 15e6, by = 2.5e6)
  ) +
  scale_x_continuous(
    labels = dollar_format(scale = 1e-6, suffix = "M"),
    breaks = seq(0, 15e6, by = 2.5e6)
  ) +
  
  labs(
    title    = "NHL Forwards: Actual vs. Predicted Cap Hit",
    subtitle = "Macklin Celebrini - Comparable Players Highlighted",
    x        = "Predicted Cap Hit ($ Millions)",
    y        = "Actual Cap Hit ($ Millions)",
    caption  = "CAA Hockey Analytics - Ian Denning"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "gray50", size = 11),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.text      = element_text(size = 9)
  )

ggsave("Celebrini_Actual_vs_Predicted.png",
       width = 10, height = 8, dpi = 300, bg = "white")

cat("Saved: Celebrini_Actual_vs_Predicted.png\n")