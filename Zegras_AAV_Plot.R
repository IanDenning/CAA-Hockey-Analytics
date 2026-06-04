# ============================================================
# TREVOR ZEGRAS — EUCLIDEAN DISTANCE COMP FINDER & AAV PLOT
# CAA Hockey Analytics - Ian Denning
# ============================================================
# ------------------------------------------------------------
# CONTEXT
# Celebrini and Robertson comp identification was straightforward —
# both players rank in the top 5–10% across most metrics, making
# their peer group obvious. Zegras requires a different approach.
#
# His 3-year window (2023–26) is suppressed by two injury-riddled
# seasons in Anaheim, compounded by a coaching staff that did not
# deploy him to his strengths. His 2025-26 bounce-back in Philadelphia
# (67 points, 0.83 pts/GP) is more representative of his true ceiling —
# a 60–70 point per season, high-skill offensive driver.
#
# We use Euclidean Distance across 6 z-score dimensions to 
# mathematically identify his closest statistical twins in the dataset,
# then use those comps to anchor the contract argument,
# in addition to the regression model and z-scores.
# ------------------------------------------------------------
library(dplyr)

# Step 1 — pick your dimensions (z-score columns)
z_cols <- c("TOI_perGP_zScore", 
            "iTotal_Points_per60_zScore",
            "iFirst_Assists_per60_zScore",
            "ixG_per60_zScore",
            "SCF_pct_zScore",
            "Carries_per60_zScore")

# Step 2 — pull Zegras' z-score vector
zegras_vec <- Forwards_zScores_Master %>%
  filter(Player == "Zegras, Trevor") %>%
  select(all_of(z_cols)) %>%
  as.numeric()

# Step 3 — compute distance for every other player
comps <- Forwards_zScores_Master %>%
  filter(Player != "Zegras, Trevor") %>%
  rowwise() %>%
  mutate(
    euclid_dist = sqrt(sum((c_across(all_of(z_cols)) - zegras_vec)^2))
  ) %>%
  ungroup() %>%
  arrange(euclid_dist)

# Step 4 — Create a list of the Top 15 closest players to Zegras
comps %>% select(Player, Team, Cap_Hit, euclid_dist) %>% head(15)

# Creating Benchmarks - Relative to Zegras 
# Create a distribution for all 404 forwards from Zegras
comps %>%
  summarise(
    min    = min(euclid_dist),
    p10    = quantile(euclid_dist, 0.10), # p10 = Top 10% closest
    p25    = quantile(euclid_dist, 0.25), # p25 = Top 25% closest, etc.
    median = median(euclid_dist),
    p75    = quantile(euclid_dist, 0.75),
    max    = max(euclid_dist)
  )

# Run p10
comps %>%
  filter(euclid_dist < 1.34) %>%
  select(Player, Team, Cap_Hit, euclid_dist) %>%
  arrange(euclid_dist) %>%
  print(n = 48)

# Create Euclidean Distance Matrix
# CF_pct_zScore and SCF_pct_zScore were found to have a 0.98 correlation
# CF_pct was taken out (reflected at the beginning of the file)
Forwards_zScores_Master %>%
  select(
    TOI_perGP_zScore,
    iTotal_Points_per60_zScore,
    iFirst_Assists_per60_zScore,
    ixG_per60_zScore,
    CF_pct_zScore,
    SCF_pct_zScore,
    Carries_per60_zScore
  ) %>%
  cor() %>%
  as.data.frame() %>%
  write.csv("Zegras_Euclidean_Correlation_Matrix.csv")

# Export p10 comp list to CSV
comps %>%
  filter(euclid_dist < 1.34) %>%
  select(Player, Team, Cap_Hit, euclid_dist) %>%
  arrange(euclid_dist) %>%
  write.csv("Zegras_Euclidean_Comps_p10.csv", row.names = FALSE)

# ============================================================
# ZEGRAS — Actual vs. Predicted Cap Hit
# ============================================================
library(ggplot2)
library(ggrepel)
library(scales)

f_multi_r2 <- summary(forwards_multimodel)$r.squared

# ============================================================
# COMP GROUPS
# ============================================================

zegras <- c("Zegras, Trevor")

ntdp <- c(
  "Hughes, Jack",
  "Caufield, Cole",
  "Boldy, Matt",
  "Turcotte, Alex"
)

draft_class <- c(
  "Kakko, Kaapo",
  "Cozens, Dylan",
  "Newhook, Alex",
  "Krebs, Peyton",
  "Pinto, Shane",
  "Dorofeyev, Pavel",
  "Protas, Aliaksei",   # note spelling
  "Maccelli, Matias",
  "Voronkov, Dmitri"
  # Dach, Kirby — not in dataset (games threshold)
)

philly <- c(
  "Konecny, Travis",
  "Tippett, Owen",
  "Dvorak, Christian",
  "Michkov, Matvei",
  "Couturier, Sean",
  "Frost, Morgan",
  "Farabee, Joel"
)

ceilings <- c(
  "Pettersson, Elias",
  "Huberdeau, Jonathan"
)

all_labeled <- c(zegras, ntdp, draft_class, philly, ceilings)

# ============================================================
# SUBSET & ASSIGN GROUPS
# ============================================================

labeled_df <- subset(Forwards_Regression, Player %in% all_labeled)

labeled_df$group <- ifelse(labeled_df$Player %in% zegras,       "Zegras",
                           ifelse(labeled_df$Player %in% ntdp,          "NTDP Teammates",
                                  ifelse(labeled_df$Player %in% draft_class,   "Draft Class",
                                         ifelse(labeled_df$Player %in% philly,        "Philly Teammates",
                                                ifelse(labeled_df$Player %in% ceilings,      "Superstar Ceiling", "Other")))))

unlabeled_df <- subset(Forwards_Regression, !Player %in% all_labeled)

# ============================================================
# COLORS
# ============================================================

group_colors <- c(
  "Zegras"             = "#E74C3C",     # red
  "NTDP Teammates"     = "dodgerblue",
  "Draft Class"        = "#8E44AD",     # purple
  "Philly Teammates"   = "#E67E22",     # orange
  "Superstar Ceiling"  = "#2ECC71"      # green
)

# ============================================================
# PLOT
# ============================================================

ggplot() +
  
  # Background — unlabeled grey
  geom_point(data = unlabeled_df,
             aes(x = Predicted_Cap_Hit, y = Cap_Hit),
             color = "#CCCCCC", size = 2, alpha = 0.6) +
  
  # Labeled — colored by group
  geom_point(data = labeled_df,
             aes(x = Predicted_Cap_Hit, y = Cap_Hit, color = group),
             size = 3, alpha = 0.9) +
  
  # 45-degree line
  geom_abline(intercept = 0, slope = 1,
              color = "black", linewidth = 0.8, linetype = "dashed") +
  
  # Labels
  geom_text_repel(data = labeled_df,
                  aes(x = Predicted_Cap_Hit, y = Cap_Hit,
                      label = Player, color = group),
                  size = 2.6,
                  max.overlaps = 40,
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
    subtitle = "Trevor Zegras - Comparable Players Highlighted",
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

ggsave("Zegras_Actual_vs_Predicted.png",
       width = 10, height = 8, dpi = 300, bg = "white")

cat("Saved: Zegras_Actual_vs_Predicted.png\n")

# ============================================================
# ZEGRAS — Euclidean Nearest Neighbors Plot
# ============================================================
library(ggplot2)
library(ggrepel)
library(scales)

Euclidean_multi_r2 <- summary(forwards_multimodel)$r.squared

# ============================================================
# UTF-8 NAME FIX
# ============================================================

lafreniere <- Forwards_Regression$Player[grepl("Lafren", Forwards_Regression$Player)]
slafkovsky <- Forwards_Regression$Player[grepl("Slafkov", Forwards_Regression$Player)]

# ============================================================
# COMP GROUPS — full p10 list
# ============================================================

zegras <- c("Zegras, Trevor")

euclid_neighbors <- c(
  "Huberdeau, Jonathan",
  "Garland, Conor",
  "Zacha, Pavel",
  lafreniere,
  "Barbashev, Ivan",
  "Marchenko, Kirill",
  "Buchnevich, Pavel",
  "Schmaltz, Nick",
  "Pettersson, Elias",
  "Holloway, Dylan",
  "Kane, Patrick",
  "Kopitar, Anze",
  "Maccelli, Matias",
  "Cozens, Dylan",
  "Dubois, Pierre-Luc",
  "Karlsson, William",
  "Quinn, Jack",
  "Smith, Will",
  slafkovsky,
  "Geekie, Morgan",
  "Peterka, JJ",
  "Nugent-Hopkins, Ryan",
  "Bjorkstrand, Oliver",
  "Byfield, Quinton",
  "Knies, Matthew",
  "McCann, Jared",
  "Nelson, Brock",
  "Norris, Josh",
  "Giroux, Claude",
  "Schenn, Brayden",
  "Eberle, Jordan",
  "Strome, Dylan",
  "Zibanejad, Mika",
  "Lundell, Anton",
  "Beniers, Matty",
  "Monahan, Sean",
  "Cooley, Logan",
  "Kempe, Adrian"
)

# ============================================================
# SUBSET & ASSIGN GROUPS
# ============================================================

euclid_labeled_df <- subset(Forwards_Regression, Player %in% c(zegras, euclid_neighbors))

# Cap hit percentile cutoffs from full dataset
p25_cap <- 1350000
p50_cap <- 3250000
p75_cap <- 5750000

# Assign color tier based on cap hit
euclid_labeled_df$group <- ifelse(
  euclid_labeled_df$Player %in% zegras, "Zegras",
  ifelse(euclid_labeled_df$Cap_Hit < p25_cap,  "Neighbor: AAV < 25th percentile",
         ifelse(euclid_labeled_df$Cap_Hit < p50_cap,  "Neighbor: AAV between p25-p50",
                ifelse(euclid_labeled_df$Cap_Hit < p75_cap,  "Neighbor: AAV between p50-p75",
                       "Neighbor: AAV > p75")))
)

euclid_unlabeled_df <- subset(Forwards_Regression, !Player %in% c(zegras, euclid_neighbors))

# ============================================================
# COLORS
# ============================================================

euclid_group_colors <- c(
  "Zegras" = "red",
  "Neighbor: AAV < 25th percentile" = "dodgerblue",   
  "Neighbor: AAV between p25-p50" = "orange",   
  "Neighbor: AAV between p50-p75" = "purple",   
  "Neighbor: AAV > p75" = "darkgreen"    
)

# ============================================================
# PLOT
# ============================================================

ggplot() +
  
  # Background — unlabeled grey
  geom_point(data = euclid_unlabeled_df,
             aes(x = Predicted_Cap_Hit, y = Cap_Hit),
             color = "#CCCCCC", size = 2, alpha = 0.6) +
  
  # Labeled — colored by group
  geom_point(data = euclid_labeled_df,
             aes(x = Predicted_Cap_Hit, y = Cap_Hit, color = group),
             size = 3, alpha = 0.9) +
  
  # 45-degree line
  geom_abline(intercept = 0, slope = 1,
              color = "black", linewidth = 0.8, linetype = "dashed") +
  
  # Labels
  geom_text_repel(data = euclid_labeled_df,
                  aes(x = Predicted_Cap_Hit, y = Cap_Hit,
                      label = Player, color = group),
                  size = 2.6,
                  max.overlaps = 100,
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
           label = paste0("R² = ", round(Euclidean_multi_r2, 2)),
           size = 4.0, fontface = "bold", color = "gray20") +
  
  annotate("text",
           x = -Inf, y = Inf,
           hjust = 0.002, vjust = 4.0,
           label = "Cap Hit = -$6,505,372 + ($512,240 × TOI/GP) + ($1,312,925 × Total Points/60)",
           size = 3.2, fontface = "italic", color = "gray40") +
  
  scale_color_manual(
    values = euclid_group_colors,
    name = "",
    breaks = c(
      "Zegras",
      "Neighbor: AAV < 25th percentile",
      "Neighbor: AAV between p25-p50",
      "Neighbor: AAV between p50-p75",
      "Neighbor: AAV > p75"
    )
  ) +
  
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
    subtitle = "Trevor Zegras - Euclidean Nearest Neighbors Highlighted",
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

ggsave("Zegras_Euclidean_Neighbors.png",
       width = 10, height = 8, dpi = 300, bg = "white")

cat("Saved: Zegras_Euclidean_Neighbors.png\n")