library(dplyr)
library(ggplot2)
library(readr)
library(magick)
library(cowplot)

# ============================================================
# LOAD DATA — run once, reuse for all players
# ============================================================

Centers_Z_Cards <- Forwards_zScores_Master

# ============================================================
# PLAYER CONFIG — only edit this block per player
# ============================================================

player_name     <- "Celebrini, Macklin"
player_team     <- "SJS"
player_pos      <- "C"
player_number   <- 71
player_age      <- 19
player_shot     <- "L"
player_height   <- "6'0\""
player_weight   <- 190
player_aav      <- "$975K"
player_contract <- "ELC expires 2027 (RFA)"
headshot_path   <- "/Users/iandenning/Downloads/Celebrini_Macklin.png"

# Flex slot 1 — deployment / usage context
flex1_col       <- "Off_ZoneStart_pct_percentile"
flex1_label     <- "Off. Zone Start %"
flex1_raw_col   <- "Off_ZoneStart_pct"
flex1_raw_fmt   <- function(x) paste0(round(x, 1), "%")
flex1_lower_better <- FALSE

# Flex slot 2 — centers: faceoffs | wingers: swap as needed
flex2_col       <- "iFaceoffs_Won_per60_percentile"
flex2_label     <- "Faceoffs Won/60"
flex2_raw_col   <- "iFaceoffs_Won_per60"
flex2_raw_fmt   <- function(x) as.character(round(x, 1))
flex2_lower_better <- FALSE

# ============================================================
# PULL PLAYER ROW
# ============================================================

p_row <- Centers_Z_Cards %>% filter(Player == player_name)

if (nrow(p_row) == 0) stop(paste("Player not found:", player_name))

gp      <- p_row$GP
cap_hit <- player_aav  # use manually set formatted value

# ============================================================
# HELPER — pull percentile and raw value, format bar color
# ============================================================

get_pct <- function(col) as.numeric(p_row[[col]])

bar_color <- function(pct, lower_better = FALSE) {
  if (lower_better) {
    if (pct <= 33) return("#C0392B")   # elite (low is good)
    if (pct <= 66) return("#95A5A6")   # average
    return("#2980B9")                   # below average (high is bad)
  } else {
    if (pct >= 67) return("#C0392B")   # elite
    if (pct >= 34) return("#95A5A6")   # average
    return("#2980B9")                   # below average
  }
}

fmt_raw <- function(col, decimals = 2, suffix = "") {
  val <- as.numeric(p_row[[col]])
  paste0(round(val, decimals), suffix)
}

fmt_toi <- function(col) {
  val <- as.numeric(p_row[[col]])
  mins <- floor(val)
  secs <- round((val - mins) * 60)
  sprintf("%d:%02d", mins, secs)
}

# ============================================================
# BUILD ROWS TRIBBLE
# ============================================================

# Pull all percentiles
onIce_pos_pct  <- get_pct("F_onIce_Positive_Composite_Percentile (Higher = Better)")
onIce_neg_pct  <- get_pct("F_onIce_Negative_Composite_Percentile (Higher = Worse)")
irates_pct     <- get_pct("F_iRates_Positive_Composite_Percentile (Higher = Better)")
entries_pct    <- get_pct("F_EntriesFor_Positive_Composite_Percentile (Higher = Better)")

toi_pct        <- get_pct("TOI_perGP_percentile")
cf_pct_pct     <- get_pct("CF_pct_percentile")
gf_pct         <- get_pct("GF_per60_percentile")
scf_pct_pct    <- get_pct("SCF_pct_percentile")

pts_pct        <- get_pct("iTotal_Points_per60_percentile")
a1_pct         <- get_pct("iFirst_Assists_per60_percentile")
ixg_pct        <- get_pct("ixG_per60_percentile")
icf_pct        <- get_pct("iCF_per60_percentile")

carries_pct    <- get_pct("Carries_per60_percentile")
ewc_pct        <- get_pct("Entries_wChances_per60_percentile")

flex1_pct      <- get_pct(flex1_col)
flex2_pct      <- get_pct(flex2_col)

# Composite z-scores for raw label
onIce_pos_z  <- round(as.numeric(p_row[["F_onIce_Positive_Composite_zScore"]]), 2)
onIce_neg_z  <- round(as.numeric(p_row[["F_onIce_Negative_Composite_zScore"]]), 2)
irates_z     <- round(as.numeric(p_row[["F_iRates_Positive_Composite_zScore"]]), 2)
entries_z    <- round(as.numeric(p_row[["F_EntriesFor_Positive_Composite_zScore"]]), 2)

fmt_z <- function(z) ifelse(z >= 0, paste0("z = +", z), paste0("z = ", z))

rows <- tribble(
  ~y,  ~type,    ~label,                        ~pct,          ~raw_label,                                       ~bar_color,          ~lower_better,
  
  # COMPOSITES
  28,  "header", "COMPOSITES",                  NA,            "",                                               NA,                  FALSE,
  27,  "metric", "On-Ice Positive Composite",   onIce_pos_pct, fmt_z(onIce_pos_z),                              bar_color(onIce_pos_pct), FALSE,
  26,  "metric", "On-Ice Negative Composite",   onIce_neg_pct, paste0(fmt_z(onIce_neg_z), " ↓"),               bar_color(onIce_neg_pct, TRUE), TRUE,
  25,  "metric", "Indiv. Rates Composite",      irates_pct,    fmt_z(irates_z),                                 bar_color(irates_pct), FALSE,
  24,  "metric", "Zone Entries Composite",      entries_pct,   fmt_z(entries_z),                                bar_color(entries_pct), FALSE,
  
  # ON-ICE
  22,  "header", "ON-ICE",                      NA,            "",                                               NA,                  FALSE,
  21,  "metric", "TOI per Game",                toi_pct,       fmt_toi("TOI_perGP"),                            bar_color(toi_pct),  FALSE,
  20,  "metric", "Corsi For %",                 cf_pct_pct,    fmt_raw("CF_pct", 1, "%"),                       bar_color(cf_pct_pct), FALSE,
  19,  "metric", "Goals For/60",                gf_pct,        fmt_raw("GF_per60", 2),                          bar_color(gf_pct),   FALSE,
  18,  "metric", "Scoring Chance For %",        scf_pct_pct,   fmt_raw("SCF_pct", 1, "%"),                      bar_color(scf_pct_pct), FALSE,
  
  # iRATES
  16,  "header", "INDIVIDUAL RATES",            NA,            "",                                               NA,                  FALSE,
  15,  "metric", "Total Points/60",             pts_pct,       fmt_raw("iTotal_Points_per60", 2),               bar_color(pts_pct),  FALSE,
  14,  "metric", "First Assists/60",            a1_pct,        fmt_raw("iFirst_Assists_per60", 2),              bar_color(a1_pct),   FALSE,
  13,  "metric", "Ind. Expected Goals/60",      ixg_pct,       fmt_raw("ixG_per60", 2),                         bar_color(ixg_pct),  FALSE,
  12,  "metric", "Ind. Corsi For/60",           icf_pct,       fmt_raw("iCF_per60", 1),                         bar_color(icf_pct),  FALSE,
  
  # ZONE ENTRIES
  10,  "header", "ZONE ENTRIES",                NA,            "",                                               NA,                  FALSE,
  9,  "metric", "Carries/60",                  carries_pct,   fmt_raw("Carries_per60", 1),                     bar_color(carries_pct), FALSE,
  8,  "metric", "Entries w/ Chances/60",       ewc_pct,       fmt_raw("Entries_wChances_per60", 2),            bar_color(ewc_pct),  FALSE,
  
  # FLEX SLOTS
  6,  "header", "FLEX SLOTS",                        NA,            "",                                               NA,                  FALSE,
  5,  "metric", flex1_label,                   flex1_pct,     flex1_raw_fmt(as.numeric(p_row[[flex1_raw_col]])), bar_color(flex1_pct, flex1_lower_better), flex1_lower_better,
  4,  "metric", flex2_label,                   flex2_pct,     flex2_raw_fmt(as.numeric(p_row[[flex2_raw_col]])), bar_color(flex2_pct, flex2_lower_better), flex2_lower_better
)

metrics <- rows %>% filter(type == "metric")
headers <- rows %>% filter(type == "header")

# ============================================================
# BUILD PLOT
# ============================================================

p <- ggplot() +
  
  # Header bands
  geom_rect(data = headers,
            aes(xmin = -35, xmax = 115, ymin = y - 0.45, ymax = y + 0.45),
            fill = "#1C1C1C") +
  
  geom_text(data = headers,
            aes(x = -34, y = y, label = label),
            hjust = 0, vjust = 0.5, size = 3.2,
            fontface = "bold", color = "white") +
  
  # Grey track
  geom_segment(data = metrics,
               aes(x = 0, xend = 100, y = y, yend = y),
               color = "#E0E0E0", linewidth = 4, lineend = "round") +
  
  # Colored bar
  geom_segment(data = metrics,
               aes(x = 0, xend = pct, y = y, yend = y, color = bar_color),
               linewidth = 4, lineend = "round") +
  
  # Circle
  geom_point(data = metrics,
             aes(x = pct, y = y, fill = bar_color),
             shape = 21, size = 5.2, color = "white", stroke = 0.8) +
  
  # Number in circle
  geom_text(data = metrics,
            aes(x = pct, y = y, label = round(pct)),
            size = 2.3, fontface = "bold", color = "white") +
  
  # Metric label (left)
  geom_text(data = metrics,
            aes(x = -2.3, y = y, label = label),
            hjust = 1, vjust = 0.5, size = 2.9, color = "#333333") +
  
  # Raw value (right)
  geom_text(data = metrics,
            aes(x = 102, y = y, label = raw_label),
            hjust = 0, vjust = 0.5, size = 2.8, color = "#666666") +
  
  # POOR / AVERAGE / GREAT legend
  annotate("text", x =   0, y = 2.6, label = "POOR",    size = 2.6, color = "#2980B9", fontface = "bold", hjust = 0.5) +
  annotate("text", x =  50, y = 2.6, label = "AVERAGE", size = 2.6, color = "#888888", fontface = "bold", hjust = 0.5) +
  annotate("text", x = 100, y = 2.6, label = "GREAT",   size = 2.6, color = "#C0392B", fontface = "bold", hjust = 0.5) +
  
  scale_color_identity() +
  scale_fill_identity() +
  scale_x_continuous(limits = c(-35, 118), expand = c(0, 0)) +
  scale_y_continuous(limits = c(2.0, 29.5), expand = c(0, 0)) +
  
  labs(
    title    = paste0(player_name, "  |  ", player_team, "  | ", player_pos,
                      "  |  #", player_number, "  |  Age: ", player_age,
                      "  |  Shot: ", player_shot,
                      "  |  ", player_height, "  |  ", player_weight, " lbs"),
    subtitle = paste0(cap_hit, "  |  ", player_contract,
                      "  | GP: ", gp, " |  2023-2026"),
    caption  = "CAA Hockey Analytics  •  Ian Denning  •  ↓ = lower is better for this metric"
  ) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F7F7F7", color = NA),
    plot.title      = element_text(size = 11, face = "bold",   color = "#1C1C1C",
                                   margin = margin(t = 12, b = 3, l = 2)),
    plot.subtitle   = element_text(size = 8.5, face = "italic", color = "#777777",
                                   margin = margin(b = 8, l = 2)),
    plot.caption    = element_text(size = 7, color = "#AAAAAA",
                                   margin = margin(t = 6, b = 8)),
    plot.margin     = margin(10, 15, 8, 15)
  )

# ============================================================
# HEADSHOT — add if file exists
# ============================================================

if (file.exists(headshot_path)) {
  headshot <- image_read(headshot_path) %>%
    image_scale("200x200") %>%
    image_crop("200x160+0+0") %>%
    image_background("none")
  
  final_plot <- ggdraw(p) +
    draw_image(headshot,
               x = 0.82, y = 1.007,
               width = 0.16, height = 0.16,
               hjust = 0, vjust = 1)
} else {
  final_plot <- p
  message("Headshot not found — skipping image. Place file at: ", headshot_path)
}

# ============================================================
# SAVE
# ============================================================

output_file <- paste0(gsub(", ", "_", player_name), "_Percentile_Card.png")

ggsave(output_file,
       plot = final_plot,
       width = 7, height = 9, dpi = 300, bg = "#F7F7F7")

cat("Saved:", output_file, "\n")