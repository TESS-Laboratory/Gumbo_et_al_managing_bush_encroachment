########################################################################
# WOODY PLANT HEIGHT ANALYSIS
# Test whether treatment reduced height of seedlings, saplings, and
# trees relative to control (2024 pre-treatment vs 2026 post-treatment)
#
# Approach:
#   1. Classify individuals into size classes (Seedlings / Saplings / Trees)
#   2. Compute mean height per subplot per year
#   3. Delta height (Δ = 2026 – 2024) + log response ratio (lnRR)
#   4. LMM: Treatment * Fencing + (1|Site) (interaction model)
#   5. emmeans → % change relative to control with 95% CIs
#   6. Three-panel figure per size class: raw violin / Δ violin / forest plot
########################################################################

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(patchwork)

dir.create("Plots", showWarnings = FALSE)

#-----------------------------------------------------------------------
# 1. Load and prepare data
#-----------------------------------------------------------------------

A <- read_csv("DATA/GEODE_Subplot_area.csv")
colnames(A) <- c("Site", "Plot", "Subplot", "Area")

B <- read.csv("DATA/March2026/WOODY2426.csv", stringsAsFactors = FALSE)

woody <- B |>
  left_join(
    A |> dplyr::select(Site, Plot, Subplot, Area),
    by = c("Site", "Plot", "Subplot")
  ) |>
  filter(!is.na(Max_height.m.)) |>
  mutate(
    woody_cat = case_when(
      between(Max_height.m., 0.05, 0.50) ~ "Seedlings",
      between(Max_height.m., 0.51, 1.49) ~ "Saplings",
      Max_height.m. >= 1.5               ~ "Trees",
      TRUE                               ~ NA_character_
    ),
    Treatment = factor(Treatment),
    Fencing   = factor(Fencing, levels = c("Unfenced", "Fenced"))
  ) |>
  filter(!is.na(woody_cat), Year %in% c(2024, 2026))

#-----------------------------------------------------------------------
# 2. Helper functions
#-----------------------------------------------------------------------

prep_factors <- function(df) {
  df |>
    mutate(
      Treatment = factor(Treatment),
      Fencing   = factor(Fencing, levels = c("Unfenced", "Fenced"))
    )
}

# Mean height per subplot × year
mean_height_subplot <- function(data, cat) {
  data |>
    filter(woody_cat == cat) |>
    group_by(Site, Plot, Subplot, Treatment, Fencing, Year) |>
    summarise(mean_height = mean(Max_height.m., na.rm = TRUE),
              .groups = "drop")
}

# Delta height: 2026 – 2024
compute_delta <- function(height_df, delta_col) {
  height_df |>
    pivot_wider(names_from = Year, values_from = mean_height,
                names_prefix = "ht_") |>
    mutate({{ delta_col }} := ht_2026 - ht_2024) |>
    drop_na({{ delta_col }}) |>
    prep_factors()
}

# Log response ratio: log[(trt_post/trt_pre) / (ctrl_post/ctrl_pre)]
compute_lnRR <- function(height_df) {
  wide <- height_df |>
    pivot_wider(names_from = Year, values_from = mean_height,
                names_prefix = "ht_")

  ctrl <- wide |>
    filter(Treatment == "C") |>
    group_by(Site, Fencing) |>
    summarise(C_pre  = mean(ht_2024, na.rm = TRUE),
              C_post = mean(ht_2026, na.rm = TRUE),
              .groups = "drop")

  wide |>
    filter(Treatment != "C") |>
    left_join(ctrl, by = c("Site", "Fencing")) |>
    mutate(lnRR = log((ht_2026 / ht_2024) / (C_post / C_pre))) |>
    drop_na(lnRR) |>
    prep_factors()
}

# emmeans % change from lnRR interaction model
emm_pct_change <- function(model) {
  emmeans(model, ~ Treatment | Fencing) |>
    as.data.frame() |>
    mutate(
      pct_change   = (exp(emmean)    - 1) * 100,
      CI_lower_pct = (exp(lower.CL) - 1) * 100,
      CI_upper_pct = (exp(upper.CL) - 1) * 100,
      Treatment    = factor(Treatment, levels = c("F", "TF", "TFB", "THF")),
      Fencing      = factor(Fencing, levels = c("Unfenced", "Fenced"))
    ) |>
    mutate(across(where(is.numeric), \(x) round(x, 2)))
}

#-----------------------------------------------------------------------
# 3. Plot aesthetics
#-----------------------------------------------------------------------

cv <- c("Fenced" = "#1B5", "Unfenced" = "magenta")

panel_theme <- theme_classic() +
  theme(axis.title = element_text(size = 9),
        axis.text  = element_text(size = 9))

tag_ann <- plot_annotation(
  tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
  theme = theme(plot.tag = element_text(size = 9, hjust = 0))
)

multipanel_theme <- theme(
  axis.text  = element_text(size = 9),
  axis.title = element_text(size = 10),
  plot.tag   = element_text(size = 9, hjust = 0)
)

########################################################################
# ── SEEDLINGS (0.05 – 0.50 m) ────────────────────────────────────────
########################################################################

cat("\n===== SEEDLING HEIGHT =====\n")

seed_ht    <- mean_height_subplot(woody, "Seedlings")
seed_delta <- compute_delta(seed_ht, delta_SeedHt)
seed_lnRR  <- compute_lnRR(seed_ht)

# --- Models ---
seed_lmm_delta <- lmer(delta_SeedHt ~ Treatment * Fencing + (1|Site), data = seed_delta)
seed_lmm_lnRR  <- lmer(lnRR ~ Treatment * Fencing + (1|Site), data = seed_lnRR)

cat("\n--- Seedling Δ height model summary ---\n")
print(summary(seed_lmm_delta))

cat("\n--- Seedling lnRR model summary ---\n")
print(summary(seed_lmm_lnRR))

# Diagnostics
check_model(seed_lmm_delta, check = "qq")
check_model(seed_lmm_delta, check = "normality")
check_model(seed_lmm_delta, check = "homogeneity")

# % change from interaction lnRR model
seed_pct <- emm_pct_change(seed_lmm_lnRR)

# --- Panels ---
SeedHa <- woody |>
  filter(woody_cat == "Seedlings") |>
  mutate(Period  = factor(ifelse(Year == 2024, "Pre-treatment", "Post-treatment"),
                          levels = c("Pre-treatment", "Post-treatment")),
         Fencing = factor(Fencing, levels = c("Unfenced", "Fenced"))) |>
  ggplot(aes(x = Treatment, y = Max_height.m., fill = Fencing)) +
  facet_wrap(~Period) +
  geom_violin(trim = TRUE) +
  stat_summary(fun = mean, geom = "point",
               position = position_dodge(0.8), size = 1.4, color = "black") +
  labs(x = "Treatment", y = "Seedling height (m)") +
  scale_fill_manual(values = cv) + panel_theme

SeedHb <- ggplot(seed_delta,
                 aes(x = Treatment, y = delta_SeedHt, fill = Fencing)) +
  geom_violin(trim = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = mean, geom = "point",
               position = position_dodge(0.8), size = 1.4, color = "black") +
  labs(x = "Treatment", y = expression(Delta * " Seedling height (m)")) +
  scale_fill_manual(values = cv) + panel_theme

SeedHc <- ggplot(seed_pct,
                 aes(x = pct_change, y = Treatment, color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash",
             color = "black", linewidth = 0.5) +
  geom_point(size = 2.0, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
                 height = 0.3, linewidth = 0.8,
                 position = position_dodge(0.5)) +
  scale_color_manual(values = cv) +
  labs(x = "Change in seedling height relative to Control (%)",
       y = "Treatment") +
  panel_theme

multi_SeedH <- (SeedHa / SeedHb / SeedHc) + tag_ann & multipanel_theme

ggsave("Plots/Seedling_Height_ViolinLog.png", multi_SeedH,
       width = 16, height = 14, units = "cm", dpi = 300, bg = "white")


########################################################################
# ── SAPLINGS (0.51 – 1.49 m) ─────────────────────────────────────────
########################################################################

cat("\n===== SAPLING HEIGHT =====\n")

sap_ht    <- mean_height_subplot(woody, "Saplings")
sap_delta <- compute_delta(sap_ht, delta_SapHt)
sap_lnRR  <- compute_lnRR(sap_ht)

# --- Models ---
sap_lmm_delta <- lmer(delta_SapHt ~ Treatment * Fencing + (1|Site), data = sap_delta)
sap_lmm_lnRR  <- lmer(lnRR ~ Treatment * Fencing + (1|Site), data = sap_lnRR)

cat("\n--- Sapling Δ height model summary ---\n")
print(summary(sap_lmm_delta))

cat("\n--- Sapling lnRR model summary ---\n")
print(summary(sap_lmm_lnRR))

# Diagnostics
check_model(sap_lmm_delta, check = "qq")
check_model(sap_lmm_delta, check = "normality")
check_model(sap_lmm_delta, check = "homogeneity")

# % change from interaction lnRR model
sap_pct <- emm_pct_change(sap_lmm_lnRR)

# --- Panels ---
SapHa <- woody |>
  filter(woody_cat == "Saplings") |>
  mutate(Period  = factor(ifelse(Year == 2024, "Pre-treatment", "Post-treatment"),
                          levels = c("Pre-treatment", "Post-treatment")),
         Fencing = factor(Fencing, levels = c("Unfenced", "Fenced"))) |>
  ggplot(aes(x = Treatment, y = Max_height.m., fill = Fencing)) +
  facet_wrap(~Period) +
  geom_violin(trim = TRUE) +
  stat_summary(fun = mean, geom = "point",
               position = position_dodge(0.8), size = 1.4, color = "black") +
  labs(x = "Treatment", y = "Sapling height (m)") +
  scale_fill_manual(values = cv) + panel_theme

SapHb <- ggplot(sap_delta,
                aes(x = Treatment, y = delta_SapHt, fill = Fencing)) +
  geom_violin(trim = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = mean, geom = "point",
               position = position_dodge(0.8), size = 1.4, color = "black") +
  labs(x = "Treatment", y = expression(Delta * " Sapling height (m)")) +
  scale_fill_manual(values = cv) + panel_theme

SapHc <- ggplot(sap_pct,
                aes(x = pct_change, y = Treatment, color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash",
             color = "black", linewidth = 0.5) +
  geom_point(size = 2.0, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
                 height = 0.3, linewidth = 0.8,
                 position = position_dodge(0.5)) +
  scale_color_manual(values = cv) +
  labs(x = "Change in sapling height relative to Control (%)",
       y = "Treatment") +
  panel_theme

multi_SapH <- (SapHa / SapHb / SapHc) + tag_ann & multipanel_theme

ggsave("Plots/Sapling_Height_ViolinLog.png", multi_SapH,
       width = 16, height = 14, units = "cm", dpi = 300, bg = "white")


########################################################################
# ── TREES (≥ 1.5 m) ──────────────────────────────────────────────────
########################################################################

cat("\n===== TREE HEIGHT =====\n")

tree_ht    <- mean_height_subplot(woody, "Trees")
tree_delta <- compute_delta(tree_ht, delta_TreeHt)
tree_lnRR  <- compute_lnRR(tree_ht)

# --- Models ---
tree_lmm_delta <- lmer(delta_TreeHt ~ Treatment * Fencing + (1|Site), data = tree_delta)
tree_lmm_lnRR  <- lmer(lnRR ~ Treatment * Fencing + (1|Site), data = tree_lnRR)

cat("\n--- Tree Δ height model summary ---\n")
print(summary(tree_lmm_delta))

cat("\n--- Tree lnRR model summary ---\n")
print(summary(tree_lmm_lnRR))

# Diagnostics
check_model(tree_lmm_delta, check = "qq")
check_model(tree_lmm_delta, check = "normality")
check_model(tree_lmm_delta, check = "homogeneity")

# % change from interaction lnRR model
tree_pct <- emm_pct_change(tree_lmm_lnRR)

# --- Panels ---
TreeHa <- woody |>
  filter(woody_cat == "Trees") |>
  mutate(Period  = factor(ifelse(Year == 2024, "Pre-treatment", "Post-treatment"),
                          levels = c("Pre-treatment", "Post-treatment")),
         Fencing = factor(Fencing, levels = c("Unfenced", "Fenced"))) |>
  ggplot(aes(x = Treatment, y = Max_height.m., fill = Fencing)) +
  facet_wrap(~Period) +
  geom_violin(trim = TRUE) +
  stat_summary(fun = mean, geom = "point",
               position = position_dodge(0.8), size = 1.4, color = "black") +
  labs(x = "Treatment", y = "Tree height (m)") +
  scale_fill_manual(values = cv) + panel_theme

TreeHb <- ggplot(tree_delta,
                 aes(x = Treatment, y = delta_TreeHt, fill = Fencing)) +
  geom_violin(trim = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = mean, geom = "point",
               position = position_dodge(0.8), size = 1.4, color = "black") +
  labs(x = "Treatment", y = expression(Delta * " Tree height (m)")) +
  scale_fill_manual(values = cv) + panel_theme

TreeHc <- ggplot(tree_pct,
                 aes(x = pct_change, y = Treatment, color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash",
             color = "black", linewidth = 0.5) +
  geom_point(size = 2.0, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
                 height = 0.3, linewidth = 0.8,
                 position = position_dodge(0.5)) +
  scale_color_manual(values = cv) +
  labs(x = "Change in tree height relative to Control (%)",
       y = "Treatment") +
  panel_theme

multi_TreeH <- (TreeHa / TreeHb / TreeHc) + tag_ann & multipanel_theme

ggsave("Plots/Tree_Height_ViolinLog.png", multi_TreeH,
       width = 16, height = 14, units = "cm", dpi = 300, bg = "white")

cat("\nDone. Plots saved to Plots/\n")
