################################################################################
# HYPOTHESIS TEST: Integrated interventions reduce woody seedling density
#
# Hypothesis: Integrated interventions reduce woody seedling density relative
# to controls, with TFB in unfenced subplots exerting the strongest suppressive
# effect.
#
# Approach: Log Response Ratio (lnRR) as the response variable in a Linear
# Mixed Model (LMM). lnRR < 0 indicates suppression relative to control.
# Treatment is the fixed effect of interest; Site is the random effect
# capturing plot-level clustering.
#
# Treatment codes:
#   C   = Control (reference, absorbed into lnRR)
#   F   = Burning only
#   TF  = Integrated (Treatment + Fire)
#   TFB = Integrated (Treatment + Fire + Burning)
#   THF = Integrated (Treatment + Herbicide + Fire)
################################################################################


# --- 1. Libraries --------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(patchwork)
library(vegan)


# --- 2. Load data --------------------------------------------------------------

A <- read_csv("DATA/GEODE_Subplot_area.csv")
B <- read.csv("DATA/March2026/WOODY2426.csv", stringsAsFactors = FALSE)

colnames(A) <- c("Site", "Plot", "Subplot", "Area")

B_merged <- B |>
  dplyr::left_join(
    A |> dplyr::select(Site, Plot, Subplot, Area),
    by = c("Site", "Plot", "Subplot")
  )


# --- 3. Classify woody plants and filter seedlings ----------------------------

SapF <- B_merged |>
  mutate(
    woody_cat = case_when(
      Woody_class == "Cut stump"              ~ "Cut stump",
      between(Max_height.m., 0.05, 0.50)      ~ "Seedlings",
      between(Max_height.m., 0.51, 1.49)      ~ "Saplings",
      between(Max_height.m., 1.5,  25.0)      ~ "Trees",
      TRUE                                    ~ NA_character_
    ),
    Treatment = factor(Treatment),
    Fencing   = factor(Fencing)
  )


# --- 4. Calculate seedling density per ha -------------------------------------

SeedLOG <- SapF |>
  filter(woody_cat == "Seedlings", Year %in% c(2024, 2026)) |>
  count(Site, Plot, Subplot, Treatment, Fencing, Year, Area,
        name = "Seedlings") |>
  mutate(density_ha = Seedlings * 10000 / Area)


# --- 5. Calculate Log Response Ratio (lnRR) -----------------------------------
# lnRR = log( (treatment post/pre) / (control post/pre) )
# Computed per Site × Fencing stratum so each treatment plot is compared
# to the control plots within the same site and fencing context.

Seed_wide <- SeedLOG |>
  dplyr::select(Site, Plot, Subplot, Treatment, Fencing, Year, Area, density_ha) |>
  pivot_wider(names_from = Year, values_from = density_ha, names_prefix = "Y")

# Control means per Site × Fencing
control_means <- Seed_wide |>
  filter(Treatment == "C") |>
  group_by(Site, Fencing) |>
  summarise(
    C_pre  = mean(Y2024, na.rm = TRUE),
    C_post = mean(Y2026, na.rm = TRUE),
    .groups = "drop"
  )

# Join and compute lnRR for treated plots only
SSeed_lnRR <- Seed_wide |>
  filter(Treatment != "C") |>
  left_join(control_means, by = c("Site", "Fencing")) |>
  mutate(
    lnRR = log((Y2026 / Y2024) / (C_post / C_pre))
  ) |>
  # Set factor levels
  mutate(
    Treatment = factor(Treatment, levels = c("F", "TF", "TFB", "THF")),
    Fencing   = factor(Fencing, levels = c("Unfenced", "Fenced"))
  )

# Quick check for Inf / NaN (from zeros or missing)
SSeed_lnRR |>
  summarise(
    n_Inf = sum(is.infinite(lnRR)),
    n_NaN = sum(is.nan(lnRR)),
    n_NA  = sum(is.na(lnRR))
  )


# --- 6. Descriptive summary ---------------------------------------------------

seedling_summary <- SSeed_lnRR |>
  group_by(Treatment, Fencing) |>
  summarise(
    n        = n(),
    mean_lnRR = mean(lnRR, na.rm = TRUE),
    sd_lnRR   = sd(lnRR,   na.rm = TRUE),
    se_lnRR   = sd_lnRR / sqrt(n),
    .groups   = "drop"
  )

print(seedling_summary)


# --- 7. Fit Linear Mixed Model ------------------------------------------------
# Fixed: Treatment × Fencing interaction
# Random: Site (accounts for plot-level clustering in the split-plot design)

Seedllog <- lmer(lnRR ~ Treatment * Fencing + (1 | Site),
                 data = SSeed_lnRR)

summary(Seedllog)


# --- 8. Model diagnostics -----------------------------------------------------

performance::check_model(Seedllog, check = c("qq", "normality", "homogeneity"))

# Check for singularity and convergence
performance::check_singularity(Seedllog)
performance::check_convergence(Seedllog)


# --- 9. Hypothesis tests ------------------------------------------------------

# 9a. Does each Treatment × Fencing combination suppress seedling density?
#     One-sided test: lnRR < 0 (suppression relative to control).

emm_full <- emmeans(Seedllog, ~ Treatment * Fencing)

# Two-sided first (for full inference table)
emm_table <- summary(emm_full)
print(emm_table)

# One-sided test for suppression (H: lnRR < 0)
suppression_test <- test(emm_full, null = 0, side = "<")
print(suppression_test)


# 9b. Are integrated treatments more suppressive than burning-only (F)?
#     Planned contrast: mean(TF, TFB, THF) vs F, within each fencing level.

emm_by_fencing <- emmeans(Seedllog, ~ Treatment | Fencing)

integrated_vs_burning <- contrast(
  emm_by_fencing,
  list("Integrated vs Burning only" = c(-3, 1, 1, 1) / 3),
  # F=-3/3, TF=1/3, TFB=1/3, THF=1/3  → positive = integrated > burning
  adjust = "none"
)

# Reverse sign: negative = integrated more suppressive than burning
print(integrated_vs_burning)


# 9c. Is TFB in Unfenced subplots the most suppressive treatment?
#     Pairwise comparisons within Unfenced subplots; look for TFB < all others.

emm_unfenced <- emmeans(Seedllog, ~ Treatment, at = list(Fencing = "Unfenced"))

pairs_unfenced <- pairs(emm_unfenced, adjust = "tukey")
print(pairs_unfenced)

# Direct one-sided test: TFB Unfenced < each other treatment (Unfenced)
tfb_contrasts <- contrast(
  emm_unfenced,
  list(
    "TFB vs F"   = c(-1,  0,  1,  0),
    "TFB vs TF"  = c( 0, -1,  1,  0),
    "TFB vs THF" = c( 0,  0,  1, -1)
  ),
  side = "<",   # one-sided: TFB is more suppressive (more negative)
  adjust = "holm"
)
print(tfb_contrasts)


# 9d. Fencing effect: does fencing moderate treatment responses?
#     Test the Treatment × Fencing interaction term.

anova(Seedllog)  # Type III ANOVA table with Satterthwaite df


# --- 10. Estimated marginal means and % change --------------------------------

emm_interaction <- emmeans(Seedllog, ~ Treatment | Fencing)
emm_df <- as.data.frame(emm_interaction) |>
  mutate(
    pct_change    = (exp(emmean)     - 1) * 100,
    CI_lower_pct  = (exp(lower.CL)   - 1) * 100,
    CI_upper_pct  = (exp(upper.CL)   - 1) * 100
  ) |>
  mutate(across(where(is.numeric), round, 2))

print(emm_df)


# --- 11. Visualisation --------------------------------------------------------

# Forest-style plot: % change in seedling density relative to control
ggplot(emm_df, aes(x = pct_change, y = Treatment, color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.6) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  geom_errorbarh(
    aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
    height = 0.25, linewidth = 0.8, position = position_dodge(0.5)
  ) +
  scale_color_manual(values = c("Unfenced" = "magenta", "Fenced" = "#1B5")) +
  labs(
    x     = "Change in seedling density relative to Control (%)",
    y     = "Treatment",
    color = "Fencing"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 12),
    legend.position = "top"
  )

ggsave("Plots/Seedling_lnRR_HypothesisTest.png",
       width = 16, height = 10, units = "cm", dpi = 300, bg = "white")


# --- 9e. Fencing effect within each treatment ---------------------------------
# Test: Is there a statistical difference between Fenced and Unfenced subplots
# within each treatment? Contrasts Fenced vs Unfenced for F, TF, TFB, THF.

emm_fence_by_trt <- emmeans(Seedllog, ~ Fencing | Treatment)

fencing_within_trt <- pairs(
  emm_fence_by_trt,
  reverse = TRUE,    # Fenced - Unfenced (positive = Fenced has higher lnRR)
  adjust  = "holm"   # Holm correction across the four treatment-level tests
)

print(fencing_within_trt)

# Confidence intervals on the Fenced - Unfenced contrasts
confint(fencing_within_trt)



################################################################################
# HYPOTHESIS TEST: Integrated interventions reduce woody sapling density
#
# Hypothesis: Integrated interventions reduce woody sapling density relative
# to controls, with TFB in unfenced subplots exerting the strongest suppressive
# effect.
#
# Approach: Log Response Ratio (lnRR) as the response variable in a Linear
# Mixed Model (LMM). lnRR < 0 indicates suppression relative to control.
# Treatment is the fixed effect of interest; Site is the random effect
# capturing plot-level clustering.
################################################################################


# --- S1. Calculate sapling density per ha -------------------------------------

SapLOG <- SapF |>
  filter(woody_cat == "Saplings", Year %in% c(2024, 2026)) |>
  count(Site, Plot, Subplot, Treatment, Fencing, Year, Area,
        name = "Saplings") |>
  mutate(density_ha = Saplings * 10000 / Area)


# --- S2. Calculate Log Response Ratio (lnRR) ----------------------------------
# lnRR = log( (treatment post/pre) / (control post/pre) )
# Computed per Site × Fencing stratum.

Sap_wide <- SapLOG |>
  dplyr::select(Site, Plot, Subplot, Treatment, Fencing, Year, Area, density_ha) |>
  pivot_wider(names_from = Year, values_from = density_ha, names_prefix = "Y")

# Control means per Site × Fencing
sap_control_means <- Sap_wide |>
  filter(Treatment == "C") |>
  group_by(Site, Fencing) |>
  summarise(
    C_pre  = mean(Y2024, na.rm = TRUE),
    C_post = mean(Y2026, na.rm = TRUE),
    .groups = "drop"
  )

# Join and compute lnRR for treated plots only
SSap_lnRR <- Sap_wide |>
  filter(Treatment != "C") |>
  left_join(sap_control_means, by = c("Site", "Fencing")) |>
  mutate(
    lnRR = log((Y2026 / Y2024) / (C_post / C_pre))
  ) |>
  mutate(
    Treatment = factor(Treatment, levels = c("F", "TF", "TFB", "THF")),
    Fencing   = factor(Fencing, levels = c("Unfenced", "Fenced"))
  )

# Check for Inf / NaN (from zeros or missing)
SSap_lnRR |>
  summarise(
    n_Inf = sum(is.infinite(lnRR)),
    n_NaN = sum(is.nan(lnRR)),
    n_NA  = sum(is.na(lnRR))
  )


# --- S3. Descriptive summary --------------------------------------------------

sapling_summary <- SSap_lnRR |>
  group_by(Treatment, Fencing) |>
  summarise(
    n         = n(),
    mean_lnRR = mean(lnRR, na.rm = TRUE),
    sd_lnRR   = sd(lnRR,   na.rm = TRUE),
    se_lnRR   = sd_lnRR / sqrt(n),
    .groups   = "drop"
  )

print(sapling_summary)


# --- S4. Fit Linear Mixed Model -----------------------------------------------
# Fixed: Treatment × Fencing interaction
# Random: Site (accounts for plot-level clustering in the split-plot design)

Saplog <- lmer(lnRR ~ Treatment * Fencing + (1 | Site),
               data = SSap_lnRR)

summary(Saplog)


# --- S5. Model diagnostics ----------------------------------------------------

performance::check_model(Saplog, check = c("qq", "normality", "homogeneity"))
performance::check_singularity(Saplog)
performance::check_convergence(Saplog)


# --- S6. Hypothesis tests -----------------------------------------------------

# S6a. Does each Treatment × Fencing combination suppress sapling density?
#      One-sided test: lnRR < 0 (suppression relative to control).

emm_sap_full <- emmeans(Saplog, ~ Treatment * Fencing)

emm_sap_table <- summary(emm_sap_full)
print(emm_sap_table)

sap_suppression_test <- test(emm_sap_full, null = 0, side = "<")
print(sap_suppression_test)


# S6b. Are integrated treatments more suppressive than burning-only (F)?
#      Planned contrast: mean(TF, TFB, THF) vs F, within each fencing level.

emm_sap_by_fencing <- emmeans(Saplog, ~ Treatment | Fencing)

sap_integrated_vs_burning <- contrast(
  emm_sap_by_fencing,
  list("Integrated vs Burning only" = c(-3, 1, 1, 1) / 3),
  # F=-3/3, TF=1/3, TFB=1/3, THF=1/3  → positive = integrated > burning
  adjust = "none"
)
print(sap_integrated_vs_burning)


# S6c. Is TFB in Unfenced subplots the most suppressive treatment?
#      Direct planned contrasts: TFB vs each other treatment within Unfenced.

emm_sap_unfenced <- emmeans(Saplog, ~ Treatment, at = list(Fencing = "Unfenced"))

sap_tfb_contrasts <- contrast(
  emm_sap_unfenced,
  list(
    "TFB vs F"   = c(-1,  0,  1,  0),
    "TFB vs TF"  = c( 0, -1,  1,  0),
    "TFB vs THF" = c( 0,  0,  1, -1)
  ),
  side = "<",   # one-sided: TFB is more suppressive (more negative lnRR)
  adjust = "holm"
)
print(sap_tfb_contrasts)


# S6d. Fencing effect: does fencing moderate treatment responses?
#      Test the Treatment × Fencing interaction term.

anova(Saplog)  # Type III ANOVA table with Satterthwaite df



## --- S6e. Fencing effect within each treatment ---------------------------------
# Test: Is there a statistical difference between Fenced and Unfenced subplots
# within each treatment? Contrasts Fenced vs Unfenced for F, TF, TFB, THF.

sapemm_fence_by_trt <- emmeans(Saplog, ~ Fencing | Treatment)

fencing_within_trt <- pairs(
  sapemm_fence_by_trt,
  reverse = TRUE,    # Fenced - Unfenced (positive = Fenced has higher lnRR)
  adjust  = "holm"   # Holm correction across the four treatment-level tests
)

print(fencing_within_trt)

# Confidence intervals on the Fenced - Unfenced contrasts
confint(fencing_within_trt)


# --- S6f. Fencing effect within each treatment (back-transformed to % change) -
# exp(Fenced - Unfenced contrast) gives the ratio of Fenced to Unfenced lnRR.
# Converted to % difference: (ratio - 1) * 100.
# Positive = Fenced subplot had a higher (less suppressive) lnRR than Unfenced.

fencing_sap_bt <- as.data.frame(summary(fencing_within_trt, infer = TRUE)) |>
  mutate(
    ratio        = exp(estimate),
    pct_diff     = round((ratio - 1) * 100, 1),
    CI_lower_pct = round((exp(lower.CL) - 1) * 100, 1),
    CI_upper_pct = round((exp(upper.CL) - 1) * 100, 1)
  ) |>
  dplyr::select(contrast, Treatment, pct_diff, CI_lower_pct, CI_upper_pct, p.value)

print(fencing_sap_bt)


# --- S7. Estimated marginal means and % change --------------------------------

emm_sap_interaction <- emmeans(Saplog, ~ Treatment | Fencing)
emm_sap_df <- as.data.frame(emm_sap_interaction) |>
  mutate(
    pct_change   = (exp(emmean)   - 1) * 100,
    CI_lower_pct = (exp(lower.CL) - 1) * 100,
    CI_upper_pct = (exp(upper.CL) - 1) * 100
  ) |>
  mutate(across(where(is.numeric), round, 2))

print(emm_sap_df)


# --- S8. Visualisation --------------------------------------------------------

ggplot(emm_sap_df, aes(x = pct_change, y = Treatment, color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.6) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  geom_errorbarh(
    aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
    height = 0.25, linewidth = 0.8, position = position_dodge(0.5)
  ) +
  scale_color_manual(values = c("Unfenced" = "magenta", "Fenced" = "#1B5")) +
  labs(
    x     = "Change in sapling density relative to Control (%)",
    y     = "Treatment",
    color = "Fencing"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 12),
    legend.position = "top"
  )

ggsave("Plots/Sapling_lnRR_HypothesisTest.png",
       width = 16, height = 10, units = "cm", dpi = 300, bg = "white")


################################################################################
# HYPOTHESIS TEST: Integrated interventions increase above-ground grass biomass
#
# Hypothesis: Integrated interventions increase grass biomass relative to
# controls, with THF in fenced subplots exerting the strongest positive effect.
#
# Approach: Log Response Ratio (LRR) as the response variable in a Linear
# Mixed Model (LMM). LRR > 0 indicates increase relative to control.
# Treatment is the fixed effect of interest; Site is the random effect
# capturing plot-level clustering.
#
# Data: Gbiomass_LRR and GBiomlog are sourced from "4. W & GAnalysis .R"
# and must be available in the session before running this section.
################################################################################


# --- G1. Confirm data and set factor levels -----------------------------------

Gbiomass_LRR <- Gbiomass_LRR |>
  mutate(
    Treatment = factor(Treatment, levels = c("F", "TF", "TFB", "THF")),
    Fencing   = factor(Fencing,   levels = c("Unfenced", "Fenced"))
  )

# Check for Inf / NaN
Gbiomass_LRR |>
  summarise(
    n_Inf = sum(is.infinite(LRR)),
    n_NaN = sum(is.nan(LRR)),
    n_NA  = sum(is.na(LRR))
  )


# --- G2. Descriptive summary --------------------------------------------------

grass_biomass_summary <- Gbiomass_LRR |>
  group_by(Treatment, Fencing) |>
  summarise(
    n        = n(),
    mean_LRR = mean(LRR, na.rm = TRUE),
    sd_LRR   = sd(LRR,   na.rm = TRUE),
    se_LRR   = sd_LRR / sqrt(n),
    .groups  = "drop"
  )

print(grass_biomass_summary)


# --- G3. Model diagnostics ----------------------------------------------------

performance::check_model(GBiomlog, check = c("qq", "normality", "homogeneity"))
performance::check_singularity(GBiomlog)
performance::check_convergence(GBiomlog)


# --- G4. Hypothesis tests -----------------------------------------------------

# G4a. Does each Treatment × Fencing combination increase grass biomass?
#      One-sided test: LRR > 0 (increase relative to control).

 #emm_gb_full <- emmeans(GBiomlog, ~ Treatment * Fencing)

emm_gb_table <- summary(emm_gb_full)
print(emm_gb_table)

gb_increase_test <- test(emm_gb_full, null = 0, side = ">")
 #print(gb_increase_test)


# G4b. Are integrated treatments more effective than burning-only (F)?
#      Planned contrast: mean(TF, TFB, THF) vs F, within each fencing level.

 emm_gb_by_fencing <- emmeans(GBiomlog, ~ Treatment | Fencing)

gb_integrated_vs_burning <- contrast(
  emm_gb_by_fencing,
  list("Integrated vs Burning only" = c(-3, 1, 1, 1) / 3),
  # F=-3/3, TF=1/3, TFB=1/3, THF=1/3  → positive = integrated > burning
  adjust = "none"
)
 #print(gb_integrated_vs_burning)


# G4c. Is THF in Fenced subplots the treatment with the greatest biomass increase?
#      Direct planned contrasts: THF vs each other treatment within Fenced.

emm_gb_fenced <- emmeans(GBiomlog, ~ Treatment, at = list(Fencing = "Fenced"))

gb_thf_contrasts <- contrast(
  emm_gb_fenced,
  list(
    "THF vs F"   = c(-1,  0,  0,  1),
    "THF vs TF"  = c( 0, -1,  0,  1),
    "THF vs TFB" = c( 0,  0, -1,  1)
  ),
  side = ">",   # one-sided: THF has higher LRR (greater biomass increase)
  adjust = "holm"
)
print(gb_thf_contrasts)


# G4d. Fencing effect: does fencing moderate treatment responses?
#      Test the Treatment × Fencing interaction term.

  #anova(GBiomlog)  # Type III ANOVA table with Satterthwaite df


# G4e. Fencing effect within each treatment ------------------------------------
# Contrasts Fenced vs Unfenced for F, TF, TFB, THF on the LRR scale.

emm_gb_fence_by_trt <- emmeans(GBiomlog, ~ Fencing | Treatment)

fencing_gb_within_trt <- pairs(
  emm_gb_fence_by_trt,
  reverse = TRUE,    # Fenced - Unfenced (positive = Fenced has higher LRR)
  adjust  = "holm"
)

print(fencing_gb_within_trt)
confint(fencing_gb_within_trt)


# G4f. Fencing effect within each treatment (back-transformed to % change) -----
# exp(Fenced - Unfenced contrast) gives ratio of Fenced to Unfenced effect.
# Positive pct_diff = Fenced subplot had a greater biomass increase than Unfenced.

fencing_gb_bt <- as.data.frame(summary(fencing_gb_within_trt, infer = TRUE)) |>
  mutate(
    ratio        = exp(estimate),
    pct_diff     = round((ratio - 1) * 100, 1),
    CI_lower_pct = round((exp(lower.CL) - 1) * 100, 1),
    CI_upper_pct = round((exp(upper.CL) - 1) * 100, 1)
  ) |>
  dplyr::select(contrast, Treatment, pct_diff, CI_lower_pct, CI_upper_pct, p.value)

print(fencing_gb_bt)


# --- G5. Estimated marginal means and % change --------------------------------

emm_gb_interaction <- emmeans(GBiomlog, ~ Treatment | Fencing)
emm_gb_df <- as.data.frame(emm_gb_interaction) |>
  mutate(
    pct_change   = (exp(emmean)   - 1) * 100,
    CI_lower_pct = (exp(lower.CL) - 1) * 100,
    CI_upper_pct = (exp(upper.CL) - 1) * 100
  ) |>
  mutate(across(where(is.numeric), round, 2))

print(emm_gb_df)


# --- G6. Visualisation --------------------------------------------------------

ggplot(emm_gb_df, aes(x = pct_change, y = Treatment, color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.6) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  geom_errorbarh(
    aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
    height = 0.25, linewidth = 0.8, position = position_dodge(0.5)
  ) +
  scale_color_manual(values = c("Unfenced" = "magenta", "Fenced" = "#1B5")) +
  labs(
    x     = "Change in grass biomass relative to Control (%)",
    y     = "Treatment",
    color = "Fencing"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 12),
    legend.position = "top"
  )

ggsave("Plots/GrassBiomass_LRR_HypothesisTest.png",
       width = 16, height = 10, units = "cm", dpi = 300, bg = "white")


################################################################################
# HYPOTHESIS TEST: Integrated interventions increase grass species richness
#
# Hypothesis: Integrated interventions increase grass species richness relative
# to controls, with TFB in fenced subplots exerting the strongest positive effect.
#
# Approach: Log Response Ratio (LRR) as the response variable in a Linear
# Mixed Model (LMM). LRR > 0 indicates increase relative to control.
# Treatment is the fixed effect of interest; Site is the random effect.
#
# Data: Built from Grasses (loaded via W & GAnalysis script).
################################################################################


# --- R1. Calculate grass species richness LRR ---------------------------------

GRich_lnRR <- Grasses |>
  filter(!is.na(Species_name), Year %in% c(2024, 2026)) |>
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) |>
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop") |>
  mutate(Treatment_Group = ifelse(Treatment == "C", "C", "Treated")) |>
  pivot_wider(
    names_from  = Year,
    values_from = spp_richness,
    names_prefix = "Year_"
  ) |>
  rename(Pre = Year_2024, Post = Year_2026) |>
  group_by(Site, Fencing) |>
  mutate(
    Control_Pre  = mean(Pre[Treatment_Group == "C"],  na.rm = TRUE),
    Control_Post = mean(Post[Treatment_Group == "C"], na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(Treatment_Group == "Treated") |>
  mutate(
    GrLRR = log((Post / Pre) / (Control_Post / Control_Pre)),
    Treatment = factor(Treatment, levels = c("F", "TF", "TFB", "THF")),
    Fencing   = factor(Fencing,   levels = c("Unfenced", "Fenced"))
  ) |>
  dplyr::select(Site, Plot, Subplot, Treatment, Fencing, Pre, Post, GrLRR)

# Check for Inf / NaN
GRich_lnRR |>
  summarise(
    n_Inf = sum(is.infinite(GrLRR)),
    n_NaN = sum(is.nan(GrLRR)),
    n_NA  = sum(is.na(GrLRR))
  )


# --- R2. Descriptive summary --------------------------------------------------

grass_rich_summary <- GRich_lnRR |>
  group_by(Treatment, Fencing) |>
  summarise(
    n        = n(),
    mean_LRR = mean(GrLRR, na.rm = TRUE),
    sd_LRR   = sd(GrLRR,   na.rm = TRUE),
    se_LRR   = sd_LRR / sqrt(n),
    .groups  = "drop"
  )

print(grass_rich_summary)


# --- R3. Fit Linear Mixed Model -----------------------------------------------

GRiclog <- lmer(GrLRR ~ Treatment * Fencing + (1 | Site),
                data = GRich_lnRR)

summary(GRiclog)


# --- R4. Model diagnostics ----------------------------------------------------

performance::check_model(GRiclog, check = c("qq", "normality", "homogeneity"))
performance::check_singularity(GRiclog)
performance::check_convergence(GRiclog)


# --- R5. Hypothesis tests -----------------------------------------------------

# R5a. Does each Treatment × Fencing combination increase grass richness?
#      One-sided test: LRR > 0 (increase relative to control).

 #emm_gr_full <- emmeans(GRiclog, ~ Treatment * Fencing)

emm_gr_table <- summary(emm_gr_full)
 #print(emm_gr_table)

gr_increase_test <- test(emm_gr_full, null = 0, side = ">")
print(gr_increase_test)


# R5b. Are integrated treatments more effective than burning-only (F)?
#      Planned contrast: mean(TF, TFB, THF) vs F, within each fencing level.

 #emm_gr_by_fencing <- emmeans(GRiclog, ~ Treatment | Fencing)

gr_integrated_vs_burning <- contrast(
  emm_gr_by_fencing,
  list("Integrated vs Burning only" = c(-3, 1, 1, 1) / 3),
  adjust = "none"
)
 #print(gr_integrated_vs_burning)


# R5c. Is TFB in Fenced subplots the treatment with the greatest richness increase?
#      Direct planned contrasts: TFB vs each other treatment within Fenced.

emm_gr_fenced <- emmeans(GRiclog, ~ Treatment, at = list(Fencing = "Fenced"))

gr_tfb_contrasts <- contrast(
  emm_gr_fenced,
  list(
    "TFB vs F"   = c(-1,  0,  1,  0),
    "TFB vs TF"  = c( 0, -1,  1,  0),
    "TFB vs THF" = c( 0,  0,  1, -1)
  ),
  side = ">",   # one-sided: TFB has higher LRR (greater richness increase)
  adjust = "holm"
)
print(gr_tfb_contrasts)


# R5d. Fencing effect: does fencing moderate treatment responses?

 #anova(GRiclog)


# R5e. Fencing effect within each treatment ------------------------------------
# Contrasts Fenced vs Unfenced for F, TF, TFB, THF on the GrLRR scale.

emm_gr_fence_by_trt <- emmeans(GRiclog, ~ Fencing | Treatment)

fencing_gr_within_trt <- pairs(
  emm_gr_fence_by_trt,
  reverse = TRUE,    # Fenced - Unfenced (positive = Fenced has higher GrLRR)
  adjust  = "holm"
)

print(fencing_gr_within_trt)
confint(fencing_gr_within_trt)


# R5f. Fencing effect within each treatment (back-transformed to % change) -----
# Positive pct_diff = Fenced subplot had a greater richness increase than Unfenced.

fencing_gr_bt <- as.data.frame(summary(fencing_gr_within_trt, infer = TRUE)) |>
  mutate(
    ratio        = exp(estimate),
    pct_diff     = round((ratio - 1) * 100, 1),
    CI_lower_pct = round((exp(lower.CL) - 1) * 100, 1),
    CI_upper_pct = round((exp(upper.CL) - 1) * 100, 1)
  ) |>
  dplyr::select(contrast, Treatment, pct_diff, CI_lower_pct, CI_upper_pct, p.value)

print(fencing_gr_bt)


# --- R6. Estimated marginal means and % change --------------------------------

emm_gr_interaction <- emmeans(GRiclog, ~ Treatment | Fencing)
emm_gr_df <- as.data.frame(emm_gr_interaction) |>
  mutate(
    pct_change   = (exp(emmean)   - 1) * 100,
    CI_lower_pct = (exp(lower.CL) - 1) * 100,
    CI_upper_pct = (exp(upper.CL) - 1) * 100
  ) |>
  mutate(across(where(is.numeric), round, 2))

print(emm_gr_df)


# --- R7. Visualisation --------------------------------------------------------

ggplot(emm_gr_df, aes(x = pct_change, y = Treatment, color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.6) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  geom_errorbarh(
    aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
    height = 0.25, linewidth = 0.8, position = position_dodge(0.5)
  ) +
  scale_color_manual(values = c("Unfenced" = "magenta", "Fenced" = "#1B5")) +
  labs(
    x     = "Change in grass species richness relative to Control (%)",
    y     = "Treatment",
    color = "Fencing"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 12),
    legend.position = "top"
  )

ggsave("Plots/GrassRichness_LRR_HypothesisTest.png",
       width = 16, height = 10, units = "cm", dpi = 300, bg = "white")


################################################################################
# HYPOTHESIS TEST: Integrated interventions increase grass diversity (Shannon)
#
# Hypothesis: Integrated interventions increase grass Shannon diversity relative
# to controls, with TFB in fenced subplots exerting the strongest positive effect.
#
# Approach: Log Response Ratio (LRR) on Shannon index as the response variable
# in a Linear Mixed Model (LMM). LRR > 0 indicates increase relative to control.
# Treatment is the fixed effect of interest; Site is the random effect.
#
# Data: Built from Grasses (loaded via W & GAnalysis script).
# Requires vegan package for Shannon diversity calculation.
################################################################################



# --- D1. Calculate Shannon diversity LRR --------------------------------------

# Step 1: Species-level abundance per subplot × year
subplot_abundance <- Grasses |>
  filter(!is.na(Species_name), Year %in% c(2024, 2026)) |>
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year, Species_name) |>
  summarise(total_abundance = sum(Number, na.rm = TRUE), .groups = "drop")

# Step 2: Shannon index per subplot × year
sw_diversity <- subplot_abundance |>
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) |>
  summarise(
    Shannon = diversity(total_abundance, index = "shannon"),
    .groups = "drop"
  )

# Step 3: Compute LRR
GDiv_lnRR <- sw_diversity |>
  mutate(Treatment_Group = ifelse(Treatment == "C", "C", "Treated")) |>
  pivot_wider(
    names_from  = Year,
    values_from = Shannon,
    names_prefix = "Year_"
  ) |>
  rename(Pre = Year_2024, Post = Year_2026) |>
  group_by(Site, Fencing) |>
  mutate(
    Control_Pre  = mean(Pre[Treatment_Group == "C"],  na.rm = TRUE),
    Control_Post = mean(Post[Treatment_Group == "C"], na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(Treatment_Group == "Treated") |>
  mutate(
    # Pseudocount of 0.01 added to avoid log(0) when Shannon = 0 (single-species subplots)
         GdLRR     = log(((Post + 0.01) / (Pre + 0.01)) / ((Control_Post + 0.01) / (Control_Pre + 0.01))),
    Treatment = factor(Treatment, levels = c("F", "TF", "TFB", "THF")),
    Fencing   = factor(Fencing,   levels = c("Unfenced", "Fenced"))
  ) |>
  dplyr::select(Site, Plot, Subplot, Treatment, Fencing, Pre, Post, GdLRR)

# Check for Inf / NaN
GDiv_lnRR |>
  summarise(
    n_Inf = sum(is.infinite(GdLRR)),
    n_NaN = sum(is.nan(GdLRR)),
    n_NA  = sum(is.na(GdLRR))
  )


# --- D2. Descriptive summary --------------------------------------------------

grass_div_summary <- GDiv_lnRR |>
  group_by(Treatment, Fencing) |>
  summarise(
    n        = n(),
    mean_LRR = mean(GdLRR, na.rm = TRUE),
    sd_LRR   = sd(GdLRR,   na.rm = TRUE),
    se_LRR   = sd_LRR / sqrt(n),
    .groups  = "drop"
  )

#print(grass_div_summary)


# --- D3. Fit Linear Mixed Model -----------------------------------------------

GDivlog <- lmer(GdLRR ~ Treatment * Fencing + (1 | Site),
                data = GDiv_lnRR)

summary(GDivlog)


# --- D4. Model diagnostics ----------------------------------------------------

performance::check_model(GDivlog, check = c("qq", "normality", "homogeneity"))
performance::check_singularity(GDivlog)
performance::check_convergence(GDivlog)


# --- D5. Hypothesis tests -----------------------------------------------------

# D5a. Does each Treatment × Fencing combination increase grass diversity?
#      One-sided test: LRR > 0 (increase relative to control).

 #emm_gd_full <- emmeans(GDivlog, ~ Treatment * Fencing)

emm_gd_table <- summary(emm_gd_full)
print(emm_gd_table)

gd_increase_test <- test(emm_gd_full, null = 0, side = ">")
 #print(gd_increase_test)


# D5b. Are integrated treatments more effective than burning-only (F)?
#      Planned contrast: mean(TF, TFB, THF) vs F, within each fencing level.

 #emm_gd_by_fencing <- emmeans(GDivlog, ~ Treatment | Fencing)

gd_integrated_vs_burning <- contrast(
  emm_gd_by_fencing,
  list("Integrated vs Burning only" = c(-3, 1, 1, 1) / 3),
  adjust = "none"
)
 #print(gd_integrated_vs_burning)


# D5c. Is TFB in Fenced subplots the treatment with the greatest diversity increase?
#      Direct planned contrasts: TFB vs each other treatment within Fenced.

emm_gd_fenced <- emmeans(GDivlog, ~ Treatment, at = list(Fencing = "Fenced"))

gd_tfb_contrasts <- contrast(
  emm_gd_fenced,
  list(
    "TFB vs F"   = c(-1,  0,  1,  0),
    "TFB vs TF"  = c( 0, -1,  1,  0),
    "TFB vs THF" = c( 0,  0,  1, -1)
  ),
  side = ">",   # one-sided: TFB has higher LRR (greater diversity increase)
  adjust = "holm"
)
print(gd_tfb_contrasts)


# D5d. Fencing effect: does fencing moderate treatment responses?

anova(GDivlog)


# D5e. Fencing effect within each treatment ------------------------------------
# Contrasts Fenced vs Unfenced for F, TF, TFB, THF on the GdLRR scale.

emm_gd_fence_by_trt <- emmeans(GDivlog, ~ Fencing | Treatment)

fencing_gd_within_trt <- pairs(
  emm_gd_fence_by_trt,
  reverse = TRUE,    # Fenced - Unfenced (positive = Fenced has higher GdLRR)
  adjust  = "holm"
)

print(fencing_gd_within_trt)
confint(fencing_gd_within_trt)


# D5f. Fencing effect within each treatment (back-transformed to % change) -----
# Positive pct_diff = Fenced subplot had a greater diversity increase than Unfenced.

fencing_gd_bt <- as.data.frame(summary(fencing_gd_within_trt, infer = TRUE)) |>
  mutate(
    ratio        = exp(estimate),
    pct_diff     = round((ratio - 1) * 100, 1),
    CI_lower_pct = round((exp(lower.CL) - 1) * 100, 1),
    CI_upper_pct = round((exp(upper.CL) - 1) * 100, 1)
  ) |>
  dplyr::select(contrast, Treatment, pct_diff, CI_lower_pct, CI_upper_pct, p.value)

print(fencing_gd_bt)


# --- D6. Estimated marginal means and % change --------------------------------

emm_gd_interaction <- emmeans(GDivlog, ~ Treatment | Fencing)
emm_gd_df <- as.data.frame(emm_gd_interaction) |>
  mutate(
    pct_change   = (exp(emmean)   - 1) * 100,
    CI_lower_pct = (exp(lower.CL) - 1) * 100,
    CI_upper_pct = (exp(upper.CL) - 1) * 100
  ) |>
  mutate(across(where(is.numeric), round, 2))

print(emm_gd_df)


# --- D7. Visualisation --------------------------------------------------------

ggplot(emm_gd_df, aes(x = pct_change, y = Treatment, color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.6) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  geom_errorbarh(
    aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
    height = 0.25, linewidth = 0.8, position = position_dodge(0.5)
  ) +
  scale_color_manual(values = c("Unfenced" = "magenta", "Fenced" = "#1B5")) +
  labs(
    x     = "Change in grass Shannon diversity relative to Control (%)",
    y     = "Treatment",
    color = "Fencing"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 12),
    legend.position = "top"
  )

ggsave("Plots/GrassDiversity_LRR_HypothesisTest.png",
       width = 16, height = 10, units = "cm", dpi = 300, bg = "white")


################################################################################
# HYPOTHESIS TEST: Integrated interventions reduce tree density
#
# Hypothesis: Integrated interventions reduce tree density relative to controls.
# Approach: Log Response Ratio (lnRR) as the response variable in a Linear
# Mixed Model (LMM). lnRR < 0 indicates suppression relative to control.
################################################################################


# --- T1. Calculate tree density per ha ----------------------------------------

TreeLOG <- SapF |>
  filter(woody_cat == "Trees", Year %in% c(2024, 2026)) |>
  count(Site, Plot, Subplot, Treatment, Fencing, Year, Area,
        name = "Trees") |>
  mutate(density_ha = Trees * 10000 / Area)


# --- T2. Calculate Log Response Ratio (lnRR) ----------------------------------

Tree_wide <- TreeLOG |>
  dplyr::select(Site, Plot, Subplot, Treatment, Fencing, Year, Area, density_ha) |>
  pivot_wider(names_from = Year, values_from = density_ha, names_prefix = "Y")

tree_control_means <- Tree_wide |>
  filter(Treatment == "C") |>
  group_by(Site, Fencing) |>
  summarise(
    C_pre  = mean(Y2024, na.rm = TRUE),
    C_post = mean(Y2026, na.rm = TRUE),
    .groups = "drop"
  )

STree_lnRR <- Tree_wide |>
  filter(Treatment != "C") |>
  left_join(tree_control_means, by = c("Site", "Fencing")) |>
  mutate(
    lnRR = log((Y2026 / Y2024) / (C_post / C_pre)),
    Treatment = factor(Treatment, levels = c("F", "TF", "TFB", "THF")),
    Fencing   = factor(Fencing,   levels = c("Unfenced", "Fenced"))
  )

# Check for Inf / NaN (zeros in numerator or denominator will produce these)
STree_lnRR |>
  summarise(
    n_Inf = sum(is.infinite(lnRR)),
    n_NaN = sum(is.nan(lnRR)),
    n_NA  = sum(is.na(lnRR))
  )

# Remove non-finite rows before modelling
STree_lnRR <- STree_lnRR |> filter(is.finite(lnRR))


# --- T3. Descriptive summary --------------------------------------------------

tree_summary <- STree_lnRR |>
  group_by(Treatment, Fencing) |>
  summarise(
    n         = n(),
    mean_lnRR = mean(lnRR, na.rm = TRUE),
    sd_lnRR   = sd(lnRR,   na.rm = TRUE),
    se_lnRR   = sd_lnRR / sqrt(n),
    .groups   = "drop"
  )

print(tree_summary)


# --- T4. Fit Linear Mixed Model -----------------------------------------------

Treelog <- lmer(lnRR ~ Treatment * Fencing + (1 | Site),
                data = STree_lnRR)

summary(Treelog)


# --- T5. Model diagnostics ----------------------------------------------------

performance::check_model(Treelog, check = c("qq", "normality", "homogeneity"))
performance::check_singularity(Treelog)
performance::check_convergence(Treelog)


# --- T6. Hypothesis tests -----------------------------------------------------

# T6a. Does each Treatment × Fencing combination suppress tree density?
emm_tree_full <- emmeans(Treelog, ~ Treatment * Fencing)
tree_suppression_test <- test(emm_tree_full, null = 0, side = "<")
print(tree_suppression_test)

# T6b. Pairwise comparisons within each fencing level
emm_tree_by_fencing <- emmeans(Treelog, ~ Treatment | Fencing)
print(pairs(emm_tree_by_fencing, adjust = "tukey"))

# T6c. Fencing × Treatment interaction
anova(Treelog)


# T6d. Fencing effect within each treatment ------------------------------------
# Contrasts Fenced vs Unfenced for F, TF, TFB, THF on the lnRR scale.

emm_tree_fence_by_trt <- emmeans(Treelog, ~ Fencing | Treatment)

fencing_tree_within_trt <- pairs(
  emm_tree_fence_by_trt,
  reverse = TRUE,    # Fenced - Unfenced (positive = Fenced has higher lnRR)
  adjust  = "holm"
)

print(fencing_tree_within_trt)
confint(fencing_tree_within_trt)


# T6e. Fencing effect within each treatment (back-transformed to % change) -----
# Positive pct_diff = Fenced subplot had a less suppressive effect than Unfenced.

fencing_tree_bt <- as.data.frame(summary(fencing_tree_within_trt, infer = TRUE)) |>
  mutate(
    ratio        = exp(estimate),
    pct_diff     = round((ratio - 1) * 100, 1),
    CI_lower_pct = round((exp(lower.CL) - 1) * 100, 1),
    CI_upper_pct = round((exp(upper.CL) - 1) * 100, 1)
  ) |>
  dplyr::select(contrast, Treatment, pct_diff, CI_lower_pct, CI_upper_pct, p.value)

print(fencing_tree_bt)


# --- T7. Estimated marginal means and % change --------------------------------

emm_tree_df <- as.data.frame(emmeans(Treelog, ~ Treatment | Fencing)) |>
  mutate(
    pct_change   = (exp(emmean)   - 1) * 100,
    CI_lower_pct = (exp(lower.CL) - 1) * 100,
    CI_upper_pct = (exp(upper.CL) - 1) * 100
  ) |>
  mutate(across(where(is.numeric), round, 2))

print(emm_tree_df)


# --- T8. Visualisation --------------------------------------------------------

ggplot(emm_tree_df, aes(x = pct_change, y = Treatment, color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.6) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  geom_errorbarh(
    aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
    height = 0.25, linewidth = 0.8, position = position_dodge(0.5)
  ) +
  scale_color_manual(values = c("Unfenced" = "magenta", "Fenced" = "#1B5")) +
  labs(
    x     = "Change in tree density relative to Control (%)",
    y     = "Treatment",
    color = "Fencing"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 12),
    legend.position = "top"
  )

ggsave("Plots/TreeDensity_LRR_HypothesisTest.png",
       width = 16, height = 10, units = "cm", dpi = 300, bg = "white")


################################################################################
# COMPOSITE PERFORMANCE RANKING: Which treatment achieved the greatest yield?
#
# Combines estimated marginal mean % changes from all six outcome models
# (tree, seedling, sapling density; grass biomass, richness, diversity).
# Direction-corrected so that positive always = beneficial, then z-score
# standardised within each metric (equal weighting) before summing into a
# composite score. Higher composite score = better overall performance.
################################################################################


# --- P1. Assemble direction-corrected % changes from all six models -----------
# Suppression metrics (trees, seedlings, saplings): flip sign so positive = good
# Enhancement metrics (biomass, richness, diversity): keep sign

make_metric <- function(df, metric, type) {
  df |>
    dplyr::select(Treatment, Fencing, pct_change) |>
    mutate(
      Metric = metric,
      Type   = type,
      Score  = if (type == "suppress") -pct_change else pct_change
    )
}

composite_long <- bind_rows(
  make_metric(emm_tree_df, "Tree density",     "suppress"),
  make_metric(emm_df,      "Seedling density", "suppress"),
  make_metric(emm_sap_df,  "Sapling density",  "suppress"),
  make_metric(emm_gb_df,   "Grass biomass",    "enhance"),
  make_metric(emm_gr_df,   "Grass richness",   "enhance"),
  make_metric(emm_gd_df,   "Grass diversity",  "enhance")
) |>
  mutate(
    Metric = factor(Metric, levels = c(
      "Tree density", "Seedling density", "Sapling density",
      "Grass biomass", "Grass richness", "Grass diversity"
    ))
  )


# --- P2. Standardise within metric and compute composite score ----------------
# Z-score within each Metric so all outcomes contribute equally regardless of scale.

composite_scored <- composite_long |>
  group_by(Metric) |>
  mutate(z_score = (Score - mean(Score, na.rm = TRUE)) / sd(Score, na.rm = TRUE)) |>
  ungroup()

composite_summary <- composite_scored |>
  group_by(Treatment, Fencing) |>
  summarise(
    composite_z      = round(sum(z_score, na.rm = TRUE), 2),
    n_metrics_positive = sum(z_score > 0, na.rm = TRUE),
    .groups          = "drop"
  ) |>
  arrange(Fencing, desc(composite_z)) |>
  group_by(Fencing) |>
  mutate(rank = row_number()) |>
  ungroup()

print(composite_summary)


# --- P3. Heatmap: direction-corrected % change per Treatment × Metric ---------

ggplot(
  composite_long |>
    mutate(trt_fence = paste(Treatment, Fencing, sep = " / "),
           label     = round(Score, 1)),
  aes(x = Metric, y = trt_fence, fill = Score)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.2) +
  scale_fill_gradient2(
    low      = "#d73027",
    mid      = "white",
    high     = "#1a9641",
    midpoint = 0,
    name     = "Direction-corrected\n% change"
  ) +
  labs(
    x        = NULL,
    y        = "Treatment / Fencing",
    title    = "Treatment performance across all outcome metrics",
    subtitle = "Green = beneficial; Red = detrimental (suppression metrics sign-flipped)"
  ) +
  theme_classic() +
  theme(
    axis.text.x     = element_text(angle = 35, hjust = 1, size = 11),
    axis.text.y     = element_text(size = 11),
    axis.title.y    = element_text(size = 12),
    plot.title      = element_text(size = 13, face = "bold"),
    plot.subtitle   = element_text(size = 10),
    legend.position = "right"
  )

ggsave("Plots/Composite_Performance_Heatmap.png",
       width = 24, height = 14, units = "cm", dpi = 300, bg = "white")


# --- P4. Composite score barplot by Fencing -----------------------------------

ggplot(composite_summary, aes(x = composite_z, y = Treatment, fill = Fencing)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.6) +
  facet_wrap(~ Fencing, ncol = 1) +
  scale_fill_manual(values = c("Unfenced" = "magenta", "Fenced" = "#1B5")) +
  labs(
    x        = "Composite z-score (sum across 6 metrics, equally weighted)",
    y        = "Treatment",
    title    = "Overall treatment performance ranking",
    subtitle = "Higher score = greater combined yield improvement"
  ) +
  theme_classic() +
  theme(
    axis.title    = element_text(size = 12),
    axis.text     = element_text(size = 12),
    strip.text    = element_text(size = 12, face = "bold"),
    plot.title    = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "none"
  )

ggsave("Plots/Composite_Performance_Ranking.png",
       width = 18, height = 14, units = "cm", dpi = 300, bg = "white")


################################################################################
# TREATMENT RANKING PER VARIABLE
#
# For each outcome, treatments are ranked from most to least effective within
# each fencing level, based on the model-estimated % change (pct_change).
#
# Effectiveness direction:
#   Suppression metrics (seedlings, saplings, trees):
#     Most negative pct_change = Rank 1 (greatest reduction relative to control)
#   Enhancement metrics (grass biomass, richness, diversity):
#     Most positive pct_change = Rank 1 (greatest increase relative to control)
#
# Pairwise comparisons (Tukey-adjusted) follow each ranking to indicate which
# differences are statistically distinguishable.
################################################################################


# Helper: rank treatments from most to least effective within each Fencing level
# suppress = TRUE  -> lower pct_change is better (suppression metrics)
# suppress = FALSE -> higher pct_change is better (enhancement metrics)
rank_treatments <- function(emm_df_in, variable_label, suppress = FALSE) {
  emm_df_in |>
    group_by(Fencing) |>
    mutate(
      Rank = if (suppress) rank(pct_change, ties.method = "min")
             else          rank(-pct_change, ties.method = "min")
    ) |>
    ungroup() |>
    arrange(Fencing, Rank) |>
    mutate(Variable = variable_label) |>
    dplyr::select(Variable, Fencing, Rank, Treatment, pct_change, CI_lower_pct, CI_upper_pct, emmean)
}


# --- Grass Biomass (enhancement: higher pct_change = more effective) ----------

cat("\n================================================================================\n")
cat("  GRASS BIOMASS — Treatment ranking (most to least effective)\n")
cat("================================================================================\n")
rank_treatments(emm_gb_df, "Grass biomass", suppress = FALSE) |> print(n = Inf)
cat("\nPairwise comparisons (Tukey) within each Fencing level:\n")
print(pairs(emmeans(GBiomlog, ~ Treatment | Fencing), adjust = "tukey"))


# --- Seedling Density (suppression: lower pct_change = more effective) --------

cat("\n================================================================================\n")
cat("  SEEDLING DENSITY — Treatment ranking (most to least effective)\n")
cat("================================================================================\n")
rank_treatments(emm_df, "Seedling density", suppress = TRUE) |> print(n = Inf)
cat("\nPairwise comparisons (Tukey) within each Fencing level:\n")
print(pairs(emmeans(Seedllog, ~ Treatment | Fencing), adjust = "tukey"))


# --- Sapling Density (suppression: lower pct_change = more effective) ---------

cat("\n================================================================================\n")
cat("  SAPLING DENSITY — Treatment ranking (most to least effective)\n")
cat("================================================================================\n")
rank_treatments(emm_sap_df, "Sapling density", suppress = TRUE) |> print(n = Inf)
cat("\nPairwise comparisons (Tukey) within each Fencing level:\n")
print(pairs(emmeans(Saplog, ~ Treatment | Fencing), adjust = "tukey"))


# --- Tree Density (suppression: lower pct_change = more effective) ------------

cat("\n================================================================================\n")
cat("  TREE DENSITY — Treatment ranking (most to least effective)\n")
cat("================================================================================\n")
rank_treatments(emm_tree_df, "Tree density", suppress = TRUE) |> print(n = Inf)
cat("\nPairwise comparisons (Tukey) within each Fencing level:\n")
print(pairs(emmeans(Treelog, ~ Treatment | Fencing), adjust = "tukey"))


# --- Grass Richness (enhancement: higher pct_change = more effective) ---------

cat("\n================================================================================\n")
cat("  GRASS RICHNESS — Treatment ranking (most to least effective)\n")
cat("================================================================================\n")
rank_treatments(emm_gr_df, "Grass richness", suppress = FALSE) |> print(n = Inf)
cat("\nPairwise comparisons (Tukey) within each Fencing level:\n")
print(pairs(emmeans(GRiclog, ~ Treatment | Fencing), adjust = "tukey"))


# --- Grass Diversity / Shannon (enhancement: higher pct_change = more effective)

cat("\n================================================================================\n")
cat("  GRASS DIVERSITY (Shannon) — Treatment ranking (most to least effective)\n")
cat("================================================================================\n")
rank_treatments(emm_gd_df, "Grass diversity", suppress = FALSE) |> print(n = Inf)
cat("\nPairwise comparisons (Tukey) within each Fencing level:\n")
print(pairs(emmeans(GDivlog, ~ Treatment | Fencing), adjust = "tukey"))
