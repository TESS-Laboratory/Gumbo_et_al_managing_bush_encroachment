########################################################################
## Grass Biomass ~ Woody Plant Density Scatter Plots
## Relationship between Δ grass biomass and Δ woody density
## (Seedlings, Saplings, Trees) faceted by Fencing status
## Date: June 2026
########################################################################

library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)

# ── 1. Load raw data ──────────────────────────────────────────────────

A            <- read_csv("DATA/GEODE_Subplot_area.csv", show_col_types = FALSE)
B            <- read.csv("DATA/March2026/WOODY2426.csv", stringsAsFactors = FALSE)
heights_data <- read_csv("DATA/March2026/2Grasses2426.csv", show_col_types = FALSE)

colnames(A) <- c("Site", "Plot", "Subplot", "Area")

# ── 2. Classify woody plants by size class ────────────────────────────

B_merged <- B |>
  dplyr::left_join(A |> dplyr::select(Site, Plot, Subplot, Area),
                   by = c("Site", "Plot", "Subplot"))

SapF <- B_merged |>
  mutate(
    woody_cat = case_when(
      Woody_class == "Cut stump"         ~ "Cut stump",
      between(Max_height.m., 0.05, 0.50) ~ "Seedlings",
      between(Max_height.m., 0.51, 1.49) ~ "Saplings",
      between(Max_height.m., 1.5, 25.0)  ~ "Trees",
      TRUE                               ~ NA_character_
    ),
    Treatment = factor(Treatment),
    Fencing   = factor(Fencing)
  )

# ── 3. Compute Δ seedling density ─────────────────────────────────────

Seedlings <- SapF |>
  filter(woody_cat == "Seedlings", Year %in% c(2024, 2026)) |>
  count(Site, Plot, Subplot, Treatment, Fencing, Year, Area,
        name = "Seedlings") |>
  mutate(density_ha = Seedlings * 10000 / Area)

Seedlings_Delta1 <- Seedlings |>
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) |>
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop") |>
  pivot_wider(names_from = Year, values_from = mean_dens_ha,
              names_glue = "dens_{Year}") |>
  mutate(delta_Seeddens = dens_2026 - dens_2024,
         Treatment = as.factor(Treatment),
         Fencing   = factor(Fencing, levels = c("Unfenced", "Fenced")))

# ── 4. Compute Δ sapling density ──────────────────────────────────────

Saplings <- SapF |>
  filter(woody_cat == "Saplings", Year %in% c(2024, 2026)) |>
  count(Site, Plot, Subplot, Treatment, Fencing, Year, Area,
        name = "Saplings") |>
  mutate(density_ha = Saplings * 10000 / Area)

Saplings_Delta1 <- Saplings |>
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) |>
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop") |>
  pivot_wider(names_from = Year, values_from = mean_dens_ha,
              names_glue = "dens_{Year}") |>
  mutate(delta_Sapdens = dens_2026 - dens_2024,
         Treatment = as.factor(Treatment),
         Fencing   = factor(Fencing, levels = c("Unfenced", "Fenced")))

# ── 5. Compute Δ tree density ─────────────────────────────────────────

Trees <- SapF |>
  filter(woody_cat == "Trees", Year %in% c(2024, 2026)) |>
  count(Site, Plot, Subplot, Treatment, Fencing, Year, Area,
        name = "Trees") |>
  mutate(density_ha = Trees * 10000 / Area)

Trees_Delta1 <- Trees |>
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) |>
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop") |>
  pivot_wider(names_from = Year, values_from = mean_dens_ha,
              names_glue = "dens_{Year}") |>
  mutate(delta_Treedens = dens_2026 - dens_2024,
         Treatment = as.factor(Treatment),
         Fencing   = factor(Fencing, levels = c("Unfenced", "Fenced")))

# ── 6. Compute Δ grass biomass ────────────────────────────────────────

# Log-linear allometric equation: log(biomass) = 4.67 + 1.14 * log(DPM height)
INTERCEPT <- 4.67
SLOPE     <- 1.14

predict_biomass <- function(h) {
  h <- pmax(h, 1e-6)
  exp(INTERCEPT + SLOPE * log(h))
}

heights_data$Biomass_kg_ha <- round(predict_biomass(heights_data$DPM_Height), 2)

delta_GRBiomass <- heights_data |>
  filter(Year %in% c(2024, 2026)) |>
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) |>
  summarise(mean_Biomass = mean(Biomass_kg_ha, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Year, values_from = mean_Biomass,
              names_glue = "Biomass_{Year}") |>
  mutate(delta_GR  = Biomass_2026 - Biomass_2024,
         Treatment = as.factor(Treatment),
         Fencing   = factor(Fencing, levels = c("Unfenced", "Fenced"))) |>
  drop_na(delta_GR)

# ── 7. Join Δ grass biomass with each size class ──────────────────────

gb <- delta_GRBiomass |>
  dplyr::select(Site, Plot, Subplot, Treatment, Fencing, delta_GR)

scatter_data <- Seedlings_Delta1 |>
  inner_join(gb, by = c("Site", "Plot", "Subplot", "Treatment", "Fencing"))

sap_scatter  <- Saplings_Delta1 |>
  inner_join(gb, by = c("Site", "Plot", "Subplot", "Treatment", "Fencing"))

tree_scatter <- Trees_Delta1 |>
  inner_join(gb, by = c("Site", "Plot", "Subplot", "Treatment", "Fencing"))

# ── 8. Scatter plots (faceted by Fencing, with trend line) ───────────

plot_seed <- ggplot(scatter_data,
                    aes(x = delta_GR, y = delta_Seeddens,
                        color = Treatment, shape = Treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_smooth(aes(group = Fencing), method = "lm", se = TRUE,
              color = "black", fill = "grey80", linewidth = 0.8) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~Fencing) +
  labs(
    x = expression("Δ Grass biomass (kg "*ha^{-1}*")"),
    y = expression("Δ Seedling density "*ha^{-1}),
    color = "Treatment", shape = "Treatment"
  ) +
  theme_classic() +
  theme(axis.title   = element_text(size = 11),
        axis.text    = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text  = element_text(size = 9),
        strip.text   = element_text(size = 11))

plot_sap <- ggplot(sap_scatter,
                   aes(x = delta_GR, y = delta_Sapdens,
                       color = Treatment, shape = Treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_smooth(aes(group = Fencing), method = "lm", se = TRUE,
              color = "black", fill = "grey80", linewidth = 0.8) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~Fencing) +
  labs(
    x = expression("Δ Grass biomass (kg "*ha^{-1}*")"),
    y = expression("Δ Sapling density "*ha^{-1}),
    color = "Treatment", shape = "Treatment"
  ) +
  theme_classic() +
  theme(axis.title   = element_text(size = 11),
        axis.text    = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text  = element_text(size = 9),
        strip.text   = element_text(size = 11))

plot_tree <- ggplot(tree_scatter,
                    aes(x = delta_GR, y = delta_Treedens,
                        color = Treatment, shape = Treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_smooth(aes(group = Fencing), method = "lm", se = TRUE,
              color = "black", fill = "grey80", linewidth = 0.8) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~Fencing) +
  labs(
    x = expression("Δ Grass biomass (kg "*ha^{-1}*")"),
    y = expression("Δ Tree density "*ha^{-1}),
    color = "Treatment", shape = "Treatment"
  ) +
  theme_classic() +
  theme(axis.title   = element_text(size = 11),
        axis.text    = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text  = element_text(size = 9),
        strip.text   = element_text(size = 11))

# ── 9. Save plots ─────────────────────────────────────────────────────

ggsave(plot_seed, filename = "Plots/GrassBiomass_SeedlingDensity_Scatter.png",
       width = 16, height = 14, units = "cm", dpi = 300, bg = "white")

ggsave(plot_sap,  filename = "Plots/GrassBiomass_SaplingDensity_Scatter.png",
       width = 16, height = 14, units = "cm", dpi = 300, bg = "white")

ggsave(plot_tree, filename = "Plots/GrassBiomass_TreeDensity_Scatter.png",
       width = 16, height = 14, units = "cm", dpi = 300, bg = "white")

# ── 10. Mixed-effects model: does the slope differ by Fencing? ────────
# (Demonstrated for seedlings; repeat for saplings/trees as needed)

mod_slopes <- lmer(delta_Seeddens ~ delta_GR * Fencing + (1 | Site),
                   data = scatter_data)

summary(mod_slopes)

# Key result:
#   delta_GR (main effect)          → overall negative slope, p < 0.001
#   delta_GR:FencingFenced          → slope difference Fenced vs Unfenced, p = 0.497
#   Conclusion: slopes do not significantly differ between fencing groups;
#   the negative grass–seedling relationship holds across both.
