
## Corrected % Control using Henderson Tilton method

library(dplyr)
library(tidyr)
library(robustlmm)

# Pivot years wide
Seedling2_wide <- Seed_treat %>%
  pivot_wider(
    names_from = Year,
    values_from = mean_dens_ha,
    names_glue = "dens_{Year}"
  )

#Extract control values within Site × Fencing

controlSeedl <- Seedling2_wide %>%
  filter(Treatment == "C") %>%
  dplyr::select(
    Site,
    Fencing,
    control_2024 = dens_2024,
    control_2026 = dens_2026
  )



# Join controls back and calculate Henderson–Tilton correction
Seed_Delta1 <- Seedling2_wide %>%
  
  left_join(
    controlSeedl,
    by = c("Site", "Fencing")
  ) %>%
  
  mutate(
    
    delta_seeddens = dens_2026 - dens_2024,
    
    HT_corrected = round(
      (
        1 -
          (
            (dens_2026 / dens_2024) *
              (control_2024 / control_2026)
          )
      ) * 100,
      3
    )
  )
#suppressing scientific notation
options(scipen = 999)



### LMM test using the HT_corrected

Seed_ht <- lmer(HT_corrected ~ Treatment * Fencing + (1 | Site) + (1 | Plot) + (1 | Subplot),
       data = Seed_Delta1)

summary(Seed_ht)

# # using a robust linear
# mod_robust <- rlmer(HT_corrected ~ Treatment * Fencing + (1 | Site), 
#  #                                    #+ (1 | Plot) + (1 | Subplot),
#  #                                    data = Seed_Delta1)
#  # #summary(mod_robust)


# diagnostics
plot(Seed_ht)
qqnorm(residuals(Seed_ht))
qqline(residuals(Seed_ht))


# 
########### USING EMMEANS
# Estimated marginal means for Treatment within Kraaling (if needed)
Seedhtemm <- emmeans(Seed_ht, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with Tukey adjustment (or "none" if you only want vs control)
contrast_vs_control <- contrast(Seedhtemm, method = "trt.vs.ctrl", ref = "C")
summary(contrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
Seedhtcld_emm <- cld(Seedhtemm, adjust = "Dunnett", Letters = letters, type = "response")
cldht_tbl <- as.data.frame(Seedhtcld_emm)


# prepare clean database for plotting
htplot_df <- cldht_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace



## Visualisation using ggplot
semm <- ggplot(htplot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 2.5) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.35), width = 0.12) +
  geom_text(aes(label = Group,
                y = CI_upper + 0.5 * max(EMM)),
            position = position_dodge(width = 0.35), size = 3, color = "black") +
  scale_color_manual(values = c("Fenced" = "#8c5", "Unfenced" = "#d8b")) +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    #y = "Change in seedling density per ha",
    y = expression("Control corrected; Seedling density "*ha^{-1}*"")
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8))


###################################   SAPLINGS SAPLINGS
# Pivot years wide
Sapling2_wide <- Sap_treat %>%
  pivot_wider(
    names_from = Year,
    values_from = mean_dens_ha,
    names_glue = "dens_{Year}"
  )

#Extract control values within Site × Fencing

controlSapl <- Sapling2_wide %>%
  filter(Treatment == "C") %>%
  dplyr::select(
    Site,
    Fencing,
    control_2024 = dens_2024,
    control_2026 = dens_2026
  )


# Join controls back and calculate Henderson–Tilton correction
Saplings_Delta1 <- Sapling2_wide %>%
  left_join(
    controlSapl,
    by = c("Site", "Fencing")
  ) %>%
  mutate(
    
    delta_Sapdens = dens_2026 - dens_2024,
    
    HT_corrected = round(
      (1 -
          (
            (dens_2026 / dens_2024) *
              (control_2024 / control_2026)
          )
      ) * 100,
      3
    )
  )
#suppressing scientific notation
options(scipen = 999)



### LMM test using the HT_corrected

Sap_ht <- lmer(HT_corrected ~ Treatment * Fencing + (1 | Site),
                data = Saplings_Delta1)

summary(Sap_ht)


##
# 
########### USING EMMEANS
# Estimated marginal means for Treatment within Kraaling (if needed)
Saphtemm <- emmeans(Sap_ht, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with Tukey adjustment (or "none" if you only want vs control)
contrast_vs_control <- contrast(Saphtemm, method = "trt.vs.ctrl", ref = "C")
summary(contrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
Saphtcld_emm <- cld(Saphtemm, adjust = "Dunnett", Letters = letters, type = "response")
cldht_tbl <- as.data.frame(Saphtcld_emm)


# prepare clean database for plotting
sphtplot_df <- cldht_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace



## Visualisation using ggplot
spemm <- ggplot(sphtplot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 2.5) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.35), width = 0.12) +
  geom_text(aes(label = Group,
                y = CI_upper + 0.5 * max(EMM)),
            position = position_dodge(width = 0.35), size = 3, color = "black") +
  scale_color_manual(values = c("Fenced" = "green", "Unfenced" = "magenta")) +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    y = expression("Control corrected: Sapling density "*ha^{-1}*"")
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8))





### GRASS BIOMASS

## Calculate Henderson–Tilton Corrected % Control

biomass_wide <- biomass_wide  %>%
  
  # Avoid division by zero
  mutate(
    `Pre-treatment`  = pmax(`Pre-treatment`, 1e-6),
    `Post-treatment` = pmax(`Post-treatment`, 1e-6)
  ) %>%
  
  # Join corresponding control values within Site × Fencing
  left_join(biomass_wide %>%
      filter(Treatment == "C") %>%
      dplyr::select(
        Site,
        Fencing,
        control_pre  = `Pre-treatment`,
        control_post = `Post-treatment`
      ),
    by = c("Site", "Fencing")
  ) %>%
  
  # Henderson–Tilton corrected % control
  mutate(HT_corrected = round(
    (
        1 -
          (
            (`Post-treatment` / `Pre-treatment`) *
              (control_pre / control_post)
          )
      ) * 100,
      3
    )
  )  # this formula give - values that dont depict the reality on the ground

  
  

## Second formula that focuses on increases as ideal for the experiment
  
  biomass_wide <- biomass_wide  %>%
  
  # Avoid division by zero
  mutate(
    `Pre-treatment`  = pmax(`Pre-treatment`, 1e-6),
    `Post-treatment` = pmax(`Post-treatment`, 1e-6)
  ) %>%
  left_join(biomass_wide %>%
              filter(Treatment == "C") %>%
              dplyr::select(
                Site,
                Fencing,
                control_pre  = `Pre-treatment`,
                control_post = `Post-treatment`
              ),
            by = c("Site", "Fencing")
  ) %>%
    mutate(
      Effect_size = (
        ((`Post-treatment` / `Pre-treatment`) *
           (control_pre / control_post)
        ) - 1
      ) * 100
    )



#====================================================
# Mixed model
#====================================================

bmod_ht <- lmer(
  HT_corrected ~ Treatment * Fencing +
    (1 | Site),
  data = biomass_wide
)

summary(bmod_ht)

#====================================================
# Estimated marginal means
#====================================================

emm_ht <- emmeans(
  bmod_ht,
  ~ Treatment * Fencing
)

#====================================================
# Compact letter display
#====================================================

bcld_ht <- cld(
  emm_ht,
  Letters = letters,
  adjust = "Dunnett"
)

plot_df <- bcld_ht %>%
  as.data.frame() %>%
  mutate(
    .group = gsub(" ", "", .group)
  )

#====================================================
# High-quality effect size plot
#====================================================

p <- ggplot(
  plot_df,
  aes(
    x = Treatment,
    y = emmean,
    fill = Fencing
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    colour = "black",
    linewidth = 0.3
  ) +
  
  geom_errorbar(
    aes(
      ymin = lower.CL,
      ymax = upper.CL
    ),
    position = position_dodge(width = 0.8),
    width = 0.15,
    linewidth = 0.5
  ) +
  
  geom_text(
    aes(
      label = .group,
      y = upper.CL + 5
    ),
    position = position_dodge(width = 0.8),
    size = 5,
    fontface = "bold"
  ) +
  
  labs(
    x = "Treatment",
    y = "Corrected % Control",
    fill = "Fencing"
  ) +
  
  scale_fill_manual(
    values = c(
      "Fenced"   = "#1b9e77",
      "Unfenced" = "#d95f02"
    )
  ) +
  
  theme_bw(base_size = 14) +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    legend.title = element_text(face = "bold"),
    legend.position = "top",
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 16
    )
  )

#====================================================
# Display plot
#====================================================

p

#====================================================
# Save publication-quality figure
#====================================================

ggsave(
  "HT_effect_size_plot.png",
  p,
  width = 10,
  height = 7,
  dpi = 600
)
```
