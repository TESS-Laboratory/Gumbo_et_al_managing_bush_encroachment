library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(emmeans)

#########################################################

#--------------------------------------------------
# STEP 1: Create wide dataset
#--------------------------------------------------
Seed_wide <- SeedLOG %>%
  dplyr::select(
    Site,
    Plot,
    Subplot,
    Treatment,
    Fencing,
    Year,
    Area,
    density_ha
  ) %>%
  pivot_wider(
    names_from = Year,
    values_from = density_ha,
    names_prefix = "Y"
  )

#--------------------------------------------------
# STEP 2: Convert variables to factors
#--------------------------------------------------

Seed_wide <- Seed_wide %>%
  mutate(
    Treatment = factor(Treatment),
    
    Fencing = factor(
      Fencing,
      levels = c("Fenced", "Unfenced"))
  )

#--------------------------------------------------
# STEP 3: Set control as reference
#--------------------------------------------------

Seed_wide$Treatment <- relevel(
  Seed_wide$Treatment,
  ref = "C")

#--------------------------------------------------
# STEP 4: Mixed ANCOVA model
#--------------------------------------------------

model_mixed <- lmer(
  Y2026 ~ Treatment * Fencing + Y2024 +
    (1 | Site),
  data = Seed_wide)

## Diagnostics
#code to create a Q-Q plot
qqnorm(model_mixed$residuals)
qqline(model_mixed$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))



#--------------------------------------------------
# STEP 5: Dunnett-adjusted comparisons
#--------------------------------------------------

sedaemm <- emmeans(
  model_mixed,
  ~ Treatment | Fencing)

dunnett_results <- contrast(
  sedaemm,
  method = "dunnett")

summary(
  dunnett_results,
  infer = TRUE
)

#--------------------------------------------------
# STEP 6: Create dataframe for plotting
#--------------------------------------------------

sedemm_df <- as.data.frame(sedaemm)

#--------------------------------------------------
# STEP 7: Publication-quality plot
#--------------------------------------------------

p <- ggplot(
  sedemm_df,
  aes(
    x = Treatment,
    y = emmean,
    fill = Fencing
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  
  geom_errorbar(
    aes(
      ymin = lower.CL,
      ymax = upper.CL
    ),
    position = position_dodge(width = 0.8),
    width = 0.15,
    linewidth = 0.7
  ) +
  
  labs(x = "Treatment", y = expression(Density~(ha^{-1})),
    fill = "Fencing"
  ) + theme_classic() +
  theme(plot.title = element_text(face = "bold",size = 18), 
        plot.subtitle = element_text(size = 13),
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    ),
    
    legend.position = "top",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      linewidth = 1
    )
  )

# Display plot
p

#--------------------------------------------------
# STEP 8: Save high-resolution figure
#--------------------------------------------------

ggsave(
  "ANCOVA_Kraaling_Dunnett.png",
  p,
  width = 12,
  height = 7,
  dpi = 600
)