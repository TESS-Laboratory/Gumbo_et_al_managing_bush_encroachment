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


# Set "Fenced" as the reference level 
Seed_wide$Fencing <- relevel(Seed_wide$Fencing, ref = "Unfenced")


#--------------------------------------------------
# STEP 4: Mixed ANCOVA model
#--------------------------------------------------

SeedANCO <- lmer(
  Y2026 ~ Treatment * Fencing + Y2024 +(1 | Site) ,
  data = Seed_wide)


summary(SeedANCO)

# 
# model_mixed2 <- lmer(Y2026 ~ Treatment * Fencing + Y2024 +(1 | Site),
#   data = Seed_wide,control = lmerControl(autoscale = TRUE))  # Add this
# options(scipen = 10) 
# 
# summary(model_mixed2)
# # Or just for one object
# print(summary(model_mixed2), digits = 3)  # Rounds to 3 decimal places
# 
# #check for singularity
# performance::check_singularity(model_mixed2) # FALSE desired shows- all random effects have nonzero variance → stable

## Diagnostics
#code to create a Q-Q plot
qqnorm(model_mixed$residuals)
qqline(model_mixed$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))



#--------------------------------------------------
# STEP 5: Dunnett-adjusted comparisons
#--------------------------------------------------

sedaemm <- emmeans(SeedANCO, ~ Treatment | Fencing)

dunnett_results <- contrast(sedaemm, method = "dunnett")

summary( dunnett_results,infer = TRUE)

#--------------------------------------------------
# STEP 6: Create dataframe for plotting
#--------------------------------------------------

sedemm_df <- as.data.frame(sedaemm)

#--------------------------------------------------
# STEP 7: Publication-quality plot
#--------------------------------------------------
 ggplot(sedemm_df, aes(Treatment, emmean, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 2.5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.35), width = 0.12) +
  # geom_text(aes(label = Group,
  #               y = upper.CL + 0.5 * max(emmeans)),
            #position = position_dodge(width = 0.35), size = 3, color = "black") +
  scale_color_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365")) +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    y = expression(" proportional change "*ha^{-1}*"")
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8))


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