library(tidyverse)
library(vegan)
library(multcompView)
library(patchwork)
library(lmerTest)
library(Matrix)
library(lme4)
library(emmeans)
library(here) ## MOST IMPORTANT NOT TO FORGET THIS ONE in quarto
library(robustbase)
library(sjPlot)
library(flextable)
library(officer)
library(glmmTMB)
library(DHARMa)   # model diagnostics GLMM
library(performance)  # model diagnostics
library(car)
library(openxlsx)
library(broom.mixed) #convert model objects to data frames
library(corrplot)
library(robustlmm) # for robust regression analysis
library(boot)  # For bootsrapping
library(DescTools)
library(effectsize) # For easy centering
library(marginaleffects)
library(effects)
library(ggeffects)
library(knitr)  # for table
library(gtsummary)
library(MASS)
library(multcomp) # for compact letters
library(sf)#for 

#create theme beautiful
theme_beautiful <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 8, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 8,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 8, color = "black"),
      legend.title = element_text(size = 8, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
}

# load data

calibration_data<- read_csv("DATA/March2025/GrassHeight.csv")


#### testing which model has best fit 
# Fit both models to your calibration data
sqrt_data <- calibration_data %>%
  mutate(
    sqrt_Biomass = sqrt(Biomass),
    sqrt_Height = sqrt(DPM_Height)
  )

# Free intercept model
model_free <- lm(sqrt_Biomass ~ sqrt_Height, data = sqrt_data)

# Zero intercept model  
model_zero <- lm(sqrt_Biomass ~ 0 + sqrt_Height, data = sqrt_data)  # 0+ forces intercept=0

# Compare
cat("Free intercept model:\n")
print(summary(model_free))

cat("\nZero intercept model:\n")
print(summary(model_zero))

# AIC comparison
cat(sprintf("\nAIC Free: %.1f\n", AIC(model_free)))
cat(sprintf("AIC Zero: %.1f\n", AIC(model_zero))) 
     ##Result show that model free has a better fit(low AIC = 21743) than zero intercept


##Check R² Comparison:
# Get R² for both
r2_free <- summary(model_free)$r.squared
r2_zero <- summary(model_zero)$r.squared

cat(sprintf("Free intercept R²: %.4f\n", r2_free))
cat(sprintf("Zero intercept R²: %.4f\n", r2_zero))
cat(sprintf("Difference: %.4f\n", r2_free - r2_zero))


#### Compare residuals
residuals_comparison <- data.frame(
  Height = sqrt_data$DPM_Height,
  Residual_Free = residuals(model_free),
  Residual_Zero = residuals(model_zero)
)

# Plot residuals
p1 <- ggplot(residuals_comparison, aes(x = Height, y = Residual_Free)) +
  geom_point(color = "red", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Free Intercept Residuals", y = "Residuals") +
  theme_minimal()

p2 <- ggplot(residuals_comparison, aes(x = Height, y = Residual_Zero)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Zero Intercept Residuals", y = "Residuals") +
  theme_minimal()

# Look at residual sums of squares
cat("\nResidual Sum of Squares:\n")
cat(sprintf("Free intercept: %.2f\n", sum(residuals_comparison$Residual_Free^2)))
cat(sprintf("Zero intercept: %.2f\n", sum(residuals_comparison$Residual_Zero^2)))
cat(sprintf("Ratio (Zero/Free): %.2f\n", 
            sum(residuals_comparison$Residual_Zero^2) / 
              sum(residuals_comparison$Residual_Free^2)))



#### COMPARRING RMSE

# COMPLETE RMSE ANALYSIS FOR THE MODEL
# =======================================


# Your model coefficients
INTERCEPT <- -14.9520
SLOPE <- 16.7123

# 1. PREDICTIONS WITH YOUR MODEL
predict_your_model <- function(DPM_Height) {
  sqrt_pred <- INTERCEPT + SLOPE * sqrt(DPM_Height)
  sqrt_pred <- pmax(0, sqrt_pred)
  return(sqrt_pred^2)
}

calibration_data$Predicted_kg_ha <- predict_your_model(calibration_data$DPM_Height)

# 2. CALCULATE RMSE
rmse_your_model <- sqrt(mean((calibration_data$Biomass - 
                                calibration_data$Predicted_kg_ha)^2))


#
calibration_data$sqrt_Biomass <- sqrt(calibration_data$Biomass)
calibration_data$sqrt_Height <- sqrt(calibration_data$DPM_Height)


# 3. COMPARE WITH FITTED MODELS
model_free <- lm(sqrt_Biomass ~ sqrt_Height, data = calibration_data)
model_zero <- lm(sqrt_Biomass ~ 0 + sqrt_Height, data = calibration_data)

# Predictions
pred_free <- predict(model_free)
pred_zero <- predict(model_zero)

# RMSE in sqrt-space
rmse_free_sqrt <- sqrt(mean((calibration_data$sqrt_Biomass - pred_free)^2))
rmse_zero_sqrt <- sqrt(mean((calibration_data$sqrt_Biomass - pred_zero)^2))

# RMSE in kg/ha-space
pred_free_kg_ha <- pred_free^2
pred_zero_kg_ha <- pred_zero^2
rmse_free_kg_ha <- sqrt(mean((calibration_data$Biomass - pred_free_kg_ha)^2))
rmse_zero_kg_ha <- sqrt(mean((calibration_data$Biomass - pred_zero_kg_ha)^2))

# 4. PRINT RESULTS
cat("RMSE ANALYSIS RESULTS\n")
cat("=====================\n\n")

cat("YOUR SPECIFIC MODEL (with given coefficients):\n")
cat(sprintf("  sqrt(B) = %.4f + %.4f × sqrt(H)\n", INTERCEPT, SLOPE))
cat(sprintf("  RMSE: %.1f kg/ha\n\n", rmse_your_model))

cat("FITTED FREE INTERCEPT MODEL:\n")
cat(sprintf("  sqrt(B) = %.4f + %.4f × sqrt(H)\n", 
            coef(model_free)[1], coef(model_free)[2]))
cat(sprintf("  RMSE (sqrt-space): %.3f sqrt(kg/ha)\n", rmse_free_sqrt))
cat(sprintf("  RMSE (kg/ha): %.1f kg/ha\n\n", rmse_free_kg_ha))

cat("ZERO INTERCEPT MODEL:\n")
cat(sprintf("  sqrt(B) = %.4f × sqrt(H)\n", coef(model_zero)[1]))
cat(sprintf("  RMSE (sqrt-space): %.3f sqrt(kg/ha)\n", rmse_zero_sqrt))
cat(sprintf("  RMSE (kg/ha): %.1f kg/ha\n\n", rmse_zero_kg_ha))

cat("COMPARISON:\n")
cat(sprintf("  Zero model RMSE is %.1f%% higher in sqrt-space\n", 
            (rmse_zero_sqrt/rmse_free_sqrt - 1)*100))
cat(sprintf("  Zero model RMSE is %.1f%% higher in kg/ha\n",
            (rmse_zero_kg_ha/rmse_free_kg_ha - 1)*100))

# 5. PERCENT ERROR ANALYSIS
calibration_data <- calibration_data %>%
  mutate(
    Error_kg_ha = Biomass - Predicted_kg_ha,
    Percent_Error = abs(Error_kg_ha) / Biomass * 100
  )

cat(sprintf("\nAverage absolute percent error: %.1f%%\n", 
            mean(calibration_data$Percent_Error, na.rm = TRUE)))
cat(sprintf("Median absolute percent error: %.1f%%\n", 
            median(calibration_data$Percent_Error, na.rm = TRUE)))




###################
##################################### PREDICTING BIOMASS FROM DPM HEIGHT

# Load data
heights_data <-  read_csv("DATA/March2025/GrassHeight.csv")

# defining intercept and slope
INTERCEPT <- -14.9520
SLOPE <- 16.7123

# Ensure height column
if (!"DPM_Height" %in% names(heights_data)) {
  heights_data$DPM_Height <- heights_data[[names(heights_data)[1]]]
  cat("Using first column as Height_cm\n")
}

# Predict biomass
predict_biomass <- function(h) {
  sqrt_b <- INTERCEPT + SLOPE * sqrt(h)
  sqrt_b <- pmax(0, sqrt_b)
  return(sqrt_b^2)
}

heights_data$Biomass_kg_ha <- predict_biomass(heights_data$DPM_Height)

# Save
 #write.csv(heights_data, "biomass_predictions.csv", row.names = FALSE)



########### determining Biomass

Biomasssummary<- heights_data %>%
  filter(!is.na(Biomass_kg_ha),
         Year %in% c(2024, 2025),!Treatment %in% c("TFB")) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_Biomass = mean(Biomass_kg_ha, na.rm = TRUE)) %>%
  ungroup() 

### Convert character variables to factors
heights_data$Treatment <- as.factor(heights_data$Treatment)
heights_data$Fencing <- as.factor(heights_data$Fencing)

# Calculate mean grass BIOMASS for pre and post treatment
biomass2 <- heights_data %>%
  filter(!is.na(Biomass_kg_ha),
         Year %in% c(2024, 2025),!Treatment %in% c("TFB"))%>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_Biomass = mean(Biomass_kg_ha, na.rm = TRUE),
            #n_observations = n(),
            .groups = "drop") %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

##reorder so that pre-treatment appears first then post treatment second on the plots
grass_biom <- biomass2 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))



## VIOLINplot- GRASS biomass post vs pre treatment excluding TFB
Gbimviolin<- ggplot(grass_biom, 
                  aes(x = Treatment, y = mean_Biomass, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_violin(trim = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") + 
  labs(x = "Treatment", 
       #y = "Above-ground grass biomass (kgDM/ha)",
       y = expression("Above-ground grass biomass ("*kg~ha^{-1}*")")
  ) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 9),      # Axis titles
    axis.text = element_text(size = 9))+
  scale_fill_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))


########DELTA GRASS BIOMASS

# Take the mean height within each grouping for each year before differencing.
delta_Gbiomass <- heights_data  %>%
  filter(Year %in% c(2024, 2025),
         !Treatment %in% c("TFB")) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_Biomass = mean(Biomass_kg_ha, na.rm = TRUE), .groups = "drop_last") %>%
  pivot_wider(
    names_from  = Year,
    values_from = mean_Biomass,
    names_glue  = "biomass_{Year}"
  ) %>%
  mutate(delta_BiomR = biomass_2025 - biomass_2024) %>%
  drop_na(delta_BiomR)   # keep groups where both years are present


#Visualise: violin plot of Δ‑grass height. FONT INCREASED
Grasbiom2 <- ggplot(delta_Gbiomass,
                  aes(x = Treatment, y = delta_BiomR, fill = Fencing)) + 
  geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in grass biomass ("*kg~ha^{-1}*")") +
  theme_classic() +
  scale_fill_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365"))

##saving pre& post treatment VIOLINPLOT  -  excluding encroachment level
#ggsave(Grasbx2,filename ="Plots/3 TFB Delta Grass height BOXplot.png",
width = 16, height = 14, units = "cm") 

###### GLMM to test effect of treatment * fencing on Grass biomass ##################

# Make "Unfenced" the reference level 

class(delta_Gbiomass$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
delta_Gbiomass$Fencing <- factor(delta_Gbiomass$Fencing, ordered = FALSE)

# Verify
levels(delta_Gbiomass$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
delta_Gbiomass$Fencing <- relevel(delta_Gbiomass$Fencing, ref = "Unfenced")

### Convert character variables to factors
delta_Gbiomass$Treatment <- as.factor(delta_Gbiomass$Treatment)
delta_Gbiomass$Fencing <- as.factor(delta_Gbiomass$Fencing)


##LMM for grass biomass
Grasbiom1 <- lmer(delta_BiomR ~ Treatment * Fencing + (1|Site),  
                data = delta_Gbiomass)
summary(Grasbiom1)


# check model convergence
performance::check_convergence(Grasbiom1)

## check model performance

performance::check_model(Grasbiom1)

#check for singularity
performance::check_singularity(Grasbiom1) # FALSE desired shows- all random effects have nonzero variance → stable

### POST HOC ANALYSIS 
# Tukey HSD pairwise comparisons
grbiom_comparisons <- emmeans(Grasbiom1, specs = pairwise ~ Treatment | Fencing, adjust = "Dunnett")
summary(grbiom_comparisons$contrasts)


##Marginal effects grass biomass
Gbiomass <- ggpredict(Grasbiom1, terms = c("Treatment", "Fencing"))
gbt2 <-plot(Gbiomass, colors = c( "#d8b365", "#8c510a")) + 
  labs(y = "Change in grass biomass ("*kg~ha^{-1}*")",
       x = "Treatment") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )



######## EMM FOR GRASS BIOMASS
# Estimated marginal means for Treatment within Fencing - GRASS BIOMASS
Biomemm <- emmeans(Grasbiom1, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with Tukey adjustment (or "none" if you only want vs control)
Biocontrast_vs_control <- contrast(Biomemm, method = "trt.vs.ctrl", ref = "C")
summary(Biocontrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
Biomcld_emm <- cld(Biomemm, adjust = "Dunnett", Letters = letters, type = "response")
cld_tbl <- as.data.frame(Biomcld_emm)


# prepare clean database for plotting
Bplot_df <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace



## Visualisation using ggplot for Grass biomass
Bioemm <- ggplot(Bplot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.35), width = 0.12) +
  geom_text(aes(label = Group,
                y = CI_upper + 0.5 * max(EMM)),
            position = position_dodge(width = 0.35), size = 3, color = "black") +
  scale_color_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365")) +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    y = "Change in grass biomass ("*kg~ha^{-1}*")",
  ) +
  theme_classic()+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))



### Combine the plots in a single layout
multi_panelgbim <- (Gbimviolin/ Grasbiom2 / Bioemm) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1,1,  1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 7, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 7),        # Increase axis label font size
    axis.title = element_text(size = 7),       # Increase axis title font size
    plot.tag = element_text(size = 7, hjust = 0)  # Ensure left alignment
  )


##ggsave multipanel grass BIOMASS
 #ggsave(multi_panelgbim,filename ="Plots/Multipanel 2EMMGrass biomass.png",
       width = 16, height = 14, units = "cm")  

 
 

 