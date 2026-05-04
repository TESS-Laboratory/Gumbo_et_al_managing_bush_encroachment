#### this script analyses data with the TFB treatment excluded. 
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
library(multcomp) 


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

## LOAD DATA

SapF <- read_csv("DATA/March2025/WoodyPC4.csv")

Grasses <- read_csv("DATA/March2025/GrassesCombinedCleaned3.csv")

GrassHEIGHT<- read_csv("DATA/March2025/GrassHeight.csv")


## create seedling, sapling, trees and cut-stump row
SapF <- SapF %>% 
  mutate(
    woody_cat = case_when(
      Woody_class == "Cut stump"          ~ "Cut stump",
      between(`Max_height(m)`, 0.05, 0.50)          ~ "Seedlings",
      between(`Max_height(m)`, 0.51, 1.49)          ~ "Saplings",
      between(`Max_height(m)`, 1.5, 21.0)           ~ "Trees",
      TRUE                             ~ NA_character_
    ),
    Treatment = factor(Treatment),
    Fencing = factor(Fencing)
  )

#Filter SEEDLINGS  
Seedlings <- SapF %>% 
  filter(woody_cat == "Seedlings", Year %in% c(2024, 2025),
         !Treatment %in% c("TFB")) %>% 
  count(Site, Plot, Subplot, Treatment, Fencing,Year, name = "Seedlings") %>% 
  mutate(density_ha = Seedlings * 10000 / 600)     # convert to ha⁻¹


#sort pre and post treatment
strt_comparison2 <- Seedlings %>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


#reorder so that pre-treatment appears first then post treatment second on the plots
strt_comparison2 <- strt_comparison2 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))



# Convert to pre and post treatment columns
strt_wide <- strt_comparison2 %>%
  dplyr:: select(Site, Plot, Subplot, Treatment, Fencing, Period, density_ha) %>%
  pivot_wider(
    names_from = Period,
    values_from = density_ha
  )



## increasing font size for x and y axis
SeedViolin<- ggplot(strt_comparison2, 
                    aes(x = Treatment, y = density_ha, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_violin(trim = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed") +    
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1.4, color = "black") +
  labs(x = "Treatment", 
       # y = "Seedlings density per ha",
       y = expression("Seedling density "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8)        # Axis tick labels
  )+
  scale_fill_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))



# Calculate log proportional change
strt_wide <- strt_wide  %>%
mutate(
  Slog_prop_change = log((`Post-treatment`)/(`Pre-treatment`))
)

# # optional percent proportional
# strt_wide <- strt_wide %>%
#   mutate(
#     percent_change = (exp(Slog_prop_change)) * 100
#   )


# check if zeros exist
  #sum(strt_wide$`Pre-treatment` == 0)
  #sum(strt_wide$`Post-treatment` == 0)



# seedlings violin plot log proportional change

logSdviolin <- ggplot(strt_wide,
                       aes(x = Treatment, y = Slog_prop_change, fill = Fencing))+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       y = expression("Log proportional change "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 6),  # Axis titles reduced from 12 to 8
    axis.text = element_text(size = 6)) +
  scale_fill_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365"))


##### to test effect of treatment * fencing on seedling density###################

# Make "Unfenced" the reference level 

class(strt_wide$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
strt_wide$Fencing <- factor(strt_wide$Fencing, ordered = FALSE)

# Verify
levels(strt_wide$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
strt_wide$Fencing <- relevel(strt_wide$Fencing, ref = "Unfenced")

##### Convert character variables to factors
#Seedlings_Delta1$Treatment <- as.factor(Seedlings_Delta1$Treatment)
#Seedlings_Delta1$Fencing <- as.factor(Seedlings_Delta1$Fencing)

## lmm nested structure

logSeedlings <- lmer(Slog_prop_change ~ Treatment * Fencing + (1|Site),
  data = strt_wide)

summary(logSeedlings)


# checking for model assumptions
qqnorm(resid(logSeedlings))
qqline(resid(logSeedlings))


#library(ggplot2)

diag_data <- data.frame(
  fitted = fitted(logSeedlings),
  residuals = resid(logSeedlings)
)

ggplot(diag_data, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_classic()


########### USING EMMEANS
# Estimated marginal means for Treatment within fencing 
lgSeedemm <- emmeans(logSeedlings, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with Tukey adjustment (or "none" if you only want vs control)
contrast_vs_control <- contrast(lgSeedemm, method = "trt.vs.ctrl", ref = "C")
summary(contrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
lgSeedcld_emm <- cld(lgSeedemm, adjust = "Dunnett", Letters = letters, type = "response")
cld_tbl <- as.data.frame(lgSeedcld_emm)


# prepare clean database for plotting
plot_dflg <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace



## Visualisation using ggplot
lgsemm <- ggplot(plot_dflg, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 2.5) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.35), width = 0.12) +
  geom_text(aes(label = Group,
                y = CI_upper + 0.5 * max(EMM)),
            position = position_dodge(width = 0.35), size = 3, color = "black") +
  scale_color_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365")) +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    #y = "Change in seedling density per ha",
    y = expression("Log proportional change "*ha^{-1}*"")
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8))



## Combine the plots in a single layout
multi_panelsE <- (SeedViolin/logSdviolin/ lgsemm) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1, 1, 1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 6, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 7),        # Increase axis label font size
    axis.title = element_text(size = 7),       # Increase axis title font size
    plot.tag = element_text(size = 7, hjust = 0)  # Ensure left alignment
  )


# save multi-panel plot
 #ggsave(multi_panelsE,filename ="Plots/Multipanel logEMMViolin SEEDLINGS.png",
       width = 16, height = 14, units = "cm")

###### Log percent change using Dunnetts

# 
logSeedlings <- lmer(Slog_prop_change ~ Treatment * Fencing + (1|Site),
                     data = strt_wide)


levels(strt_wide$Treatment)

# set control as reference
strt_wide$Treatment <- relevel(strt_wide$Treatment, ref = "C")

# refit the model
logSeedlings <- lmer(Slog_prop_change ~ Treatment * Fencing + (1|Site),
                     data = strt_wide)

#Obtain estimated marginal means

percntSeed <- emmeans(logSeedlings, ~ Treatment | Fencing)

#Run Dunnett test (treatments vs control)
dunnett_res <- contrast(
  percntSeed,
  method = "trt.vs.ctrl",
  ref = "C",
  adjust = "dunnett"
)

summary(dunnett_res)

# Get Dunnett contrasts WITH confidence intervals
dunnett_summary <- summary(
  dunnett_res,
  infer = c(TRUE, TRUE)   # adds lower.CL and upper.CL
)

#Convert Dunnett results to percent change
dunnett_percent <- summary(dunnett_summary) |>
  dplyr::mutate(
    Percent_difference = (exp(estimate) - 1) * 100,
    Lower_CL = (exp(lower.CL) - 1) * 100,
    Upper_CL = (exp(upper.CL) - 1) * 100
  )

# compact letter display
cld(emm, adjust = "dunnett", Letters = letters)

#
# Panel A: Percent change (Dunnett)
p1 <- ggplot(plot_data,
             aes(x = Treatment,
                 y = Percent_difference,
                 ymin = Lower_CL,
                 ymax = Upper_CL,
                 colour = Fencing)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.9) +
  coord_flip() +
  labs(
    y = "Percent change vs control (%)",
    x = NULL,
    colour = "Fencing"
  ) +
  theme_classic(base_size = 13)


## Save
write.csv(dunnett_percent, "Plots/biomass_predictions.csv", row.names = FALSE)




###############################################################################

### LOG TRANSFORMED SAPLINGS


#Filter SEEDLINGS  
Saplings <- SapF %>% 
  filter(woody_cat == "Saplings", Year %in% c(2024, 2025),
         !Treatment %in% c("TFB")) %>% 
  count(Site, Plot, Subplot, Treatment, Fencing,Year, name = "Saplings") %>% 
  mutate(density_ha = Saplings * 10000 / 600)     # convert to ha⁻¹


#sort pre and post treatment
sapi <- Saplings %>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


#reorder so that pre-treatment appears first then post treatment second on the plots
sapi <- sapi %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))



# Convert to pre and post treatment columns
sapstrt_wide <- sapi %>%
  dplyr:: select(Site, Plot, Subplot, Treatment, Fencing, Period, density_ha) %>%
  pivot_wider(
    names_from = Period,
    values_from = density_ha
  )


## Calculate log proportional change
sapstrt_wide <- sapstrt_wide  %>%
  mutate(
    log_prop_change = log((`Post-treatment`)/(`Pre-treatment`))
  )


### violin plot sapling log proportional change
logSapviolin <- ggplot(sapstrt_wide,
                      aes(x = Treatment, y = log_prop_change, fill = Fencing))+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       y = expression("Log proportional change "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 6),  # Axis titles reduced from 12 to 8
    axis.text = element_text(size = 6)) +
  scale_fill_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365"))


# check if zeros exist
#sum(strt_wide$`Pre-treatment` == 0)
#sum(strt_wide$`Post-treatment` == 0)

##### to test effect of treatment * fencing on sapling density###################

# Make "Unfenced" the reference level 

class(sapstrt_wide$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
sapstrt_wide$Fencing <- factor(sapstrt_wide$Fencing, ordered = FALSE)

# Verify
levels(sapstrt_wide$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
sapstrt_wide$Fencing <- relevel(sapstrt_wide$Fencing, ref = "Unfenced")

##### Convert character variables to factors
#Seedlings_Delta1$Treatment <- as.factor(Seedlings_Delta1$Treatment)
#Seedlings_Delta1$Fencing <- as.factor(Seedlings_Delta1$Fencing)

## lmm nested structure

logSaplings <- lmer(log_prop_change ~ Treatment * Fencing + (1|Site),
                     data = sapstrt_wide)

summary(logSaplings)

# checking for model assumptions
qqnorm(resid(logSaplings))
qqline(resid(logSaplings))

## fitted residuals
diag_data <- data.frame(
  fitted = fitted(logSaplings),
  residuals = resid(logSaplings)
)

ggplot(diag_data, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_classic()


########### USING EMMEANS
# Estimated marginal means for Treatment within Kraaling (if needed)
lgSapdemm <- emmeans(logSaplings, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with Tukey adjustment (or "none" if you only want vs control)
contrast_vs_control <- contrast(lgSapdemm, method = "trt.vs.ctrl", ref = "C")
summary(contrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
lgSapcld_emm <- cld(lgSapdemm, adjust = "Dunnett", Letters = letters, type = "response")
cld_tbl <- as.data.frame(lgSapcld_emm)


# prepare clean database for plotting
plot_dfsplg <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace


## Visualising emm using ggplot
lgsapemm <- ggplot(plot_dfsplg, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 2.5) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.35), width = 0.12) +
  geom_text(aes(label = Group,
                y = CI_upper + 0.5 * max(EMM)),
            position = position_dodge(width = 0.35), size = 3, color = "black") +
  scale_color_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365")) +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    y = expression("Log proportional change "*ha^{-1}*"")
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8))


##### change in sapling density before and after treatment # used codes from REfined woody grasse
lgSapViolin<- ggplot(sptrt_comparison2, 
                   aes(x = Treatment, y = density_ha, fill = Period)) + facet_wrap(~Fencing)+
  geom_violin(trim = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") + 
  labs(x = "Treatment", 
       # y = "Sapling density per ha",
       y = expression("Sapling density "*ha^{-1}*"")
  ) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 8), # Axis titles size reduced to 8 from 12
    axis.text = element_text(size = 8))+
  scale_fill_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))


# Combine the plots in a single layout - SAPLINGS
multi_panel <- (lgSapViolin / logSapviolin/ lgsapemm) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1,1, 1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 8, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 8),        # Increase axis label font size
    axis.title = element_text(size = 8),       # Increase axis title font size
    plot.tag = element_text(size = 8, hjust = 0)  # Ensure left alignment
  )

#saving using ggsave
ggsave(multi_panel,filename ="Plots/Multipanel logEMMViolin Saplings.png",
       width = 16, height = 14, units = "cm")  


######## SAPLINGS LOG percent change using Dunnetts emmeans

logSaplings <- lmer(log_prop_change ~ Treatment * Fencing + (1|Site),
                    data = sapstrt_wide)

levels(sapstrt_wide$Treatment)

# set control as reference
sapstrt_wide$Treatment <- relevel(sapstrt_wide$Treatment, ref = "C")

# refit the model
logSaplings <- lmer(log_prop_change ~ Treatment * Fencing + (1|Site),
                    data = sapstrt_wide)

#Obtain estimated marginal means

percntSap <- emmeans(logSaplings, ~ Treatment | Fencing)

#Run Dunnett test (treatments vs control)
dunnett_resSP <- contrast(
  percntSap,
  method = "trt.vs.ctrl",
  ref = "C",
  adjust = "dunnett"
)

summary(dunnett_resSP)

# Get Dunnett contrasts WITH confidence intervals
dunnett_summarySP <- summary(
  dunnett_resSP,
  infer = c(TRUE, TRUE)   # adds lower.CL and upper.CL
)

#Convert Dunnett results to percent change
dunnett_percentSP <- summary(dunnett_summarySP) |>
  dplyr::mutate(
    Percent_difference = (exp(estimate) - 1) * 100,
    Lower_CL = (exp(lower.CL) - 1) * 100,
    Upper_CL = (exp(upper.CL) - 1) * 100
  )


############################################################################################

######################################## GRASSSES GRASSES GRASSSES
################# PREDICTING BIOMASS FROM DPM HEIGHT

# Load data
heights_data <-  read_csv("DATA/March2025/GrassHeight.csv")

# defining intercept and slope
INTERCEPT <- 4.67
SLOPE <- 1.14

# Ensure height column
if (!"DPM_Height" %in% names(heights_data)) {
  heights_data$DPM_Height <- heights_data[[names(heights_data)[1]]]
  cat("Using first column as Height_cm\n")
}

# Predict biomass using log-transformed model
predict_biomass <- function(h) {
  
  # avoid log(0) or negative values
  h <- pmax(h, 1e-6)
  
  log_b <- INTERCEPT + SLOPE * log(h)
  
  # back-transform to biomass scale
  biomass <- exp(log_b)
  
  return(round(biomass, 2))
}
options(scipen = 999) # to remove scientific notation

# 
heights_data$Biomass_kg_ha <- round(predict_biomass(heights_data$DPM_Height),2)

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

## VIOLIN plot- GRASS biomass post vs pre treatment excluding TFB
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



########## USing LOG proportional change
# Mutate height data
heights_data <- heights_data %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

# aggregate biomass at subplot level
biomass_subplot <- heights_data %>%
  filter(!is.na(DPM_Height),
         Year %in% c(2024, 2025),!Treatment %in% c("TFB")) %>%  # filter TFB
  summarise(
    Biomass_kg_ha = mean(Biomass_kg_ha, na.rm = TRUE),
    .by = c(Site, Plot, Subplot, Treatment, Fencing, Period)
  )

# reshape data
biomass_wide <- biomass_subplot %>%
  dplyr::select(Site, Plot, Subplot, Treatment, Fencing, Period, Biomass_kg_ha) %>%
  pivot_wider(
    names_from = Period,
    values_from = Biomass_kg_ha
  )

# calculating log proportional change
biomass_wide <- biomass_wide %>%
  mutate(
    `Pre-treatment`  = pmax(`Pre-treatment`, 1e-6),
    `Post-treatment` = pmax(`Post-treatment`, 1e-6),
    log_prop_changeGB = log((`Post-treatment`)/(`Pre-treatment`))
  )

### violin plot grass biomass log proportional change
logGrassBviolin <- ggplot(biomass_wide,
                       aes(x = Treatment, y = log_prop_change, fill = Fencing))+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       y = expression("Log proportional change "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 6),  # Axis titles reduced from 12 to 8
    axis.text = element_text(size = 6)) +
  scale_fill_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365"))


##### to test effect of treatment * fencing on grass biomass###################

# Make "Unfenced" the reference level 

class(biomass_wide$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
biomass_wide$Fencing <- factor(biomass_wide$Fencing, ordered = FALSE)

# Verify
levels(biomass_wide$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
biomass_wide$Fencing <- relevel(biomass_wide$Fencing, ref = "Unfenced")



## LMM analysis
logGrassBiomass <- lmer(log_prop_changeGB ~ Treatment * Fencing + (1|Site),
                        data = biomass_wide)

summary(logGrassBiomass)


########### USING EMMEANS
# Estimated marginal means for Treatment within Kraaling (if needed)
lgGBemm <- emmeans(logGrassBiomass, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with Tukey adjustment (or "none" if you only want vs control)
contrast_vs_control <- contrast(lgGBemm, method = "trt.vs.ctrl", ref = "C")
summary(contrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
lgGBcld_emm <- cld(lgGBemm, adjust = "Dunnett", Letters = letters, type = "response")
cld_tbl <- as.data.frame(lgGBcld_emm)


# prepare clean database for plotting
plot_dfGBlg <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace


## Visualisation of emm using ggplot
lgGBiemm <- ggplot(plot_dfGBlg, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 2.5) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.35), width = 0.12) +
  geom_text(aes(label = Group,
                y = CI_upper + 0.5 * max(EMM)),
            position = position_dodge(width = 0.35), size = 3, color = "black") +
  scale_color_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365")) +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    y = expression("Log proportional change "*ha^{-1}*"")
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8))


# Combine the plots in a single layout - SAPLINGS
multi_panel <- (Gbimviolin/logGrassBviolin/lgGBiemm) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1,1, 1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 8, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 8),        # Increase axis label font size
    axis.title = element_text(size = 8),       # Increase axis title font size
    plot.tag = element_text(size = 8, hjust = 0)  # Ensure left alignment
  )

#saving using ggsave
ggsave(multi_panel,filename ="Plots/Multipanel logEMMViolin Grass Biomass.png",
       width = 16, height = 14, units = "cm")  



####### GRASS BIOMAS LOG percent change using Dunnetts

logGrassBiomass <- lmer(log_prop_changeGB ~ Treatment * Fencing + (1|Site),
                        data = biomass_wide)


# set control as reference
biomass_wide$Treatment <- relevel(biomass_wide$Treatment, ref = "C")

# refit the model
logGrassBiomass <- lmer(log_prop_changeGB ~ Treatment * Fencing + (1|Site),
                        data = biomass_wide)

#Obtain estimated marginal means

percntgb <- emmeans(logGrassBiomass, ~ Treatment | Fencing)

#Run Dunnett test (treatments vs control)
dunnett_resgb <- contrast(
  percntgb,
  method = "trt.vs.ctrl",
  ref = "C",
  adjust = "dunnett"
)

summary(dunnett_resSP)

# Get Dunnett contrasts WITH confidence intervals
dunnett_summaryGB <- summary(
  dunnett_resgb,
  infer = c(TRUE, TRUE)   # adds lower.CL and upper.CL
)

#Convert Dunnett results to percent change
dunnett_percentGB <- summary(dunnett_summaryGB) |>
  dplyr::mutate(
    Percent_difference = (exp(estimate) - 1) * 100,
    Lower_CL = (exp(lower.CL) - 1) * 100,
    Upper_CL = (exp(upper.CL) - 1) * 100
  )




#############################################################################

######## GRASS SPECIES RICHNESS
# 1. Prepare data: richness per Site x Treatment x Year
Grass_rich <- Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025), !Treatment %in% c("TFB")) %>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop")%>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

##reorder so that pre-treatment appears first then post treatment second on the plots
Grass_rich <- Grass_rich %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

#Grass species richness violin 

GrRViolin <- ggplot(Grass_rich, 
                    aes(x = Treatment, y = spp_richness, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_violin(trim = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +   
  labs(x = "Treatment", 
       y = "Grass species richness") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8)        # Axis tick labels
  ) +
  scale_fill_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))

#### LOG PROPORTIONAL CHANGE - spp richness

Grass_F <- Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025),!Treatment %in% c("TFB")) %>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Year, Fencing) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop")


#Pivot the two years side‑by‑side and compute  log Δ GRASS RICHNESS ─────────────
grassR_logprop <- Grass_F %>%
  pivot_wider(names_from = Year, values_from = spp_richness, names_prefix = "Y") %>%
  mutate(logRichness = Y2025/Y2024)


# visualising grass richness using ggplot
logGrspp <- ggplot(grassR_logprop,aes(x = Treatment, y = logRichness, fill = Fencing)) +
geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Log proportional change") +
  theme_classic() +
  #scale_fill_manual(values = c("Fenced" = "saddlebrown", "Unfenced" = "navajowhite"))
  #scale_fill_brewer(palette = "YlOrBr")  # Yellow-Orange-Brown palette
  #scale_fill_manual(values = c("#8c510a", "#d8b365"))  # Dark brown, light brown
  scale_fill_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365"))


## LMM analysis 

# Make "Unfenced" the reference level 

class(grassR_logprop$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
grassR_logprop$Fencing <- factor(grassR_logprop$Fencing, ordered = FALSE)

# Verify
levels(logRichness$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
grassR_logprop$Fencing <- relevel(grassR_logprop$Fencing, ref = "Unfenced")


## Using LMM instead of glmm
LogmodelRich <- lmer(logRichness ~ Treatment * Fencing + (1|Site),  #including Plot produced error message
                  data = grassR_logprop)

summary(LogmodelRich)


##### EMM FOR GRASS RICHNESS
# Estimated marginal means for Treatment within Fencing 
logGRemm <- emmeans(LogmodelRich, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with dunnetts adjustment (or "none" if you only want vs control)
grcontrast_vs_control <- contrast(logGRemm, method = "trt.vs.ctrl", ref = "C")
summary(grcontrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
logGrlcld_emm <- cld(logGRemm, adjust = "dunnettx", Letters = letters, type = "response")
cld_tbl <- as.data.frame(logGrlcld_emm)


# prepare clean database for plotting
logGplot_df <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace

## Visualisation using ggplot for grass richness
logGrlemm <- ggplot(logGplot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.35), width = 0.12) +
  geom_text(aes(label = Group,
                y = CI_upper + 0.25 * max(EMM)),
            position = position_dodge(width = 0.35), size = 3, color = "black") +
  scale_color_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365")) +
  theme_classic(base_size = 14) +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    y = "Log proportional change",
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))


# Combine the plots in a single layout species richness
multi_panelR <- (GrRViolin / logGrspp/ logGrlemm) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1, 1, 1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 8, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 8),        # Increase axis label font size
    axis.title = element_text(size = 8),       # Increase axis title font size
    plot.tag = element_text(size = 8, hjust = 0)  # Ensure left alignment
  )

##ggsave multipanel grass richness
ggsave(multi_panelR,filename ="Plots/Multipanel logEMMViolin Grass richness.png",
       width = 16, height = 14, units = "cm")



######## GRASS RICHNESS LOG percent change using Dunnetts emmeans

LogmodelRich <- lmer(logRichness ~ Treatment * Fencing + (1|Site),  #including Plot produced error message
                     data = grassR_logprop)

levels(grassR_logprop$Treatment)

# set control as reference
grassR_logprop$Treatment <- relevel(grassR_logprop$Treatment, ref = "C")

# refit the model
LogmodelRich <- lmer(logRichness ~ Treatment * Fencing + (1|Site),  #including Plot produced error message
                     data = grassR_logprop)


#Obtain estimated marginal means

percntRichness <- emmeans(LogmodelRich, ~ Treatment | Fencing)

#Run Dunnett test (treatments vs control)
dunnett_resRCH <- contrast(
  percntRichness,
  method = "trt.vs.ctrl",
  ref = "C",
  adjust = "dunnett"
)

summary(dunnett_resRCH)

# Get Dunnett contrasts WITH confidence intervals
dunnett_summaryRCH <- summary(
  dunnett_resRCH,
  infer = c(TRUE, TRUE)   # adds lower.CL and upper.CL
)

#Convert Dunnett results to percent change
dunnett_percentRCH <- summary(dunnett_summaryRCH) |>
  dplyr::mutate(
    Percent_difference = (exp(estimate) - 1) * 100,
    Lower_CL = (exp(lower.CL) - 1) * 100,
    Upper_CL = (exp(upper.CL) - 1) * 100
  )


############################################################################################


################################ GRASS DIVERSITY 

# Calculate Shannon-Wiener Diversity Index at Site and Plot level
GrSWeiner <- Grasses%>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025),!Treatment %in% c("TFB"))%>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Year, Fencing, Species_name)%>%
  summarise(Spp_count = n(), .groups = "drop" )

## Calculate Shannon-Wiener Diversity Index at treatment level
grSWdiversity <- GrSWeiner %>%
  group_by(Site, Plot, Subplot,Treatment, Year, Fencing) %>%                   # Group by Site and Plot
  summarise(
    Shannon_Diversity = -sum((Spp_count / sum(Spp_count)) * log(Spp_count / sum(Spp_count))),
    .groups = "drop")%>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

##reorder so that pre-treatment appears first then post treatment second on the plots
grSWdiversity <- grSWdiversity %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

###### violin pre vs post treatment
GSWViolin <- ggplot(grSWdiversity,
                    aes(x = Treatment, y =Shannon_Diversity, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_violin(trim = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +
  labs(x = "Treatment", 
       y = "Shannon-Weiner diversity",
  ) + theme_classic()+
  theme(axis.title = element_text(size = 8),
        axis.text = element_text(size = 8)) +  # Axis tick labels
  scale_fill_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))


#### log change SHANNON-WEINER DIVERSITY INDEX
Gdiv <- Grasses%>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025), !Treatment %in% c("TFB"))%>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Year, Fencing, Species_name)%>%
  summarise(Spp_count = n(), .groups = "drop" )

# Calculate Shannon-Wiener Diversity Index 
Sdiversity_data <- Gdiv %>%
  group_by(Site, Plot, Subplot,Treatment, Year, Fencing) %>%   # Group by Site and Plot
  summarise(
    Shannon_Diversity = -sum((Spp_count / sum(Spp_count)) * log(Spp_count / sum(Spp_count))),
    .groups = "drop"
  )

#  Pivot the two years side‑by‑side and compute Δ‑ grass Shannon-Weiner diversity
logSW <- Sdiversity_data %>% 
  pivot_wider(names_from  = Year,
              values_from = Shannon_Diversity,
              names_glue  = "sw_{Year}") %>% 
  mutate(logprop_SW = sw_2025/sw_2024)   

### Convert character variables to factors
logSW$Treatment <- as.factor(logSW$Treatment)
logSW$Fencing <- as.factor(logSW$Fencing)


# Visualising change
### LOG proportional change Grass Diversity Mean points on the violin
logGrSW <- ggplot(logSW,aes(x = Treatment, y =logprop_SW, fill = Fencing)) +
  geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Log proportional change") +
  theme_classic() +
  scale_fill_manual(values = c("#8c510a", "#d8b365")) 


## ## LMM to test effect of treatment * fencing on Grass diversity ##################

# Make "Unfenced" the reference level 

class(logSW$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
logSW$Fencing <- factor(logSW$Fencing, ordered = FALSE)

# Verify
levels(logSW$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
logSW$Fencing <- relevel(logSW$Fencing, ref = "Unfenced")

 
#LMM analysis
logGrasD <- lmer(logprop_SW~ Treatment * Fencing + (1|Site),  
              data = logSW)

summary(logGrasD)


## checking for model assumptions
qqnorm(resid(logGrasD))
qqline(resid(logGrasD))

## fitted residuals
diag_data <- data.frame(
  fitted = fitted(logGrasD),
  residuals = resid(logGrasD)
)

ggplot(diag_data, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_classic()


## ### EMM FOR GRASS DIVERSITY
# Estimated marginal means for Treatment within Fencing 
logGDemm <- emmeans(logGrasD, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with dunnets adjustment (or "none" if you only want vs control)
gDcontrast_vs_control <- contrast(logGDemm, method = "trt.vs.ctrl", ref = "C")
summary(gDcontrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
logGDcld_emm <- cld(logGDemm, adjust = "Dunnett", Letters = letters, type = "response")
cld_tbl <- as.data.frame(logGDcld_emm)  # 


# prepare clean database for plotting
logdplot_df <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace

## Visualisation using ggplot for grass diversity
logGdm <- ggplot(logdplot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.35), width = 0.12) +
  geom_text(aes(label = Group,
                y = CI_upper + 0.25 * max(EMM)),
            position = position_dodge(width = 0.35), size = 3, color = "black") +
  scale_color_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365")) +
  theme_classic(base_size = 14) +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    y = "Log proportional change",
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))

#### Combine the plots in a single layout
multi_panelSWd <- (GSWViolin / logGrSW/ logGdm) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1,1,  1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 7.5, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 7.5),        # Increase axis label font size
    axis.title = element_text(size = 7.5),       # Increase axis title font size
    plot.tag = element_text(size = 7.5, hjust = 0)  # Ensure left alignment
  )

##ggsave multipanel grass richness
 #ggsave(multi_panelSWd,filename ="Plots/Multipanel logEMMViolin Grassdiversity.png",
       width = 16, height = 14, units = "cm")  



######## GRASS DIVERSITY LOG percent change using Dunnetts emmeans

#LMM analysis
logGrasD <- lmer(logprop_SW~ Treatment * Fencing + (1|Site),  
                 data = logSW)

levels(logSW$Treatment)

# set control as reference
logSW$Treatment <- relevel(logSW$Treatment, ref = "C")

# refit the model
logGrasD <- lmer(logprop_SW~ Treatment * Fencing + (1|Site),  
                 data = logSW)


#Obtain estimated marginal means

percntSW <- emmeans(logGrasD, ~ Treatment | Fencing)

#Run Dunnett test (treatments vs control)
dunnett_resSW <- contrast(
  percntSW,
  method = "trt.vs.ctrl",
  ref = "C",
  adjust = "dunnett"
)

summary(dunnett_resSW)

# Get Dunnett contrasts WITH confidence intervals
dunnett_summarySW <- summary(
  dunnett_resSW,
  infer = c(TRUE, TRUE)   # adds lower.CL and upper.CL
)

#Convert Dunnett results to percent change
dunnett_percentSW <- summary(dunnett_summarySW) |>
  dplyr::mutate(
    Percent_difference = (exp(estimate) - 1) * 100,
    Lower_CL = (exp(lower.CL) - 1) * 100,
    Upper_CL = (exp(upper.CL) - 1) * 100
  )

