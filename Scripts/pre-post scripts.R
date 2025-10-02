library(tidyverse)
library(vegan)
library(multcompView)
library(patchwork)
library(lmerTest)
library(ggplot2)
library(dplyr)
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
 #library(glmmLasso)  # for LASSO regression
 #library(glmnet)

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

SapF <- read_csv("DATA/March2025/WoodyPC3.csv")

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


#Filter TREES  
Treesub <- SapF %>% 
  filter(woody_cat == "Trees", Year %in% c(2024, 2025)) %>% 
  count(Site, Plot, Subplot, Treatment, Fencing,Year, name = "Trees") %>% 
  mutate(density_ha = Trees * 10000 / 600)     # convert to ha⁻¹

#comparing at treatment level

trees_treat <- Treesub %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>% 
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop")  


#sort pre and post treatment
trt_comparison2 <- Treesub %>%
filter(Year %in% c(2024, 2025)) %>%
mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

#boxplot showing overall comparison post and pre treatment
ggplot(trt_comparison2, aes(x = Period, y = density_ha, fill = Period)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Density Distribution: Pre vs Post Treatment",
       x = "Period", 
       y = "Density (ha)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none")

# boxplot- TREE density post vs pre treatment excluding encroachment level
Trpp<- ggplot(trt_comparison2, 
       aes(x = Treatment, y = density_ha, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  labs(x = "Treatment", 
       y = "Tree density per ha",
      ) +
  theme_beautiful() 
 
##saving pre& post treatment BOXPLOT  -  excluding encroachment level
ggsave(Trpp,filename ="Plots/PP Tree Density BOXplot.png",
       width = 16, height = 14, units = "cm")  
  

### DELTA TREE DENSITY

#Pivot the two years side‑by‑side and compute Δ TREE density ─────────────
Trees_Delta <- trees_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Trdens = dens_2025 - dens_2024) 

#Visualise: boxplot of Δ‑ TREE DENSITY by interaction
TRbxp <- ggplot(Trees_Delta,
                  aes(x = Treatment, y = delta_Trdens)) + facet_wrap(~ Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Trees density per ha") +
  theme_beautiful() #

##saving BOXPLOT - Δ TREE density 
ggsave(TRbxp,filename ="Plots/3Delta Tree Density INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")  


### Make "Unfenced" the reference level (to see "fenced" coefficients)
  # Check current class of FieldType
  
class(Trees_Delta$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
Trees_Delta$Fencing <- factor(Trees_Delta$Fencing, ordered = FALSE)

# Verify
levels(Trees_Delta$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level (to see "Unfenced" coefficients)
Trees_Delta$Fencing <- relevel(Trees_Delta$Fencing, ref = "Unfenced")


##GLMM FOR DELTA TREE DENSITY 
Treeden <- glmmTMB(delta_Trdens ~ Treatment * Fencing  + (1 | Site) + (1|Plot),
data = Trees_Delta, family = gaussian(link = "identity"))

summary(Treeden)

VarCorr(Treeden)

## 2. Fixed effects no interaction 
Treeden2 <- glmmTMB(density_ha ~ Treatment  + Fencing + (1 | Site) + (1|Plot),
                   data = trt_comparison2, family = gaussian(link = "identity"))

summary(Treeden2)

# check for model residuals and distribution
qqnorm(resid(Treeden))
qqline(resid(Treeden))

# For convergence diagnostics:
performance::check_convergence(Treeden) # TRUE the model converged

## MODEL performance
performance::check_model(Treeden2)


##Check for outliers
residuals_std <- scale(resid(Treeden))  # Standardize residuals
outliers <- which(abs(residuals_std) > 3)  # Find outliers
print(outliers)


## Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(Treeden))  #p < 0.05 → residuals are non-normal.
# result p = 0.0002299, hence residuals are non-normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(Treeden), breaks = 5)
leveneTest(resid(Treeden) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.00405 unequal variance 


##### LOG TRANSFORMING TREE DENSITY 

# 1) Create signed log transform of Delta weed density
Trees_Delta$DeltaDensity_log <- sign(Trees_Delta$delta_Trdens) * log(abs(Trees_Delta$delta_Trdens) + 1)


# 2) Fit Gaussian LMM with crossed random effects
##GLMM FOR log TREE DENSITY 
TreedenMod <- glmmTMB(DeltaDensity_log ~ Treatment * Fencing  + (1 | Site) + (1|Plot),
                   data = Trees_Delta, family = gaussian(link = "identity"))

summary(TreedenMod)
VarCorr(TreedenMod)

##Lmer
Modtden <- lmer(DeltaDensity_log ~ Treatment * Fencing  + (1 | Site),
                     data = Trees_Delta)

summary(Modtden)

VarCorr(Modtden)
### MODEL performance
performance::check_model(TreedenMod)

## Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(Modtden))  #p < 0.05 → residuals are non-normal.
# result p = 0.0000416, hence residuals are non-normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(Modtden), breaks = 5)
leveneTest(resid(Modtden) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.0047 unequal variance 



########################################################################

## TREE SIMPSON'S DIVERSITY 

# Calculate TREE Simpson's diversity index for each plot by year
TrSimp <- SapF %>%
  filter(!is.na(Species_name),  # Remove missing species IDs
         Year %in% c(2024, 2025),   # Filter for 2024 and 2025 only
         woody_cat == "Trees")%>%  
  group_by(Site, Plot, Subplot, Fencing, Treatment, Year, Species_name) %>% 
  summarise(abundance = n(), .groups = "drop_last") %>%  # Count individuals per species
  summarise(
    Simpson_index = 1 - sum((abundance / sum(abundance))^2),
    total_abundance = sum(abundance),
    species_richness = n(),
    .groups = "drop"
  ) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

## boxplot- TREE Simpsons diversity post vs pre treatment excluding encroachment level
TrSpp<- ggplot(TrSimp, 
              aes(x = Treatment, y = Simpson_index, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  labs(x = "Treatment", 
       y = "Tree Simpsons index of diversity",
  ) + theme_beautiful() 

##saving TREE Simpsons index pre& post treatment BOXPLOT  -  excluding encroachment level
ggsave(TrSpp,filename ="Plots/PP Tree Simpsons Index BOXplot.png",
       width = 16, height = 14, units = "cm") 


###DELTA TREE SIMPSONS DIVERSITY 
# . Pivot the two years side‑by‑side and compute Δ SIMPSONS DIVERSITY
#Calculate species abundance per plot
TrSimDiv <- SapF %>%
  filter(!is.na(Species_name),
         woody_cat == "Trees")%>%  
  group_by(Site, Plot, Subplot, Fencing, Treatment,Encroachment_level, Year, Species_name) %>% 
  summarise(abundance = n())%>%
  summarise(
    Simpson_index1 = 1 - sum((abundance / sum(abundance))^2),
    .groups = "drop"
  )

# calculate Δ tree simpson's index of diversity 
tsp_Delta <- TrSimDiv %>% 
  pivot_wider(names_from  = Year,
              values_from = Simpson_index1,
              names_glue  = "sw_{Year}") %>% 
  mutate(Simpson_index1 = sw_2025 - sw_2024)  

## Visualise: boxplot of Δ‑ TREE SIMPSON'S INDEX OF DIVERSITY excluding encroachment level
TsiBX <- ggplot(tsp_Delta,
                 aes(x = Treatment, y = Simpson_index1)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Tree Simpson's index of diversity ") +
  theme_beautiful() #

##saving BOXPLOT - INTERACTIONS
ggsave(TsiBX,filename ="Plots/3Delta Tree Simpsons BOXplot.png",
       width = 16, height = 14, units = "cm")  


##### Make "Unfenced" the reference level (to see "fenced" coefficients)
# Check current class of FieldType

class(tsp_Delta$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
tsp_Delta$Fencing <- factor(tsp_Delta$Fencing, ordered = FALSE)

# Verify
levels(tsp_Delta$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level (to see "Unfenced" coefficients)
tsp_Delta$Fencing <- relevel(tsp_Delta$Fencing, ref = "Unfenced")



#####GLMM FOR DELTA TREE SIMPSONS DIVERSITY
TreeSi <- glmmTMB(Simpson_index1 ~ Treatment * Fencing + (1|Site) + (1|Plot),  # Crossed effects,
                 data = tsp_Delta, family = gaussian(link = "identity"))
summary(TreeSi)

#removed fencing to reduce multicollinearity
TreeSi1 <- glmmTMB(Simpson_index1 ~ Treatment  + (1|Site) + (1|Plot),  # Crossed effects,
                  data = tsp_Delta, family = gaussian(link = "identity"))
summary(TreeSi1)



#check if model converged
performance::check_convergence(TreeSi) # TRUE the model converged

# MODEL performance
performance::check_model(TreeSi1)

vif(TreeSi1)

# Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(TreeSi))  #p < 0.05 → residuals are non-normal.
# result p = 0.0012, hence residuals are non-normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(TreeSi), breaks = 5)
leveneTest(resid(TreeSi) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.6875 equal variance 



###########SEEDLING DENSITY SEEDLING DENSITY SEEDLING DENSITY ########

#Filter SEEDLINGS  
Seedlings <- SapF %>% 
  filter(woody_cat == "Seedlings", Year %in% c(2024, 2025)) %>% 
  count(Site, Plot, Subplot, Treatment, Fencing,Year, name = "Seedlings") %>% 
  mutate(density_ha = Seedlings * 10000 / 600)     # convert to ha⁻¹

#comparing at treatment level

Seed_treat <- Seedlings %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>% 
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop")  


#sort pre and post treatment
strt_comparison2 <- Seedlings %>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

#reorder so that pre-treatment appears first then post treatment second on the plots
strt_comparison2 <- strt_comparison2 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

#boxplot showing overall comparison post and pre treatment
ggplot(strt_comparison2, aes(x = Period, y = density_ha, fill = Period)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Density Distribution: Pre vs Post Treatment",
       x = "Period", 
       y = "Density (ha)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none")

# boxplot- SEEDLING density post vs pre treatment excluding encroachment level
Seedpp<- ggplot(strt_comparison2, 
              aes(x = Treatment, y = density_ha, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  labs(x = "Treatment", 
       y = "Seedlings density per ha",
  ) +
  theme_beautiful() 

## increaseing font size for x and y axis
Seedpp2<- ggplot(strt_comparison2, 
                aes(x = Treatment, y = density_ha, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  labs(x = "Treatment", 
       y = "Seedlings density per ha",
  ) +
  theme_beautiful() +
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

##saving pre& post treatment BOXPLOT  -  excluding encroachment level
ggsave(Seedpp2,filename ="Plots/bPP Seedling Density BOXplot.png",
       width = 16, height = 14, units = "cm")  


###############################DELTA SEEDLING DENSITY

#Pivot the two years side‑by‑side and compute Δ seedling density ─────────────
Seedlings_Delta <- Seed_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Seeddens = dens_2025 - dens_2024) 

#Visualise: boxplot of Δ‑ SEEDLING DENSITY by interaction
Seedbxp <- ggplot(Seedlings_Delta,
                  aes(x = Treatment, y = delta_Seeddens)) + facet_wrap(~ Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in Seedling density per ha") +
  theme_beautiful() #

## increased font size for x and y axis
Seedbxp1 <- ggplot(Seedlings_Delta,
                  aes(x = Treatment, y = delta_Seeddens)) + facet_wrap(~ Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in Seedling density per ha") +
  theme_beautiful() +
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

##saving BOXPLOT - seedling density 
ggsave(Seedbxp1,filename ="Plots/3bDelta Seedling Density INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")  


#### GLMM to test effect of treatment * fencing on seedling density###################

# Make "Unfenced" the reference level 

class(Seedlings_Delta$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
Seedlings_Delta$Fencing <- factor(Seedlings_Delta$Fencing, ordered = FALSE)

# Verify
levels(Seedlings_Delta$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
Seedlings_Delta$Fencing <- relevel(Seedlings_Delta$Fencing, ref = "Unfenced")


##  2 way interaction and test using GLMM
# Site and plot as crossed random effects
Seedl3 <- glmmTMB(delta_Seeddens ~ Treatment * Fencing + (1|Site) + (1|Plot),  # Crossed effects,
                  data = Seedlings_Delta, family = gaussian(link = "identity"))
summary(Seedl3) #  converged

##2 fixed effects as independent
Seedl1 <- glmmTMB(delta_Seeddens ~ Treatment + Fencing + (1|Site) + (1|Plot),  # Crossed effects,
                  data = Seedlings_Delta, family = gaussian(link = "identity"))
summary(Seedl1)


#check if model converged
performance::check_convergence(Seedl3) # TRUE the model converged

# MODEL performance
performance::check_model(Seedl3)

# Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(Seedl3))  #p < 0.05 → residuals are non-normal.
# result p = 0.7431, hence residuals are normal


#Q-Q plots check if residuals of a model follow a normal distribution.
qqnorm(resid(Seedl3)) 
qqline(resid(Seedl3))

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(Seedl3), breaks = 5)
leveneTest(resid(Seedl3) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.918 equal variance 

#Variance Inflation Factor (VIF) to detect collinearity,VIF > 5 or 10 is problematic shows high collinearity
VIF(Seedl3)


# Check if Subplots are uniquely nested within Sites
nesting_check <- Seedlings_Delta %>%
  distinct(Site, Plot) %>%
  group_by(Plot) %>%
  summarise(n_sites = n_distinct(Site))

# If all n_locations == 1, nesting is appropriate
table(nesting_check$n_sites)

# Check if Subplot IDs are unique across Locations
subpaddock_ids <- Seedlings_Delta %>%
  group_by(Plot) %>%
  summarise(unique_sites = n_distinct(Site))

# If any Subpaddock appears in >1 Location, you can't nest
problematic <- filter(subpaddock_ids, unique_sites > 1)






################################ SAPLING SAPLING SAPLINGS SAPLINGS 
#Filter SAPLINGS  
Saplings <- SapF %>% 
  filter(woody_cat == "Saplings", Year %in% c(2024, 2025)) %>% 
  count(Site, Plot, Subplot, Treatment, Fencing,Year, name = "Saplings") %>% 
  mutate(density_ha = Saplings * 10000 / 600)     # convert to ha⁻¹

#comparing at treatment level

Sap_treat <- Saplings %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>% 
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop")  


#sort pre and post treatment
sptrt_comparison2 <- Saplings %>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

##reorder so that pre-treatment appears first then post treatment second on the plots
sptrt_comparison2 <- sptrt_comparison2 %>%
           mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

#boxplot showing overall comparison post and pre treatment
ggplot(sptrt_comparison2, aes(x = Period, y = density_ha, fill = Period)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Density Distribution: Pre vs Post Treatment",
       x = "Period", 
       y = "Density (ha)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none")

# boxplot- SAPLINGS density post vs pre treatment excluding encroachment level
Sapp<- ggplot(sptrt_comparison2, 
                aes(x = Treatment, y = density_ha, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  labs(x = "Treatment", 
       y = "Saplings density per ha") +
  theme_beautiful() 

##saving pre& post treatment BOXPLOT  -  excluding encroachment level
ggsave(Sapp,filename ="Plots/PP Saplings Density BOXplot.png",
       width = 16, height = 14, units = "cm")  


############################### DELTA SAPLING DENSITY

#Pivot the two years side‑by‑side and compute Δ sapling density ─────────────
Saplings_Delta <- Sap_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Sapsdens = dens_2025 - dens_2024) 

# Make "Unfenced" the reference level 

class(Saplings_Delta$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
Saplings_Delta$Fencing <- factor(Saplings_Delta$Fencing, ordered = FALSE)

# Verify
levels(Saplings_Delta$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
Saplings_Delta$Fencing <- relevel(Saplings_Delta$Fencing, ref = "Unfenced")



#Visualise: boxplot of Δ‑ Sapling DENSITY by interaction
Sapbxp <- ggplot(Saplings_Delta,
                  aes(x = Treatment, y = delta_Sapsdens)) + facet_wrap(~ Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Saplings density per ha") +
  theme_beautiful() #

##saving BOXPLOT - sapling density 
ggsave(Sapbxp,filename ="Plots/3Delta Sapling Density INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")  



##GLMM FOR DELTA SAPLING DENSITY
SapL1 <- glmmTMB(delta_Sapsdens ~ Treatment * Fencing + (1|Site) + (1|Plot),  # Crossed effects,
                  data = Saplings_Delta, family = gaussian(link = "identity"))
summary(SapL1)

#2. Random effects no interaction 
SapL2 <- glmmTMB(delta_Sapsdens ~ Treatment + Fencing + (1|Site), #+ (1|Plot),  # Crossed effects,
                 data = Saplings_Delta, family = gaussian(link = "identity"))
summary(SapL2)

#check if model converged
performance::check_convergence(SapL1) # FALSE the model ddnt converge

performance::check_convergence(SapL2) # TRUE the model converged

# MODEL performance
performance::check_model(SapL2)

# Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(SapL2))  #p < 0.05 → residuals are non-normal.
# result p = 0.06559, hence residuals are normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(SapL2), breaks = 5)
leveneTest(resid(SapL2) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.857 equal variance 



############################## SAPLING HEIGHT SAPLING HEIGHT ###################
#### Step 1: Filter SAPLINGS only and years 
Sapdata  <- SapF %>%
  filter(
    woody_cat == "Saplings",
    Year %in% c(2024, 2025)
  )         

#sort pre and post treatment
sapl_comparison2 <- Sapdata %>%
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_height = mean(`Max_height(m)`, na.rm = TRUE), .groups = "drop_last")%>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

#reorder so that pre-treatment appears first then post treatment second on the plots
sapl_comparison2 <- sapl_comparison2 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

##Visualise: boxplot of height by interaction
Sabx <- ggplot(sapl_comparison2,
                 aes(x = Treatment, y = mean_height, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Sapling height (m)") +
  theme_beautiful() +
  theme(legend.position = "top")

##saving BOXPLOT - INTERACTIONS
ggsave(Sabx,filename ="Plots/PP Sapling height INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")



##DELTA SAPLING HEIGHT
## Compute Δ‑height (2024 − 2025) per Site/Plot/Subplot/Treatment/Fenced ----
# Take the mean height within each grouping for each year before differencing.
delta_SPHeight <- Sapdata  %>%
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_height = mean(`Max_height(m)`, na.rm = TRUE), .groups = "drop_last") %>%
  pivot_wider(
    names_from  = Year,
    values_from = mean_height,
    names_glue  = "height_{Year}"
  ) %>%
  mutate(delta_SP = height_2025 - height_2024) %>%
  drop_na(delta_SP)   # keep groups where both years are present

# Make "Unfenced" the reference level 

class(delta_SPHeight$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
delta_SPHeight$Fencing <- factor(delta_SPHeight$Fencing, ordered = FALSE)

# Verify
levels(delta_SPHeight$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
delta_SPHeight$Fencing <- relevel(delta_SPHeight$Fencing, ref = "Unfenced")



#Visualise: boxplot of Δ‑height by interaction
Sappbx <- ggplot(delta_SPHeight,
                aes(x = Treatment, y = delta_SP)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Sapling height (m)") +
  theme_beautiful() #

##saving BOXPLOT - INTERACTIONS
ggsave(Sappbx,filename ="Plots/3Delta Sapling height INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")


####GLMM FOR DELTA SAPLING HEIGHT
SapH1 <- glmmTMB(delta_SP ~ Treatment * Fencing + (1|Site) + (1|Plot),  # Crossed effects,
                 data = delta_SPHeight, family = gaussian(link = "identity"))
summary(SapH1)


#2 random effects as independent no interaction
SapH2 <- glmmTMB(delta_SP ~ Treatment + Fencing + (1|Site) + (1|Plot),  # Crossed effects,
                 data = delta_SPHeight, family = gaussian(link = "identity"))
summary(SapH2)

#check if model converged
performance::check_convergence(SapH2) # TRUE the model converged

# MODEL performance
performance::check_model(SapH2)

# Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(SapH2))  #p < 0.05 → residuals are non-normal.
# result p = 0.2552, hence residuals are normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(SapH2), breaks = 5)
leveneTest(resid(SapH2) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.187 equal variance 

VarCorr(SapH1)



############################ RESPROUTS  RESPROUTS RESPROUTS ####################

# Step 1: Filter Cut stumps only and years 
resprouts_df <- SapF %>%
  filter(
    woody_cat == "Cut stump",
    Year %in% c(2025),
    !Treatment %in% c("C", "F", "TFB")  # exclude the two treatments
  )



### Make "Unfenced" the reference level (to see "fenced" coefficients)
# Check current class of FieldType

class(resprouts_df$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
resprouts_df$Fencing <- factor(resprouts_df$Fencing, ordered = FALSE)

# Verify
levels(resprouts_df$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level (to see "Unfenced" coefficients)
resprouts_df$Fencing <- relevel(resprouts_df$Fencing, ref = "Unfenced")


# Scale Variables to ensure convergence:
#resprouts_df$deltens_scaled <- as.numeric(scale(resprouts_df$No_of_resprouts))

# --- 4. Visualise: boxplot of RESPROUTS by INTERACTIONS -------------
Respbx <- ggplot(resprouts_df,
                 aes(x = Treatment, y = No_of_resprouts)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "No. of resprouts on cut stumps") +
  theme_beautiful() 


##saving BOXPLOT - INTERACTIONS
ggsave(Respbx,filename ="Plots/PP RESPROUTS INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")  


## GLMM for resprouts
Resc2 <- glmmTMB(No_of_resprouts ~ Treatment * Fencing + (1 | Site),
                 data = resprouts_df, family = poisson (link = "log"))
summary(Resc2)

## option 2 for dealing with non-normality# Fit the robust model. The syntax is identical to lmer!
Rrobust_model <- rlmer(No_of_resprouts ~ Treatment * Fencing + (1|Site),  # Crossed effects,
                       data = resprouts_df)

summary(robust_model)

#Checking model performance
performance::check_model(Resc2)

# Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(Resc2)) 


#############################################################################
#############################################################################

##################GRASSES GRASSES GRASSES GRASSES GRASSES

Grasses <- read_csv("DATA/March2025/GrassesCombinedCleaned2.csv")

summary_Gr <- Grasses %>%
  filter(!is.na(DPM_Height),
         Year %in% c(2024, 2025)) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_DPM_Height = mean(DPM_Height, na.rm = TRUE)) %>%
  ungroup() 


# Calculate mean grass height for pre and post treatment
grass_height <- Grasses %>%
  filter(!is.na(DPM_Height),
         Year %in% c(2024, 2025)) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_DPM_Height = mean(DPM_Height, na.rm = TRUE),
            #n_observations = n(),
            .groups = "drop") %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

##reorder so that pre-treatment appears first then post treatment second on the plots
grass_height <- grass_height %>%
    mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

### boxplot- GRASS DPM Height post vs pre treatment excluding encroachment level
Grbx<- ggplot(grass_height, 
              aes(x = Treatment, y = mean_DPM_Height, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  labs(x = "Treatment", 
       y = "Grass DPM height (cm)") +
  theme_beautiful() 

##saving pre& post treatment BOXPLOT  -  excluding encroachment level
ggsave(Grbx,filename ="Plots/PP Grass height BOXplot.png",
       width = 16, height = 14, units = "cm") 


######DELTA GRASS HEIGHT

# Take the mean height within each grouping for each year before differencing.
delta_GRHeight <- Grasses  %>%
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_DPM_Height = mean(DPM_Height, na.rm = TRUE), .groups = "drop_last") %>%
  pivot_wider(
    names_from  = Year,
    values_from = mean_DPM_Height,
    names_glue  = "height_{Year}"
  ) %>%
  mutate(delta_GR = height_2025 - height_2024) %>%
  drop_na(delta_GR)   # keep groups where both years are present


#Visualise: boxplot of Δ‑height by interaction
Grasbx1 <- ggplot(delta_GRHeight,
                 aes(x = Treatment, y = delta_GR)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Grass DPM height (cm)") +
  theme_beautiful() #


##saving pre& post treatment BOXPLOT  -  excluding encroachment level
ggsave(Grasbx1,filename ="Plots/3Delta Grass height BOXplot.png",
       width = 16, height = 14, units = "cm") 

### Make "Unfenced" the reference level 

class(delta_GRHeight$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
delta_GRHeight$Fencing <- factor(delta_GRHeight$Fencing, ordered = FALSE)

# Verify
levels(delta_GRHeight$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
delta_GRHeight$Fencing <- relevel(delta_GRHeight$Fencing, ref = "Unfenced")



######GLMM FOR DELTA GRASS HEIGHT
GrasH1 <- glmmTMB(delta_GR ~ Treatment * Fencing + (1|Site) + (1|Plot),  # Crossed effects,
                 data = delta_GRHeight, family = gaussian(link = "identity"))
summary(GrasH1)

#check if model converged
performance::check_convergence(GrasH1) # TRUE the model converged

# MODEL performance
performance::check_model(GrasH1)

# Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(GrasH1))  #p < 0.05 → residuals are non-normal.
# result p = 0.0569, hence residuals are marginally normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(GrasH1), breaks = 5)
leveneTest(resid(GrasH1) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.604 equal variance 



################################ GRASS SPECIES RICHNESS

# 1. Prepare data: richness per Site x Treatment x Year
Grass_rich <- Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025)) %>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop")%>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

##reorder so that pre-treatment appears first then post treatment second on the plots
Grass_rich <- Grass_rich %>%
        mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

#boxplot- GRASS spp richness post vs pre treatment excluding encroachment level
Grrbx<- ggplot(Grass_rich, 
              aes(x = Treatment, y = spp_richness, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  labs(x = "Treatment", 
       y = "Grass species richness") +
  theme_beautiful() 

##saving pre& post treatment BOXPLOT  -  excluding encroachment level
ggsave(Grrbx,filename ="Plots/PP Grass Richness BOXplot.png",
       width = 16, height = 14, units = "cm") 



############################# DELTA GRASS SPP RICHNESS

Grass_F <- Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025)) %>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Year, Fencing) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop")


#Pivot the two years side‑by‑side and compute Δ GRASS RICHNESS ─────────────
grassR_delta <- Grass_F %>%
  pivot_wider(names_from = Year, values_from = spp_richness, names_prefix = "Y") %>%
  mutate(delta = Y2025 - Y2024)

#Visualise: boxplot of Δ‑  species richness by interaction
GrRRbx <- ggplot(grassR_delta,
                  aes(x = Treatment, y = delta)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Grass species richness") +
  theme_beautiful() #


##saving pre& post treatment BOXPLOT  -  excluding encroachment level
ggsave(GrRRbx,filename ="Plots/3Delta Grass Richness BOXplot.png",
       width = 16, height = 14, units = "cm") 


#### Make "Unfenced" the reference level 

class(grassR_delta$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
grassR_delta$Fencing <- factor(grassR_delta$Fencing, ordered = FALSE)

# Verify
levels(grassR_delta$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
grassR_delta$Fencing <- relevel(grassR_delta$Fencing, ref = "Unfenced")


######GLMM FOR DELTA GRASS species richness
GrasS1 <- glmmTMB(delta ~ Treatment * Fencing + (1|Site), #+ (1|Plot),  # Crossed design,
                  data = grassR_delta, family = gaussian(link = "identity"))
summary(GrasS1)

# option 1 using rlmer to deal with non-normality
robust_modelGS <- rlmer(delta ~ Treatment * Fencing + (1|Site),  # Crossed design,
                                            data = grassR_delta)
summary(robust_modelGS)

#check if model converged
performance::check_convergence(robust_modelGS) # TRUE the model converged

# MODEL performance
performance::check_model(GrasS1)

# Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(robust_modelGS))  #p < 0.05 → residuals are non-normal.
# result p = 0.00275, hence residuals are NON- normal

shapiro.test(resid(GrasS1)) # p = 0.01

##QQ plot
qqnorm(resid(GrasS1)) 
qqline(resid(GrasS1))


##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(robust_modelGS), breaks = 5)
leveneTest(resid(robust_modelGS) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.5386 equal variance 



####### LOG TRANSFORMING GRASS SPECIES RICHNESS

# 1) Create signed log transform of Grass diversity
grassR_delta$RDeltaDensity_log <- sign(grassR_delta$delta) * log(abs(grassR_delta$delta) + 1)


# 2) Log Transformed. Fit Gaussian LMM with crossed random effects
##GLMM FOR log Grass diversity
GRMod <- glmmTMB(RDeltaDensity_log ~ Treatment * Fencing  + (1 | Site),
                     data = grassR_delta, family = gaussian(link = "identity"))

summary(GRMod)

# check model performance
performance::check_model(GRMod)

# Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(GRMod))  #p < 0.05 → residuals are non-normal.
# result p = 0.002463, hence residuals are NON- normal

# create a Q-Q plot of your residuals: 
qqnorm(resid(GRMod)) 
qqline(resid(GRMod)) # this is a heavy tailed distribution, with more extreme positive outliers

#B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(GRMod), breaks = 5)
leveneTest(resid(GRMod) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.243 equal variance 



#############################  GRASS SPECIES DIVERSITY  

# Calculate Shannon-Wiener Diversity Index at Site and Plot level
GrSWeiner <- Grasses%>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025))%>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Year, Fencing, Species_name)%>%
  summarise(Spp_count = n(), .groups = "drop" )

## Calculate Shannon-Wiener Diversity Index at Site and Plot level
grSWdiversity <- GrSWeiner %>%
  group_by(Site, Plot, Subplot,Treatment, Year, Fencing) %>%                   # Group by Site and Plot
  summarise(
    Shannon_Diversity = -sum((Spp_count / sum(Spp_count)) * log(Spp_count / sum(Spp_count))),
    .groups = "drop")%>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

##reorder so that pre-treatment appears first then post treatment second on the plots
grSWdiversity <- grSWdiversity %>%
     mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

#Visualisation
GSW <- ggplot(grSWdiversity,
             aes(x = Treatment, y =Shannon_Diversity, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Grass Shannon-Weiner diversity index") +
  theme_beautiful() +
  theme(legend.position = "top")

##ggsave
ggsave(GSW,filename ="Plots/PP Grass S-Weiner index FENCED boxplot.png",
width = 16, height = 14, units = "cm")


##### DELTA SHANNON-WEINER DIVERSITY INDEX
Gdiv <- Grasses%>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025))%>%   # keep only pre/post years
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
SW_Delta <- Sdiversity_data %>% 
  pivot_wider(names_from  = Year,
              values_from = Shannon_Diversity,
              names_glue  = "sw_{Year}") %>% 
  mutate(delta_SW = sw_2025 - sw_2024)   


## Boxplot Grass Shannon_Weiner index
GSWc <- ggplot(SW_Delta,
aes(x = Treatment, y =delta_SW)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Grass Shannon-Weiner diversity index") +
  theme_beautiful() +
  theme(legend.position = "top")

##ggsave
ggsave(GSWc,filename ="Plots/3Delta Grass S-Weiner index FENCED boxplot.png",
       width = 16, height = 14, units = "cm")


###### Make "Unfenced" the reference level 

class(SW_Delta$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
SW_Delta$Fencing <- factor(SW_Delta$Fencing, ordered = FALSE)

# Verify
levels(SW_Delta$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
SW_Delta$Fencing <- relevel(SW_Delta$Fencing, ref = "Unfenced")


######GLMM FOR DELTA GRASS species richness
GrasSW <- glmmTMB(delta_SW ~ Treatment * Fencing + (1|Site) + (1|Plot),  # Crossed effects,
                  data = SW_Delta, family = gaussian(link = "identity"))
summary(GrasSW)

#check if model converged
performance::check_convergence(GrasSW) # TRUE the model converged

# MODEL performance
performance::check_model(GrasSW)

# Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(GrasSW))  #p < 0.05 → residuals are non-normal.
# result p = 0.0000000823, hence residuals are NON- normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(GrasSW), breaks = 5)
leveneTest(resid(GrasSW) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.3499 equal variance 

#QQ plot
qqnorm(resid(GrasSW)) 
qqline(resid(GrasSW))

##### LOG TRANSFORMING GRASS SHANNON WEINER DIVERSITY 

# 1) Create signed log transform of Grass diversity
SW_Delta$DeltaDensity_log <- sign(SW_Delta$delta_SW) * log(abs(SW_Delta$delta_SW) + 1)


# 2) Log Transformed. Fit Gaussian LMM with crossed random effects
##GLMM FOR log Grass diversity
GdiverMod <- glmmTMB(DeltaDensity_log ~ Treatment * Fencing  + (1 | Site) + (1|Plot),
                      data = SW_Delta, family = gaussian(link = "identity"))

summary(GdiverMod)


# Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(GdiverMod))  #p < 0.05 → residuals are non-normal.
# result p = 0.002682, hence residuals are NON- normal

# create a Q-Q plot of your residuals: 
qqnorm(resid(GdiverMod)) 
qqline(resid(GdiverMod)) # this is a heavy tailed distribution, with more extreme positive outliers

#B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(GdiverMod), breaks = 5)
leveneTest(resid(GdiverMod) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.243 equal variance 



# option 2 for dealing with non-normally distributed residuals

# Fit the robust model. The syntax is identical to lmer!
robust_model <- rlmer(delta_SW ~ Treatment * Fencing + (1|Site),  # Crossed effects,
                      data = SW_Delta)

summary(robust_model)

# nromality test
shapiro.test(resid(robust_model))

## MODEL performance
performance::check_model(robust_model)

## create a Q-Q plot of your residuals: 
qqnorm(resid(robust_model)) 
qqline(resid(robust_model))



#######  CHECKING FOR MULTICOLLINEARITY

#Look at cross-tabulation of factors

with(trt_comparison2, table(Treatment, Fencing))

#Check correlation of design matrix

X <- model.matrix(~ Treatment + Fencing, data = trt_comparison2)
cor(X)  # see if columns are highly correlated #the results show that Treatment 
# and fencing are not collinear.The high VIF is almost certainly due 
#to the dummy-variable trap (redundancy from categorical predictors).


#use generalized VIF (GVIF) which is designed for factors with >2 levels:

library(car)
mod <- lm(density_ha ~ Treatment + Fencing, data = trt_comparison2)
v <- vif(mod)

#using adjusted GVIF
# If your model has categorical predictors, vif() returns a matrix with GVIF and Df
# We adjust GVIF using 1/(2*Df)
v_adj <- v[, "GVIF"]^(1/(2*v[, "Df"]))
v_adj

# Reuslts show that adjusted GVIFs are both 1, which is the lowest possible value. This means:
#there is no multicollinearity between Treatment and Fencing in the experimental design.
# The earlier “high VIF” was just an artifact of how car::vif() reports values 
#for multi-level factors (each dummy looks correlated with the
#others, but GVIF corrects for this)


#2. Chi-square test of independence -Checks whether the distribution of fencing differs significantly across Treatment.

chisq.test(table(trt_comparison2$Treatment, trt_comparison2$Fencing))
 # pvalue = 1 - there is no association between treatment and fencing

#Cramér’s V (strength of association) Even if chi-square is significant 
 #(large sample sizes make everything “significant”), 

CramerV(table(trt_comparison2$Treatment, trt_comparison2$Fencing))
 # answer = 0
#Cramér’s V tells you how strong the association is (0 = none, 1 = perfect association).


## TO CHECK IF RANDOM EFFECTS (Site and Plot) are correlated

table(trt_comparison2$Site, trt_comparison2$Plot)
  
#To check how much variation each random effect explains
VarCorr(mod_t)
#If both variance components are >0 → both contribute.
#If one ≈0 → you could drop that random effect.
 # RESULT: site = 0.6, Plot = 0.0000156 


#checking for crossed effects
cat("\nSites and Plot:\n")
print(table(strt_comparison2$Site, strt_comparison2$Plot))  # Crossed structure check
  
## Checking for groups and if they are balanced
table(strt_comparison2$Site) # <5 groups not recommended

# Check for balance random effects
strt_comparison2 %>%
  count(Site, Year) %>%
  print(n = Inf)  # Show all combinations


# Check factor levels on Fixed effects
table(strt_comparison2$Treatment)
table(strt_comparison2$Fencing)

# Ensure factors are properly coded
str(strt_comparison2$Treatment)  # Should be factor, not character

# convert from character to factor
# Convert to factor
SapF$Treatment <- as.factor(SapF$Treatment)

# Ensure factors are properly coded
str(SapF$Treatment)

str(strt_comparison2)


# 2. Basic structure
cat("\n2. DATA STRUCTURE\n")
cat("   Total observations:", nrow(strt_comparison2), "\n")
cat("   Variables:", ncol(strt_comparison2), "\n")


# Rule of thumb: need multiple observations per group
observations_per_group <- strt_comparison2 %>%
  group_by(Site) %>%
  summarise(n = n())






