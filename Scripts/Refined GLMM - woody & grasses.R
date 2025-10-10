#### this script analyses data,with the TFB treatment excluded. 
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
library(marginaleffects)
library(effects)
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

#Filter SEEDLINGS  
Seedlings <- SapF %>% 
  filter(woody_cat == "Seedlings", Year %in% c(2024, 2025),
         !Treatment %in% c("TFB")) %>% 
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

## increasing font size for x and y axis
Seedpp3<- ggplot(strt_comparison2, 
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

##saving pre& post treatment BOXPLOT  -  excluding TFB
 #ggsave(Seedpp3,filename ="Plots/TFB PP Seedling Density BOXplot.png",
       width = 16, height = 14, units = "cm")  



#################################DELTA SEEDLING DENSITY

#Pivot the two years side‑by‑side and compute Δ seedling density ─────────────
Seedlings_Delta1 <- Seed_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Seeddens = dens_2025 - dens_2024) 


## increased font size for x and y axis
Seedbxp2 <- ggplot(Seedlings_Delta1,
                   aes(x = Treatment, y = delta_Seeddens)) + facet_wrap(~ Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in Seedling density per ha") +
  theme_beautiful() +
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

##saving Delta treatment BOXPLOT  -  excluding TFB
ggsave(Seedbxp2,filename ="Plots/TFB DELTA Seedling Density BOXplot.png",
       width = 16, height = 14, units = "cm") 


##### GLMM to test effect of treatment * fencing on seedling density###################

# Make "Unfenced" the reference level 

class(Seedlings_Delta1$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
Seedlings_Delta1$Fencing <- factor(Seedlings_Delta1$Fencing, ordered = FALSE)

# Verify
levels(Seedlings_Delta1$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
Seedlings_Delta1$Fencing <- relevel(Seedlings_Delta1$Fencing, ref = "Unfenced")


#GLMM for seedling density 
Seedl4 <- glmmTMB(delta_Seeddens ~ Treatment * Fencing + (1|Site) + (1|Plot),  # Crossed effects,
                data = Seedlings_Delta1, family = gaussian(link = "identity"))
summary(Seedl4)


## check model performance

performance::check_model(Seedl4)


### POST HOC ANALYSIS FOR SEEDLINGS 

# Tukey HSD pairwise comparisons
treat_comparisons <- emmeans(Seedl4, specs = pairwise ~ Treatment | Fencing, adjust = "tukey")
summary(treat_comparisons$contrasts)

### Margin effects for seedlings

# Get the fixed effects coefficients
model_summary <- summary(Seedl4)
fixed_effects <- model_summary$coefficients$cond

cat("Fixed Effects (Marginal Effects for Gaussian Model):\n")
print(fixed_effects)


########################################SAPLING DENSITY  SAPLING DENSITY SAPLING
#Filter SAPLINGS  
Saplings <- SapF %>% 
  filter(woody_cat == "Saplings", Year %in% c(2024, 2025),
         !Treatment %in% c("TFB")) %>% 
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


# boxplot- SAPLINGS density post vs pre treatment TFB
Sapp1<- ggplot(sptrt_comparison2, 
              aes(x = Treatment, y = density_ha, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  labs(x = "Treatment", 
       y = "Saplings density per ha") +
  theme_beautiful() +
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

##saving pre& post treatment BOXPLOT  -  excluding encroachment level
ggsave(Sapp1,filename ="Plots/PP TFB Saplings Density BOXplot.png",
       width = 16, height = 14, units = "cm")  



################################# DELTA SAPLING DENSITY

#Pivot the two years side‑by‑side and compute Δ sapling density
Saplings_Delta <- Sap_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Sapsdens = dens_2025 - dens_2024) 

#Visualise: boxplot of Δ‑ Sapling DENSITY by interaction
Sapbxp1 <- ggplot(Saplings_Delta,
                 aes(x = Treatment, y = delta_Sapsdens)) + facet_wrap(~ Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in saplings density per ha") +
  theme_beautiful() +
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )
  

##saving BOXPLOT - sapling density 
ggsave(Sapbxp1,filename ="Plots/3 TFB Delta Sapling Density BOXplot.png",
       width = 16, height = 14, units = "cm")  


##### GLMM to test effect of treatment * fencing on sapling density###################

# Make "Unfenced" the reference level 

class(Saplings_Delta$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
Saplings_Delta$Fencing <- factor(Saplings_Delta$Fencing, ordered = FALSE)

# Verify
levels(Saplings_Delta$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
Saplings_Delta$Fencing <- relevel(Saplings_Delta$Fencing, ref = "Unfenced")

#GLMM for sapling density 
Sapl4 <- glmmTMB(delta_Sapsdens ~ Treatment * Fencing + (1|Site),  
                  data = Saplings_Delta, family = gaussian(link = "identity"))
summary(Sapl4)


### POST HOC ANALYSIS FOR SAPLINGS 

# Tukey HSD pairwise comparisons
treat_comparisons <- emmeans(Sapl4, specs = pairwise ~ Treatment | Fencing, adjust = "tukey")
summary(treat_comparisons$contrasts)

############################ RESPROUTS  RESPROUTS RESPROUTS ####################

# Step 1: Filter Cut stumps only and years 
resprouts_df <- SapF %>%
  filter(
    woody_cat == "Cut stump",
    Year %in% c(2025),
    !Treatment %in% c("C", "F", "TFB")  # exclude the three treatments
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
Respbx1 <- ggplot(resprouts_df,
                 aes(x = Treatment, y = No_of_resprouts)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Average no.of resprouts per cut stump") +
  theme_beautiful() +
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )


##saving BOXPLOT - INTERACTIONS
ggsave(Respbx1,filename ="Plots/PP TFB RESPROUTS BOXplot.png",
       width = 16, height = 14, units = "cm")  


##GLMM for resprouts on cut stumps  
Respr4 <- glmmTMB(No_of_resprouts ~ Treatment * Fencing + (1|Site),  
                 data = resprouts_df, family = gaussian(link = "identity"))
summary(Respr4)

### POST HOC ANALYSIS FOR RESPROUTS 
# Tukey HSD pairwise comparisons
treat_comparisons3 <- emmeans(Respr4, specs = pairwise ~ Treatment | Fencing, adjust = "tukey")
summary(treat_comparisons3$contrasts)



################################################################################################
## GRASSES     GRASSES   GRASSES GRASSES

Grasses <- read_csv("DATA/March2025/GrassesCombinedCleaned2.csv")

summary_Gr <- Grasses %>%
  filter(!is.na(DPM_Height),
         Year %in% c(2024, 2025),!Treatment %in% c("TFB")) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_DPM_Height = mean(DPM_Height, na.rm = TRUE)) %>%
  ungroup() 


# Calculate mean grass height for pre and post treatment
grass_height <- Grasses %>%
  filter(!is.na(DPM_Height),
         Year %in% c(2024, 2025),!Treatment %in% c("TFB"))%>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_DPM_Height = mean(DPM_Height, na.rm = TRUE),
            #n_observations = n(),
            .groups = "drop") %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

##reorder so that pre-treatment appears first then post treatment second on the plots
grass_height <- grass_height %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

## boxplot- GRASS DPM Height post vs pre treatment excluding TFB
Grbx1<- ggplot(grass_height, 
              aes(x = Treatment, y = mean_DPM_Height, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  labs(x = "Treatment", 
       y = "Grass DPM height (cm)") +
  theme_beautiful() +
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

##saving pre& post treatment BOXPLOT  -  excluding encroachment level
ggsave(Grbx1,filename ="Plots/PP TFB Grass height BOXplot.png",
       width = 16, height = 14, units = "cm") 


######DELTA GRASS HEIGHT

# Take the mean height within each grouping for each year before differencing.
delta_GRHeight <- Grasses  %>%
  filter(Year %in% c(2024, 2025),
         !Treatment %in% c("TFB")) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_DPM_Height = mean(DPM_Height, na.rm = TRUE), .groups = "drop_last") %>%
  pivot_wider(
    names_from  = Year,
    values_from = mean_DPM_Height,
    names_glue  = "height_{Year}"
  ) %>%
  mutate(delta_GR = height_2025 - height_2024) %>%
  drop_na(delta_GR)   # keep groups where both years are present


#Visualise: boxplot of Δ‑grass height. FONT INCREASED
Grasbx2 <- ggplot(delta_GRHeight,
                  aes(x = Treatment, y = delta_GR)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in Grass DPM height (cm)") +
  theme_beautiful()+
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

##saving pre& post treatment BOXPLOT  -  excluding encroachment level
ggsave(Grasbx2,filename ="Plots/3 TFB Delta Grass height BOXplot.png",
       width = 16, height = 14, units = "cm") 

###### GLMM to test effect of treatment * fencing on Grass height ##################

# Make "Unfenced" the reference level 

class(delta_GRHeight$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
delta_GRHeight$Fencing <- factor(delta_GRHeight$Fencing, ordered = FALSE)

# Verify
levels(delta_GRHeight$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
delta_GRHeight$Fencing <- relevel(delta_GRHeight$Fencing, ref = "Unfenced")

#GLMM for sapling density 
Grashg <- glmmTMB(delta_GR ~ Treatment * Fencing + (1|Site),  
                 data = delta_GRHeight, family = gaussian(link = "identity"))
summary(Grashg)


#################################### GRASS SPECIES RICHNESS

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

#boxplot- GRASS spp richness post vs pre treatment excluding encroachment level
Grrbx1<- ggplot(Grass_rich, 
               aes(x = Treatment, y = spp_richness, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  labs(x = "Treatment", 
       y = "Grass species richness") +
  theme_beautiful() +
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

##saving pre& post treatment BOXPLOT  -  excluding TFB
ggsave(Grrbx1,filename ="Plots/PP TFB Grass Richness BOXplot.png",
       width = 16, height = 14, units = "cm") 



############################# DELTA GRASS SPP RICHNESS

Grass_F <- Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025),!Treatment %in% c("TFB")) %>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Year, Fencing) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop")


#Pivot the two years side‑by‑side and compute Δ GRASS RICHNESS ─────────────
grassR_delta <- Grass_F %>%
  pivot_wider(names_from = Year, values_from = spp_richness, names_prefix = "Y") %>%
  mutate(delta = Y2025 - Y2024)

#Visualise: boxplot of Δ‑  species richness by interaction
GrRRbx2 <- ggplot(grassR_delta,
                 aes(x = Treatment, y = delta)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in grass species richness") +
  theme_beautiful()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))


##saving pre& post treatment BOXPLOT  -  excluding encroachment level
ggsave(GrRRbx2,filename ="Plots/3 TFB Delta Grass Richness BOXplot.png",
       width = 16, height = 14, units = "cm") 


#################################  GRASS SPECIES DIVERSITY  

# Calculate Shannon-Wiener Diversity Index at Site and Plot level
GrSWeiner <- Grasses%>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025),!Treatment %in% c("TFB"))%>%   # keep only pre/post years
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
GSW1 <- ggplot(grSWdiversity,
              aes(x = Treatment, y =Shannon_Diversity, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Grass Shannon-Weiner diversity index") +
  theme_beautiful() +
  theme(legend.position = "top") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

##ggsave
ggsave(GSW1,filename ="Plots/PP TFB Grass S-Weiner index FENCED boxplot.png",
       width = 16, height = 14, units = "cm")


####### DELTA SHANNON-WEINER DIVERSITY INDEX
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
SW_Delta <- Sdiversity_data %>% 
  pivot_wider(names_from  = Year,
              values_from = Shannon_Diversity,
              names_glue  = "sw_{Year}") %>% 
  mutate(delta_SW = sw_2025 - sw_2024)   


## Boxplot Grass Shannon_Weiner index
GSWc1 <- ggplot(SW_Delta,
               aes(x = Treatment, y =delta_SW)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Chnage in grass Shannon-Weiner diversity index") +
  theme_beautiful() +
  theme(legend.position = "top") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

##ggsave
#ggsave(GSWc1,filename ="Plots/3 TFB Delta Grass S-WeinerFENCED boxplot.png",
       width = 16, height = 14, units = "cm")


###### GLMM to test effect of treatment * fencing on Grass diversity ##################

# Make "Unfenced" the reference level 

class(SW_Delta$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
SW_Delta$Fencing <- factor(SW_Delta$Fencing, ordered = FALSE)

# Verify
levels(SW_Delta$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
SW_Delta$Fencing <- relevel(SW_Delta$Fencing, ref = "Unfenced")

#GLMM for sapling density 
Grasdiv <- glmmTMB(delta_SW ~ Treatment * Fencing + (1|Site)+ (1|Plot),  
                  data = SW_Delta, family = gaussian(link = "identity"))

summary(Grasdiv)


## check model performance
performance::check_model(Grasdiv)

############# POST HOC ANALYSIS FOR GRASS DIVERSITY 
# Tukey HSD pairwise comparisons
Grass_diversity <- emmeans(Grasdiv, specs = pairwise ~ Treatment | Fencing, adjust = "tukey")
summary(Grass_diversity$contrasts)


##################Marginal effects for Grass diversity
#Check Your Model First

# Check model summary
summary(Grasdiv)

# Check the formula
formula(Grasdiv)

# Check if it's really a Gaussian model
family(Grasdiv)
## Check what type of model this really is
class(Grasdiv)   #=glmmTMB

