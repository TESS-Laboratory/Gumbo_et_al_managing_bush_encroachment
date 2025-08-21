## LOAD DATA

SapF <- read_csv("DATA/March2025/WoodyPC2.csv")

## create seedling, sapling, trees and cut-stump row
SapF <- SapF %>% 
  mutate(
    woody_cat = case_when(
      Woody_class == "Cut stump"          ~ "Cut stump",
      between(`Max_height(m)`, 0.05, 0.50)          ~ "Seedlings",
      between(`Max_height(m)`, 0.51, 1.49)          ~ "Saplings",
      between(`Max_height(m)`, 1.5, 21.0)           ~ "Trees",
      TRUE                             ~ NA_character_
    )
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
mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))#,
#Period = factor(Period, levels = c("Pre-treatment", "Post-treatment"))
                

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
  
### Make "Unfenced" the reference level (to see "fenced" coefficients)
  # Check current class of FieldType
  
  class(trt_comparison2$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
trt_comparison2$Fencing <- factor(trt_comparison2$Fencing, ordered = FALSE)

# Verify
levels(trt_comparison2$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level (to see "Unfenced" coefficients)
trt_comparison2$Fencing <- relevel(trt_comparison2$Fencing, ref = "Unfenced")


##GLMM FOR TREE DENSITY 
Treeden <- glmmTMB(density_ha ~ Treatment * Fencing  + (1 | Site),
data = trt_comparison2, family = gaussian(link = "identity"))

summary(Treeden)
tidy (Treeden)

# check for model residuals and distribution
qqnorm(resid(Treeden))
qqline(resid(Treeden))

# For comprehensive diagnostics:
performance::check_convergence(Treeden) # TRUE the model converged

## MODEL performance
performance::check_model(Treeden)

##
VIF(Treeden) ## CHECK ON THIS,  

##Check for outliers
residuals_std <- scale(resid(Treeden))  # Standardize residuals
outliers <- which(abs(residuals_std) > 3)  # Find outliers
print(outliers)


## Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(Treeden))  #p < 0.05 → residuals are non-normal.
# result p = 0.002547, hence residuals are non-normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(Treeden), breaks = 5)
leveneTest(resid(Treeden) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.0834 unequal variance 


###
# Prepare the data for mixed model analysis
mixed_data <- Treesub %>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre", "Post"),
         Period = factor(Period, levels = c("Pre", "Post")),
         Treatment = factor(Treatment),
         Plot = factor(Plot),
         Year = factor(Year),
         Fencing = factor(Fencing),
         Site = factor(Site),
         # Create unique plot ID for random effects
         Plot_ID = interaction(Site, Plot))

# View data structure
glimpse(mixed_data)

# Option 1: Full model with interaction and random effects
model1 <- lmer(density_ha ~ Treatment * Fencing * Period + 
                 (1|Site) + (1|Plot_ID), 
               data = mixed_data)

# Summary of the full model
summary(model1)


## using glmmTMB
model1a <- glmmTMB(density_ha ~ Treatment * Fencing * Period + 
(1|Site) + (1|Plot_ID), 
data = mixed_data)

model2 <- glmmTMB(density_ha ~ Treatment * Fencing * Period + (1|Site/Plot), data = mixed_data)
model3 <- glmmTMB(density_ha ~ Treatment * Fencing * Period + (1|Site:Plot), data = mixed_data)


# Summary of the full model
summary(model1a)
summary(model2)



# For comprehensive diagnostics:
performance::check_convergence(model1) # TRUE the model converged

## MODEL performance
performance::check_model(model1)

##
VIF(model1) ## CHECK ON THIS,  

##Check for outliers
residuals_std <- scale(resid(model1))  # Standardize residuals
outliers <- which(abs(residuals_std) > 3)  # Find outliers
print(outliers)


## Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(model1))  #p < 0.05 → residuals are non-normal.
# result p = 0.000625, hence residuals are non-normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(model1), breaks = 5)
leveneTest(resid(model1) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.0862 equal variance 




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



#########SEEDLING DENSITY SEEDLING DENSITY SEEDLING DENSITY

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
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))#,
#Period = factor(Period, levels = c("Pre-treatment", "Post-treatment"))

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

##saving pre& post treatment BOXPLOT  -  excluding encroachment level
ggsave(Seedpp,filename ="Plots/PP Seedling Density BOXplot.png",
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
  labs(x = "Treatment", y = "Δ Seedling density per ha") +
  theme_beautiful() #

##saving BOXPLOT - seedling density 
ggsave(Seedbxp,filename ="Plots/3Delta Seedling Density INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")  


################################ SAPLING SAPLING SAPLINGS SAPLINGS 
#Filter SEEDLINGS  
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

#Pivot the two years side‑by‑side and compute Δ seedling density ─────────────
Saplings_Delta <- Sap_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Sapsdens = dens_2025 - dens_2024) 

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


###### SAPLING HEIGHT SAPLING HEIGHT

#### Step 1: Filter SAPLINGS only and years 
Sapdata  <- SapF %>%
  filter(
    woody_cat == "Saplings",
    Year %in% c(2024, 2025)
  )         

#sort pre and post treatment
sapl_comparison2 <- Sapdata %>%
  summarise(mean_height = mean(`Max_height(m)`, na.rm = TRUE), .groups = "drop_last")%>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))



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



################################ GRASS SPECIES RICHNESS

# 1. Prepare data: richness per Site x Treatment x Year
Grass_rich <- Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025)) %>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop")%>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

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

