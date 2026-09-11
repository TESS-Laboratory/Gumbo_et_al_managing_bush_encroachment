############# DETERMINING WOODY PLANTS DENSITY and GRASS LAYER

library(MASS)
library(tidyverse)
library(performance)  # model diagnostics
library(ggpubr)
library(RColorBrewer)
library(scales)
library(multcompView)
library(patchwork)
library(lmerTest)
library(Matrix)
library(lme4)
library(emmeans)
library(robustbase)
library(sjPlot)
library(flextable)
library(officer)
library(glmmTMB)
library(DHARMa)   # model diagnostics GLMM
library(car)
library(openxlsx)
library(broom.mixed) #convert model objects to data frames
library(corrplot)
library(DescTools)
library(effectsize) # For easy centering
library(marginaleffects)
library(effects)
library(ggeffects)
library(knitr)  # for table
library(gtsummary) # for descriptive statistics tables
library(multcomp)
library(vegan)



####################################### DETERMINING WOODY PLANTS DENSITY

# Loading data
A <- read_csv("DATA/GEODE_Subplot_area.csv")
B <- read.csv("DATA/March2026/WOODYP2426.csv", stringsAsFactors = FALSE)


# Ensuring consistent column names (case-sensitive)
colnames(A) <- c("Site", "Plot", "Subplot", "Area")


# Merge Area into B
B_merged <- B %>%
  dplyr::left_join(
    A %>% dplyr::select(Site, Plot, Subplot, Area),
    by = c("Site", "Plot", "Subplot")
  )

# preparing data for trees
SapF <- B_merged %>% 
  mutate(
    woody_cat = case_when(
      Woody_class == "Cut stump"         ~ "Cut stump",
      between(Max_height.m., 1.5, 21.0) ~ "Trees",
      TRUE                               ~ NA_character_
    ),
    Treatment = factor(Treatment),
    Fencing = factor(Fencing)
  )

## TREE DENSITY 

# calculating tree density
Trees <- SapF %>%
  filter(
    woody_cat == "Trees",
    Year %in% c(2024, 2026)) %>%
  count(
    Site, Plot, Subplot, Treatment, Fencing, Year, Area,
    name = "Trees"
  ) %>%
  mutate(
    density_ha = Trees * 10000 / Area
  )


#comparing at treatment level

Trees_treat <- Trees %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>% 
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop")  


#Summary stats for seedlings 
Treessummary_stats <- Trees %>%
  group_by(Treatment,Fencing,Year) %>%
  summarise(
    N = n(),  # number of observations per treatment
    mean_density = mean(density_ha, na.rm = TRUE),
    sd_density = sd(density_ha, na.rm = TRUE)
  ) %>%
  ungroup()

#sorting pre and post treatment
tree_comparison2 <- Trees %>%
  filter(Year %in% c(2024, 2026)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


#reorder so that pre-treatment appears first then post treatment second on the plots
tree_comparison2 <- tree_comparison2 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

##################################DELTA TREE DENSITY

#Pivoting the two years side‑by‑side and compute Δ TREE density ─────────────
Trees_Delta1 <- Trees_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Treedens = dens_2026 - dens_2024) 



##### to test effect of treatment * fencing on tree density###################

# Making "Unfenced" the reference level 

class(Trees_Delta1$Fencing)  # Likely "character" or "ordered factor"

# Converting to unordered factor explicitly
Trees_Delta1$Fencing <- factor(Trees_Delta1$Fencing, ordered = FALSE)

# Verifying
levels(Trees_Delta1$Fencing)  


### Convert character variables to factors
Trees_Delta1$Treatment <- as.factor(Trees_Delta1$Treatment)


Trees_Delta1$Fencing <- factor(Trees_Delta1$Fencing, 
                               levels = c("Unfenced", "Fenced"),
                               labels = c("Unfenced", "Fenced"))

# Set "Fenced" as the reference level 
Trees_Delta1$Fencing <- relevel(Trees_Delta1$Fencing, ref = "Unfenced")

# using the LMM for analysis
Treel5 <- lmer(delta_Treedens ~ Treatment * Fencing + (1|Site),  
               data = Trees_Delta1)


summary(Treel5)


## increasing font size for x and y axis

tree_comparison2$Fencing <- factor( tree_comparison2$Fencing,
                                    levels = c("Unfenced", "Fenced")) #ordering Fencing level to start with Unfenced


Treea<- ggplot(tree_comparison2, 
               aes(x = Treatment, y = density_ha, fill = Fencing)) + facet_wrap(~Period)+ 
  geom_violin(trim = TRUE)+
  #geom_hline(yintercept = 0, linetype = "dashed") +    
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1.4, color = "black") +
  labs(x = "Treatment", 
       # y = "Seedlings density per ha",
       y = expression("Tree density "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8)        # Axis tick labels
  ) +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))


## violin plot tree delta
Treeb <- ggplot(Trees_Delta1,
                aes(x = Treatment, y = delta_Treedens, fill = Fencing))+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       #y = "Change in Seedlings density per ha",
       y = expression("Δ Tree density "*ha^{-1}*"")
  ) +  
  scale_y_continuous( breaks = seq(-10000, 10000, 2000))+
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),  # Axis titles reduced from 12 to 8
    axis.text = element_text(size = 8)) +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta1"))



########## LOG RESPONSE RATIO (lnRR) — effect size relative to Control        


# Preparing tree count data for lnRR
TreeLOG <- SapF %>%
  filter(
    woody_cat == "Trees",
    Year %in% c(2024, 2026)
  ) %>%
  count(Site, Plot, Subplot, Treatment, Fencing, Year, Area, name = "Trees") %>%
  mutate(density_ha = Trees * 10000 / Area)

# Converting to wide format (Y2024 = pre-treatment, Y2026 = post-treatment)
Tree_wide <- TreeLOG %>%
  dplyr::select(Site, Plot, Subplot, Treatment, Fencing, Year, Area, density_ha) %>%
  pivot_wider(names_from = Year, values_from = density_ha, names_prefix = "Y")

# Control means per Site × Fencing (used as reference baseline)
Tcontrol_data <- Tree_wide %>%
  filter(Treatment == "C") %>%
  group_by(Site, Fencing) %>%
  summarise(
    C_pre  = mean(Y2024, na.rm = TRUE),
    C_post = mean(Y2026, na.rm = TRUE),
    .groups = "drop"
  )

# Join control means to treatment plots
Tree_lnRR <- Tree_wide %>%
  filter(Treatment != "C") %>%
  left_join(Tcontrol_data, by = c("Site", "Fencing"))

# Compute lnRR
Tree_lnRR <- Tree_lnRR %>%
  mutate(lnRR = log((Y2026 / Y2024) / (C_post / C_pre)))

# Treatment × Fencing summaries
Tree_summary <- Tree_lnRR %>%
  group_by(Treatment, Fencing) %>%
  summarise(
    mean_lnRR = mean(lnRR, na.rm = TRUE),
    sd_lnRR   = sd(lnRR, na.rm = TRUE),
    n         = n(),
    se_lnRR   = sd_lnRR / sqrt(n),
    .groups   = "drop"
  )

# Mixed-effects model on lnRR
Treelog <- lmer(lnRR ~ Treatment * Fencing + (1 | Site), data = Tree_lnRR)
summary(Treelog)

# Model diagnostics
 # qqnorm(residuals(Treelog))
 # qqline(residuals(Treelog))
 # check_model(Treelog, check = "qq")
 # check_model(Treelog, check = "normality")
 # check_model(Treelog, check = "homogeneity")
 # check_collinearity(Treelog)


###########################################################################
### PERCENTAGE CHANGE relative to Control (from lnRR emmeans)          ###
###########################################################################

Tremm_interaction <- emmeans(Treelog, ~ Treatment | Fencing)


Tplot_data <- as.data.frame(Tremm_interaction) %>%
  mutate(
    pct_change   = (exp(emmean)   - 1) * 100,
    CI_lower_pct = (exp(lower.CL) - 1) * 100,
    CI_upper_pct = (exp(upper.CL) - 1) * 100
  ) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(
    Treatment = factor(Treatment, levels = c("F", "TF", "TFB", "THF")),
    Fencing   = factor(Fencing,   levels = c("Unfenced", "Fenced"))
  )

# Horizontal forest plot: % change in tree density relative to Control
Treed <- ggplot(Tplot_data, aes(x = pct_change, y = Treatment, color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.5) +
  geom_point(size = 2.0, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
                 height = 0.5, linewidth = 0.9, position = position_dodge(0.5)) +
  scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
  scale_x_continuous(breaks = seq(-80, 100, 20)) +
  labs(
    x     = "Change in tree density relative to Control (%)",
    y     = "Treatment",
    color = "Fencing"
  ) +
  theme_classic() +
  ggtitle(NULL) +
  theme(
    axis.title = element_text(size = 8),
    axis.text  = element_text(size = 8)
  )


#### MULTI-PANEL FIGURE                                                 
# (a) Treea  – raw density by Period (pre vs post), coloured by Fencing
# (b) Treeb  – Δ tree density violin by Fencing
# (d) Treed  – % change relative to Control (lnRR-based forest plot)

multi_panelTree <- (Treea / Treeb / Treed) +
  plot_layout(heights = c(1, 1, 1, 1)) +
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 8, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 10),        # Increase axis label font size
    axis.title = element_text(size = 12),       # Increase axis title font size
    plot.tag = element_text(size = 10, hjust = 0)  # Ensure left alignment
  )

## saving plot
ggsave(multi_panelTree, filename = "Plots/Tree density.png",
       width = 16, height = 14, units = "cm")


###################
######### EFFECT OF FENCING  

emm_tree_fence_by_trt <- emmeans(Treel5, ~ Fencing | Treatment)

fencing_tree_within_trt <- pairs(
  emm_tree_fence_by_trt,
  reverse = TRUE,   # Fenced - Unfenced (positive = Fenced higher Δ density)
  adjust  = "holm"  
)

print(fencing_tree_within_trt)
confint(fencing_tree_within_trt)


######################################################################################
#######################################################################################

#---------------------------------------------------
# 1. Calculate SEEDLING, SAPLING DENSITY
#---------------------------------------------------


####################################### DETERMINING WOODY PLANTS DENSITY 
#  data for seedlings
SapF <- B_merged %>% 
  mutate(
    woody_cat = case_when(
      Woody_class == "Cut stump"          ~ "Cut stump",
      between(Max_height.m., 0.05, 0.50)          ~ "Seedlings",
      between(Max_height.m., 0.51, 1.49)          ~ "Saplings",
      between(Max_height.m., 1.5, 25.0)           ~ "Trees",
      TRUE                             ~ NA_character_
    ),
    Treatment = factor(Treatment),
    Fencing = factor(Fencing)
  )

# calculating seedling density
Seedlings <- SapF %>%
  filter(
    woody_cat == "Seedlings",
    Year %in% c(2024, 2026)) %>%
  count(
    Site, Plot, Subplot, Treatment, Fencing, Year, Area,
    name = "Seedlings"
  ) %>%
  mutate(
    density_ha = Seedlings * 10000 / Area
  )


### Calculating n for each Treatment × Fencing combination
sample_sizes <- Seedlings %>%
  group_by(Treatment, Fencing) %>%
  summarise(
    n = n(),
    .groups = 'drop'
  )

#comparing at treatment level

Seed_treat <- Seedlings %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>% 
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop")  


#Summary stats for seedlings 
Seedsummary_stats <- Seedlings %>%
  group_by(Treatment,Fencing,Year) %>%
  summarise(
    N = n(),                                   # number of observations per treatment
    mean_density = mean(density_ha, na.rm = TRUE),
    sd_density = sd(density_ha, na.rm = TRUE)
  ) %>%
  ungroup()

#sorting pre and post treatment
strt_comparison2 <- Seedlings %>%
  filter(Year %in% c(2024, 2026)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


#reordering so that pre-treatment appears first then post treatment second on the plots
strt_comparison2 <- strt_comparison2 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

##################################DELTA SEEDLING DENSITY

#Pivoting the two years side‑by‑side and computing Δ seedling density ─────────────
Seedlings_Delta1 <- Seed_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Seeddens = dens_2026 - dens_2024) 



##### to test effect of treatment * fencing on seedling density###################

# Making "Unfenced" the reference level 

class(Seedlings_Delta1$Fencing)  # Likely "character" or "ordered factor"

# Converting to unordered factor explicitly
Seedlings_Delta1$Fencing <- factor(Seedlings_Delta1$Fencing, ordered = FALSE)

# Verify
levels(Seedlings_Delta1$Fencing)  


### Converting character variables to factors
Seedlings_Delta1$Treatment <- as.factor(Seedlings_Delta1$Treatment)

Seedlings_Delta1$Fencing <- factor(Seedlings_Delta1$Fencing, 
                            levels = c("Fenced", "Unfenced"),
                            labels = c("Fenced", "Unfenced"))

# Set "Unfenced" as the reference level 
Seedlings_Delta1$Fencing <- relevel(Seedlings_Delta1$Fencing, ref = "Unfenced")



# using the LMM for analysis
Seedl5 <- lmer(delta_Seeddens ~ Treatment * Fencing + (1|Site),  
               data = Seedlings_Delta1)

summary(Seedl5)

## checking model performance

 #####performance::check_model(Seedl5) # this one not displaying plots

 #check_model(Seedl5, check = "qq")
 #check_model(Seedl5, check = "normality")
 #check_model(Seedl5, check = "homogeneity")
 #plot(Seedl5)

## GGplots

strt_comparison2$Fencing <- factor( strt_comparison2$Fencing,
  levels = c("Unfenced", "Fenced")) #ordering Fencing level to start with Unfenced


SeedVa<- ggplot(strt_comparison2, 
                    aes(x = Treatment, y = density_ha, fill = Fencing)) + facet_wrap(~Period)+ 
  geom_violin(trim = TRUE)+
  #geom_hline(yintercept = 0, linetype = "dashed") +    
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1.4, color = "black") +
  labs(x = "Treatment", 
       # y = "Seedlings density per ha",
       y = expression("Seedling density "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 9),      # Axis titles
    axis.text = element_text(size = 9)        # Axis tick labels
  ) +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))


## violin plot seedling delta
SeedVb <- ggplot(Seedlings_Delta1,
                      aes(x = Treatment, y = delta_Seeddens, fill = Fencing))+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1.4, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       #y = "Change in Seedlings density per ha",
       y = expression("Δ Seedling density "*ha^{-1}*"")
  ) +  
  scale_y_continuous( breaks = seq(-10000, 10000, 2000))+
  theme_classic() +
  theme(
    axis.title = element_text(size = 9),  # Axis titles reduced from 12 to 8
    axis.text = element_text(size = 9)) +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))



# calculating seedling density FOR LOG RATIO 
Seedlings <- SapF %>%
  filter(
    woody_cat == "Seedlings",
    Year %in% c(2024, 2026)) %>%
  count(
    Site, Plot, Subplot, Treatment, Fencing, Year, Area,
    name = "Seedlings"
  ) %>%
  mutate(
    density_ha = Seedlings * 10000 / Area)


# preparing seedlings data for LOG RESPONSE RATIO
SeedLOG <- SapF %>%
  filter(
    woody_cat == "Seedlings",
    Year %in% c(2024, 2026)
  ) %>%
  count(
    Site,
    Plot,
    Subplot,
    Treatment,
    Fencing,
    Year,
    Area,
    name = "Seedlings"
  ) %>%
  mutate(
    density_ha = Seedlings * 10000 / Area
  )

#---------------------------------------------------
# 2. Convert to wide format
#---------------------------------------------------

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
   
# Result:
# Y2024 = pretreatment density
# Y2026 = posttreatment density

#---------------------------------------------------
# 3. Extract control values
#---------------------------------------------------

control_data <- Seed_wide %>%
  filter(Treatment == "C") %>%
  dplyr::select(
    Site,
    Plot,
    Subplot, Fencing,
    C_pre = Y2024,
    C_post = Y2026
  )

##Create control means
control_data <- Seed_wide %>%
  filter(Treatment == "C") %>%
  group_by(Site, Fencing) %>%
  summarise(
    C_pre = mean(Y2024, na.rm = TRUE),
    C_post = mean(Y2026, na.rm = TRUE),
    .groups = "drop"
  )

#---------------------------------------------------
# 4. Join control data to treatments
#---------------------------------------------------
SSeed_lnRR <- Seed_wide %>%
  filter(Treatment != "C") %>%
  left_join(
    control_data,
    by = c("Site", "Fencing")
  )

#---------------------------------------------------
# 5. Handle zeros
#---------------------------------------------------

# seed_lnRR <- seed_lnRR %>%
#   mutate(
#     Y2014 = ifelse(Y2024 == 0, 0.001, 2024),
#     Y2016 = ifelse(Y2026 == 0, 0.001, Y2026),
#     C_pre = ifelse(C_pre == 0, 0.001, C_pre),
#     C_post = ifelse(C_post == 0, 0.001, C_post)
#   )

#---------------------------------------------------
# 6. Calculate Log Response Ratio
#---------------------------------------------------

SSeed_lnRR <- SSeed_lnRR %>%
  mutate(
    lnRR = log(
      (Y2026 / Y2024) /
        (C_post / C_pre)
    )
  )

#---------------------------------------------------
# 7. View results
#---------------------------------------------------

print(SSeed_lnRR)

#---------------------------------------------------
# 8. Treatment summaries
#---------------------------------------------------

seedlingD_summary <- SSeed_lnRR %>%
  group_by(Treatment, Fencing) %>%
  summarise(
    mean_lnRR = mean(lnRR, na.rm = TRUE),
    sd_lnRR = sd(lnRR, na.rm = TRUE),
    n = n(),
    se_lnRR = sd_lnRR / sqrt(n)
  )

#print(seedlingD_summary)


### Calculating n for each Treatment × Fencing combination
sample_sizes <- SSeed_lnRR %>%
  group_by(Treatment, Fencing) %>%
  summarise(
    n = n(),
    .groups = 'drop'
  )

#---------------------------------------------------
# 9. Mixed-effects model
#---------------------------------------------------
Seedllog <- lmer(lnRR  ~ Treatment * Fencing + (1 | Site),
  data = SSeed_lnRR)

summary(Seedllog)

#Residuals 
check_model(Seedllog, check = "qq")
check_model(Seedllog, check = "normality")
check_model(Seedllog, check = "homogeneity")
plot(Seedllog)

#---------------------------------------------------
# 10. Plot lnRR
#---------------------------------------------------

##### OPTION 2 visualising lnRR results using emmeans

# Get estimated marginal means for both factors
emm_interaction <- emmeans(Seedllog, ~ Treatment | Fencing)

# Convert to dataframe
plot_data <- as.data.frame(emm_interaction)

# Ensuring factors are properly labeled
plot_data$Treatment <- factor(plot_data$Treatment, 
                              levels = c("F", "TF", "TFB", "THF"))


# reodering fencing level 
plot_data$Fencing <- factor(plot_data$Fencing, 
                            levels = c("Unfenced", "Fenced"),
                            labels = c("Unfenced", "Fenced"))


####### CONVERT lnRR to PERCENTAGE CHANGE #####
#  estimated marginal means for Treatment × Fencing interaction
emm_interaction <- emmeans(Seedllog, ~ Treatment | Fencing)
plot_data_raw <- as.data.frame(emm_interaction)

# Convert to percentage change
plot_data <- plot_data_raw
plot_data$pct_change <- (exp(plot_data_raw$emmean) - 1) * 100
plot_data$CI_lower_pct <- (exp(plot_data_raw$lower.CL) - 1) * 100
plot_data$CI_upper_pct <- (exp(plot_data_raw$upper.CL) - 1) * 100

# rounding off to 2 decimaL places
plot_data <- plot_data %>%
  mutate(across(where(is.numeric), round, 2))

# Clean up factors
plot_data$Treatment <- factor(plot_data$Treatment, 
                              levels = c("F", "TF", "TFB", "THF"))
plot_data$Fencing <- factor(plot_data$Fencing, 
                             levels = c("Unfenced", "Fenced"),
                             labels = c("Unfenced", "Fenced"))


### inverted axis. y = treatment
SeedVd <- ggplot(plot_data, aes(x = pct_change, y = Treatment, color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.5) +
  geom_point(size = 2.0, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
                 height = 0.5, size = 0.9, position = position_dodge(0.5)) +
  scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
  scale_x_continuous(breaks = seq(-100, 250, 50)) +
  labs(x = "Change in Seedling density relative to Control (%)",
       y = "Treatment") +
  # theme(legend.position = "top") +
  theme_classic()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))


# multipanel plot

####  Combine the plots in a single layout
multi_panelSe <- (SeedVa/SeedVb/SeedVd) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1, 1, 1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 8, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 10),        # Increase axis label font size
    axis.title = element_text(size = 12),       # Increase axis title font size
    plot.tag = element_text(size = 10, hjust = 0)  # Ensure left alignment
  )


##ggsave multipanel grass richness
ggsave(multi_panelSe,filename ="Plots/Seedlings ViolinLog1A.png",
           width = 16, height = 14, units = "cm")

#################################### HYPOTHESIS TESTING SEEDLINGS

## Is TFB in Unfenced subplots the most suppressive treatment?
#   Planned contrasts within Unfenced subplots

emm_unfenced <- emmeans(Seedllog, ~ Treatment, at = list(Fencing = "Unfenced"))


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


###Fencing effect within each treatment ---------------------------------
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


################################################################################################
###############################################################################################

#### SAPLINGS  SAPLINGS  SAPLINGS SAPLINGS 

# calculating sapling density
Saplings <- SapF %>%
  filter(
    woody_cat == "Saplings",
    Year %in% c(2024, 2026)
  ) %>%
  count(
    Site, Plot, Subplot, Treatment, Fencing, Year, Area,
    name = "Saplings"
  ) %>%
  mutate(
    density_ha = Saplings * 10000 / Area
  )


#comparing at treatment level

Sap_treat <- Saplings %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>% 
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop")  


# Summary stats for saplings 
Sapsummary_stats <- Saplings %>%
  group_by(Treatment,Fencing,Year) %>%
  summarise(
    N = n(),                                   # number of observations per treatment
    mean_density = mean(density_ha, na.rm = TRUE), 
    sd_density = sd(density_ha, na.rm = TRUE)
  ) %>%
  ungroup()

#sorting pre and post treatment
sptrt_comparison2 <- Saplings %>%
  filter(Year %in% c(2024, 2026)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))  


#reordering so that pre-treatment appears first then post treatment second on the plots
sptrt_comparison2 <- sptrt_comparison2 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))


#################DELTA SAPLING DENSITY

#Pivoting the two years side‑by‑side and compute Δ sapling density ─────────────
Saplings_Delta1 <- Sap_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Sapdens = dens_2026 - dens_2024) 



##### to test effect of treatment * fencing on seedling density###################

# Make "Unfenced" the reference level 

class(Saplings_Delta1$Fencing)  # Likely "character" or "ordered factor"

# Converting to unordered factor explicitly
Saplings_Delta1$Fencing <- factor(Saplings_Delta1$Fencing, ordered = FALSE)

# Verify
levels(Saplings_Delta1$Fencing)  


### Converting character variables to factors
Saplings_Delta1$Treatment <- as.factor(Saplings_Delta1$Treatment)

Saplings_Delta1$Fencing <- factor(Saplings_Delta1$Fencing, 
                                   levels = c("Unfenced", "Fenced"),
                                   labels = c("Unfenced", "Fenced"))

# Setting "Fenced" as the reference level 
Saplings_Delta1$Fencing <- relevel(Saplings_Delta1$Fencing, ref = "Unfenced")


# using the LMM for analysis
Sapl5 <- lmer(delta_Sapdens ~ Treatment * Fencing + (1|Site),  
              data = Saplings_Delta1)

summary(Sapl5)

# 
# # Model diagnostics
# check_model(Sapl5, check = "qq")
# check_model(Sapl5, check = "normality")
# check_model(Sapl5, check = "homogeneity")
# plot(Sapl5)


#### PLOTS
# raw results using means
sptrt_comparison2$Fencing <- factor( sptrt_comparison2$Fencing,
 levels = c("Unfenced", "Fenced")) #ordering Fencing level to start with Unfenced

SapVa<- ggplot(sptrt_comparison2, 
                   aes(x = Treatment, y = density_ha, fill = Fencing)) + facet_wrap(~Period)+ 
  geom_violin(trim = TRUE)+
  #geom_hline(yintercept = 0, linetype = "dashed") +    
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1.4, color = "black") +
  labs(x = "Treatment", 
       y = expression("Sapling density "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 9),      # Axis titles
    axis.text = element_text(size = 9)        # Axis tick labels
  )+
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))


## violin plot seedling delta
SapVb <- ggplot(Saplings_Delta1,
                     aes(x = Treatment, y = delta_Sapdens, fill = Fencing))+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1.4, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       #y = "Change in Saplings density per ha",
       y = expression("Δ Sapling density "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 9),  # Axis titles reduced from 12 to 8
    axis.text = element_text(size = 9)) +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))



# preparing saplings data for LOG RESPONSE RATIO
SapLOG <- SapF %>%
  filter(
    woody_cat == "Saplings",
    Year %in% c(2024, 2026)
  ) %>%
  count(
    Site,
    Plot,
    Subplot,
    Treatment, 
    Fencing,
    Year,
    Area,
    name = "Saplings"
  ) %>%
  mutate(
    density_ha = Saplings * 10000 / Area
  )

#---------------------------------------------------
# 2. Convert to wide format
#---------------------------------------------------

Sapl_wide <- SapLOG %>%
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

# Result:
# Y2024 = pretreatment density
# Y2026 = posttreatment density

#---------------------------------------------------
# 3. Extract control values
#---------------------------------------------------

control_data <- Sapl_wide %>%
  filter(Treatment == "C") %>%
  dplyr::select(
    Site,
    Plot,
    Subplot, Fencing,
    C_pre = Y2024,
    C_post = Y2026
  )

##Create control means
control_data <- Sapl_wide %>%
  filter(Treatment == "C") %>%
  group_by(Site, Fencing) %>%
  summarise(
    C_pre = mean(Y2024, na.rm = TRUE),
    C_post = mean(Y2026, na.rm = TRUE),
    .groups = "drop"
  )

#---------------------------------------------------
# 4. Join control data to treatments
#---------------------------------------------------
SSapl_lnRR <- Sapl_wide %>%
  filter(Treatment != "C") %>%
  left_join(
    control_data,
    by = c("Site", "Fencing")
  )

#---------------------------------------------------
# 5. Handle zeros
#---------------------------------------------------

# seed_lnRR <- seed_lnRR %>%
#   mutate(
#     Y2014 = ifelse(Y2024 == 0, 0.001, 2024),
#     Y2016 = ifelse(Y2026 == 0, 0.001, Y2026),
#     C_pre = ifelse(C_pre == 0, 0.001, C_pre),
#     C_post = ifelse(C_post == 0, 0.001, C_post)
#   )

#---------------------------------------------------
# 6. Calculate Log Response Ratio
#---------------------------------------------------

SSapl_lnRR <- SSapl_lnRR %>%
  mutate(
    lnRR = log(
      (Y2026 / Y2024) /
        (C_post / C_pre)))

#---------------------------------------------------
# 7. Treatment summaries
#---------------------------------------------------

saplingD_summary <- SSapl_lnRR %>%
  group_by(Treatment, Fencing) %>%
  summarise(
    mean_lnRR = mean(lnRR, na.rm = TRUE),
    sd_lnRR = sd(lnRR, na.rm = TRUE),
    n = n(),
    se_lnRR = sd_lnRR / sqrt(n)
  )



### Calculating n for each Treatment × Fencing combination
sample_sizes2 <- SSapl_lnRR %>%
  group_by(Treatment, Fencing) %>%
  summarise(
    n = n(),
    .groups = 'drop'
  )

#---------------------------------------------------
# 8. Mixed-effects model
#---------------------------------------------------
Sapllog <- lmer(lnRR  ~ Treatment * Fencing + (1 | Site),
                 data = SSapl_lnRR)

summary(Sapllog)


# # Model diagnostics
check_model(Sapllog, check = "qq")
check_model(Sapllog, check = "normality")
check_model(Sapllog, check = "homogeneity")
plot(Sapllog)


#---------------------------------------------------
#  Plot lnRR
#---------------------------------------------------

##### Visualising lnRR results using emmeans

# Get estimated marginal means for both factors
Spemm_interaction <- emmeans(Sapllog, ~ Treatment | Fencing)

# Convert to dataframe
Splot_data <- as.data.frame(Spemm_interaction)

# Ensure factors are properly labeled
Splot_data$Treatment <- factor(Splot_data$Treatment, 
                              levels = c("F", "TF", "TFB", "THF"))
Splot_data$Fencing <- factor(Splot_data$Fencing, 
                            levels = c("Unfenced", "Fenced"),
                            labels = c("Unfenced", "Fenced"))

####### CONVERTING lnRR to PERCENTAGE CHANGE #####
# Get estimated marginal means for Treatment × Fencing interaction
Spemm_interaction <- emmeans(Sapllog, ~ Treatment | Fencing)
splot_data_raw <- as.data.frame(Spemm_interaction)

# Converting to percentage change
Splot_data <- splot_data_raw
Splot_data$pct_change <- (exp(splot_data_raw$emmean) - 1) * 100
Splot_data$CI_lower_pct <- (exp(splot_data_raw$lower.CL) - 1) * 100
Splot_data$CI_upper_pct <- (exp(splot_data_raw$upper.CL) - 1) * 100

# rounding off to 2 decimaL places
Splot_data <- Splot_data %>%
  mutate(across(where(is.numeric), round, 2))


# Clean up factors
Splot_data$Treatment <- factor(Splot_data$Treatment, 
                              levels = c("F", "TF", "TFB", "THF"))
Splot_data$Fencing <- factor(Splot_data$Fencing, 
                             levels = c("Unfenced", "Fenced"),
                             labels = c("Unfenced", "Fenced"))

######## inverted x y axis - treatment on x-axis
SapVd <- ggplot(Splot_data, aes(x = pct_change, y = Treatment , color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.5) +
  geom_point(size = 2.0, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
                 height = 0.5, size = 0.9, position = position_dodge(0.5)) +
  scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
  scale_x_continuous(breaks = seq(-80, 50, 20)) +
  labs(x = "Change in sapling density relative to Control (%)",
       y = "Treatment") +
 # theme(legend.position = "top") +
  theme_classic()+ 
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 14))


####  Combining the plots in a single layout
multi_panelSap <- (SapVa/SapVb/SapVd) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1, 1, 1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(', 
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 8, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 10),        # Increase axis label font size
    axis.title = element_text(size = 12),       # Increase axis title font size
    plot.tag = element_text(size = 10, hjust = 0)  # Ensure left alignment
  )


##ggsave multipanel grass richness
ggsave(multi_panelSap,filename ="Plots/Saplings ViolinLog1A.png",
       width = 16, height = 14, units = "cm")



######## Sapling planned contrasts and fencing effect ########################

# Is TFB in Unfenced subplots the most suppressive treatment?
# Direct planned contrasts: TFB vs each other treatment within Unfenced.SAPLINGS
Spemm_interaction <- emmeans(Sapllog, ~ Treatment | Fencing)

emm_sap_unfenced <- emmeans(Sapllog, ~ Treatment, at = list(Fencing = "Unfenced"))
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


## Effect of fencing within each treatment 
# Test: Is there a statistical difference between Fenced and Unfenced subplots
# within each treatment Contrasts Fenced vs Unfenced for F, TF, TFB, THF.

sapemm_fence_by_trt <- emmeans(Sapllog, ~ Fencing | Treatment)

fencing_within_trt <- pairs(
  sapemm_fence_by_trt,
  reverse = TRUE,    # Fenced - Unfenced (positive = Fenced has higher lnRR)
  adjust  = "holm"   # Holm correction across the four treatment-level tests
)

print(fencing_within_trt)

# Confidence intervals on the Fenced - Unfenced contrasts
confint(fencing_within_trt)


############################################################################################
############################################################################

##########RESPROUTS RESPROUTS RESPROUTS  RESPROUTS RESPROUTS ####################

# Step 1: Filtering Cut stumps only and years 
resprouts_df <- SapF %>%
  filter(
    woody_cat == "Cut stump",
    Year %in% c(2026),
    !Treatment %in% c("C", "F")  # exclude the two treatments
  )



#Summary stats for resprouts 
Respsummary_stats <- resprouts_df %>%
  group_by(Site, Treatment) %>%
  summarise(
    Total_resprouts = sum(No_of_resprouts, na.rm = TRUE),
    Mean_resprouts = mean(No_of_resprouts, na.rm = TRUE),
    Max_resprouts = max(No_of_resprouts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_resprouts))


## checking which species had more resprouts
Species_resprouts <- resprouts_df %>%
  group_by(Species_name, Treatment) %>%
  summarise(
    Total_resprouts = sum(No_of_resprouts, na.rm = TRUE),
    N = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_resprouts))



###
# Check current class of FieldType

class(resprouts_df$Fencing)  # Likely "character" or "ordered factor"

# Converting to unordered factor explicitly
resprouts_df$Fencing <- factor(resprouts_df$Fencing, ordered = FALSE)

# Verifying
levels(resprouts_df$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Setting "Unfenced" as the reference level (to see "Unfenced" coefficients)
resprouts_df$Fencing <- relevel(resprouts_df$Fencing, ref = "Unfenced")

### Converting character variables to factors
resprouts_df$Treatment <- as.factor(resprouts_df$Treatment)
resprouts_df$Fencing <- as.factor(resprouts_df$Fencing)


#violin plot
RespVio <- ggplot(resprouts_df, aes(x = Treatment, y = No_of_resprouts,
                                    fill = Fencing)) + 
  geom_violin(trim = TRUE)+
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1.5, color = "black") +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  #scale_x_continuous(breaks = seq(1, 45, 5)) +
  labs(x = "Treatment", y = "No.of resprouts per cut stump") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12)) +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))


##saving Violin PLOT - resprouts
#ggsave(RespVio,filename ="Plots/RESPROUTS VIOLIN26 plot.png",
#width = 16, height = 14, units = "cm")  



##GLMM for resprouts on cut stumps  

#  using tweedie distribution - to handle zeros

R5b_tweedie <- glmmTMB(No_of_resprouts ~ Treatment * Fencing + (1|Site),  
                       data = resprouts_df,
                       family = tweedie(link = "log"))


# diagnostics using DHARMA
simTw <- simulateResiduals(R5b_tweedie)
plot(simTw)

# Explicit tests (should be non-significant if model meets DHARMA assumptions)
testDispersion(simTw)
testZeroInflation(simTw)
testOutliers(simTw)

# model summary
summary(R5b_tweedie)


# # # Model diagnostics
#  qqnorm(residuals(R5b_tweedie)) #whether residuals are approximately normal.
# qqline(residuals(R5b_tweedie))


### POST HOC ANALYSIS FOR RESPROUTS 
# pairwise comparisons
Rstreat_comparisons3 <- emmeans(R5b_tweedie, specs = pairwise ~ Treatment | Fencing, adjust = "Dunnet")
summary(Rstreat_comparisons3$contrasts)

# Estimated marginal means for Treatment within Fencing (if needed)
Rstreat_comparisons3 <- emmeans(R5b_tweedie, ~ Treatment | Fencing, type = "response")

# Comparing each treatment to Control with Dunnett adjustment (or "none" if you only want vs control)
contrast_vs_control <- contrast(Rstreat_comparisons3, method = "trt.vs.ctrl", ref = "TF")
summary(contrast_vs_control, infer = TRUE)

# generating letters using cld in multicomp package
Respcld_emm <- cld(Rstreat_comparisons3, adjust = "Dunnett", Letters = letters, type = "response")
cld_tbl <- as.data.frame(Respcld_emm)


# preparing clean database for plotting
Rspplot_df <- cld_tbl %>%
  rename(
    EMM = response,
    CI_lower = asymp.LCL, # tweedie used different typology for EMM and CIs
    CI_upper = asymp.UCL, 
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace


# checking if log scale has been back transformed
summary(emmeans(R5b_tweedie, ~ Treatment, type = "response"))

### Visualisation using ggplot

Rspplot_df$Fencing <- factor( Rspplot_df$Fencing,
                              levels = c("Unfenced", "Fenced")) #ordering Fencing level to start with Unfenced


Resp <- ggplot(Rspplot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 1.5) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.35), width = 0.12) +
  geom_text(aes(label = Group,
                y = CI_upper + 0.1 * max(EMM)),
            position = position_dodge(width = 0.35), size = 4, color = "black") +
  scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))  +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    #y = expression("Average number of resprouts per cut stump")
    y = expression("Mean resprouts per cut stump")
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  #geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))



## creating a multipanel for panels with 2 different y-axis

# # Combining the plots in a single layout
Resp2 <- (RespVio/Resp) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1, 1,1)) +    # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 8, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 11),        # Increase axis label font size
    axis.title = element_text(size = 10),       # Increase axis title font size
    plot.tag = element_text(size = 12, hjust = 0))  # Ensure left alignment


### saving plot
ggsave(Resp2,filename ="Plots/ 3Violin Resprouts.png",
       width = 16, height = 14, units = "cm") 


######################### EFFECT of fencing ON RESPROUTS --------------------------
# Test: Is there a statistical difference between Fenced and Unfenced subplots
# within each treatment? Contrasts Fenced vs Unfenced for TF, TFB, THF.

respemm_fence_by_trt <- emmeans(R5b_tweedie, ~ Fencing | Treatment)

resfencing_within_trt <- pairs(
  respemm_fence_by_trt,
  reverse = TRUE,    # Fenced - Unfenced 
  adjust  = "holm"   # Holm correction across the three treatment-level tests
)

print(resfencing_within_trt)

##########################################################################################

######
# 1. CLASSIFYNG RESPROUTS SPECIES response────────────────────────────────────────────────

SapF <- B_merged %>%
  mutate(
    woody_cat = case_when(
      Woody_class == "Cut stump"             ~ "Cut stump",
      between(Max_height.m., 0.05, 0.50)     ~ "Seedlings",
      between(Max_height.m., 0.51, 1.49)     ~ "Saplings",
      between(Max_height.m., 1.50, 21.0)     ~ "Trees",
      TRUE                                   ~ NA_character_
    )
  )
# ─── 2. FILTERING TO RESPROUTS ────────────────────────────────────
# All cut stumps in 2026; No_of_resprouts = 0 are valid observations

resprouts2 <- SapF %>%
  filter(
    woody_cat == "Cut stump",
    Year == 2026,
    !Treatment %in% c("C", "F")
  ) %>%
  mutate(
    Treatment    = factor(Treatment, levels = c("TF", "TFB", "THF")),
    Fencing      = factor(Fencing, levels = c("Unfenced", "Fenced")),
    Species_name = factor(Species_name),
    Site         = factor(Site)
  )

# ─── 3. FILTERING: SPECIES WITH N >= 3 IN EACH TREATMENT (ACROSS FENCING) ───────

sp_counts <- resprouts2 %>%
  count(Species_name, Treatment) %>%
  pivot_wider(names_from = Treatment,
              values_from = n,
              values_fill = 0)

sp_eligible2 <- sp_counts %>%
  filter(TF >= 3, TFB >= 3, THF >= 3) %>%
  pull(Species_name)

cat("Species meeting N >= 3 per treatment:", length(sp_eligible2), "\n")
print(as.character(sp_eligible2))

resprouts2_sp <- resprouts2 %>%
  filter(Species_name %in% sp_eligible2) %>%
  mutate(Species_name = droplevels(Species_name))

# Checking data structure
str(resprouts2_sp)
summary(resprouts2_sp)

# Checking the response variable
class(resprouts2_sp$No_of_resprouts)
summary(resprouts2_sp$No_of_resprouts)
hist(resprouts2_sp$No_of_resprouts, breaks = 20, main = "Distribution of Resprout Counts")

# Checking for NAs in the response variable
sum(is.na(resprouts2_sp$No_of_resprouts))
summary(resprouts2_sp$No_of_resprouts)

# Checking for NAs in other important columns
sum(is.na(resprouts2_sp$Treatment))
sum(is.na(resprouts2_sp$Fencing))
sum(is.na(resprouts2_sp$Species_name))
sum(is.na(resprouts2_sp$Site))
sum(is.na(resprouts2_sp$Plot))


# ─── MODEL SELECTION FOR COUNT DATA ────────────────────────────────────────

# Making sure factors are properly coded
resprouts2_sp$Treatment <- factor(resprouts2_sp$Treatment)
resprouts2_sp$Fencing <- factor(resprouts2_sp$Fencing)
resprouts2_sp$Species_name <- factor(resprouts2_sp$Species_name)


#  MODEL WITH POST-HOC TESTS 
# Using the reduced model approach 
best_model <- glmmTMB(No_of_resprouts ~ Treatment * Fencing + 
                        Treatment * Species_name + 
                        Fencing * Species_name + 
                        (1|Site),
                      data = resprouts2_sp,
                      family = nbinom2)



# ─── MODEL DIAGNOSTICS ──────────────────────────────────────────────────────

simulationOutput <- simulateResiduals(fittedModel = best_model, n = 250)
plot(simulationOutput)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)



# ─── POST-HOC ANALYSIS ────────────────────────────────────────────────────

# Specific comparisons - Treatment × Fencing interaction for each species
# Note: Since 3-way interaction was removed, we look at 2-way interactions
emm_interaction <- emmeans(best_model, ~ Treatment * Fencing | Species_name)
interaction_pairs <- pairs(emm_interaction, adjust = "tukey")
print(interaction_pairs)

# VISUALIZATION WITH BACK-TRANSFORMED VALUES (ACTUAL COUNTS) ────────────
# using predicted means 
emm_results <- as.data.frame(emmeans(best_model, 
                                     ~ Treatment * Fencing * Species_name,
                                     type = "response"))

#  SPECIES ORDER: ranking by total cut stumps (most dominant at top) ──────
sp_stump_order <- resprouts2_sp |>
  count(Species_name, name = "N_stumps") |>
  arrange(N_stumps) |>        # ascending so most dominant lands at top of y-axis
  pull(Species_name) |>
  as.character()

# Applying the species order to the emm_results
emm_results$Species_name <- factor(emm_results$Species_name, 
                                   levels = sp_stump_order)

##### plotting  

multi_sp2 <- ggplot(emm_results, aes(x = response, y = Species_name, 
                                     color = Treatment)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL),
                 height = 0.35, linewidth = 0.55,
                 position = position_dodge(0.55)) +
  facet_wrap(~Fencing, ncol = 2, scales = "free_x") +
  scale_color_manual(values = c("TF" = "red", "TFB" = "black", "THF" = "blue")) +
  labs(x = "Mean resprouts per cut stump",
       y = NULL, color = "Treatment") +
  theme_classic() +
  theme(
    axis.text.y  = element_text(size = 9, face = "italic"),
    axis.text.x  = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    strip.text   = element_text(size = 9, face = "bold"),
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.title   = element_text(size = 9, hjust = 0.5)
  )

# saving
ggsave(multi_sp2,
       filename = "Plots/Resprouting_SpeciesN3.png",
       width = 16, height = 14, units = "cm")


#  COMPLETE CONTRASTS WITH TF AS REFERENCE  ──────────

# Creating custom contrasts with TF as reference
# First, getting the emmeans
emm_all <- emmeans(best_model, ~ Treatment * Fencing * Species_name, 
                   type = "response")

# Create contrast matrix for TFB vs TF and THF vs TF
contrast_list <- list(
  "TFB_vs_TF" = c(1, -1, 0),  # Assuming order: TF, TFB, THF
  "THF_vs_TF" = c(1, 0, -1)
)

# Apply contrasts
custom_contrasts <- contrast(emm_all, 
                             method = list(
                               "TFB - TF" = c(-1, 1, 0),
                               "THF - TF" = c(-1, 0, 1)
                             ),
                             adjust = "BH")

# View results
print(custom_contrasts)


######################################################################################################
##################################################################################################

################################# GRASSES     BIOMASS CALIBRATION


# load data

RSQTGdata <- read_csv("DATA/DPM.csv")

################## DPM HEIGHT ~ OVEN DRIED WEIGHT. NO TRANSFORMATION
# 2. Convert weight from grams to kg/ha
#    Area of 34cm diameter disc = π * (0.17 m)^2 = 0.0908 m²
frame_area <- pi * (0.17^2)  # = 0.0908 m²
RSQTGdata$Biomass_kg_ha <- RSQTGdata$Weight * 10 / frame_area


# 3. Linear regression: Biomass ~ DPH
modelWG <- lm(Biomass_kg_ha ~ DPH_Height, data = RSQTGdata)
#summary(modelWG)
# tab_model(modelWG)

# model diagnostics
 #qqnorm(residuals(modelWG))
 #qqline(residuals(modelWG))

# checking for equal variance
plot(fitted(modelWG), residuals(modelWG),
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, lty = 2)



###adding annotation to the plot

# Extracting coefficients
coefs <- coef(modelWG)
intercept <- round(coefs[1], 2)
slope <- round(coefs[2], 2)

# Create equation string for annotation 
# Building the equation string (biomass = intercept + slope * x)
eq <- paste0("Biomass== ", intercept, " + ", slope, " %*% Dpm")

coef <- coefficients(modelWG)
r2 <- summary(modelWG)$r.squared
#n <- nobs(modelWG)
eq <- paste0(
  "y = ", round(coef[1], 2), " + ", round(coef[2], 2), "x\n",
  "R² = ", round(r2, 3))#, ", n = ", n)


# Plotting with regression and equation

DPM <- ggplot(RSQTGdata, aes(x = DPH_Height, y = Biomass_kg_ha)) + 
  geom_point(size = 0.7, color = "black") + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  annotate("text", x = Inf, y = -Inf, label = eq, hjust = 1.0, vjust = -0.1, size = 2.0, color = "black") +
  labs( x = "DPM height (cm)",
        y = "Standing grass biomass ("*kg~ha^{-1}*")") +
  theme_beautiful()+
  theme(
    axis.title = element_text(size = 11),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

## saving plot
#ggsave(DPM,filename ="Plots/DPM&Biomass.png",width = 16, height = 14, units = "cm")  


# MODEL performance
#performance::check_model(modelWG)

#############################################
############# LOG TRANSFORMED RLM            LOG TRANSFORMED RLM     


## 1. Converting weight from grams to kg/ha
#    Area of 34cm diameter disc = π * (0.17 m)^2 = 0.0908 m²
frame_area <- pi * (0.17^2)  # = 0.0908 m²
RSQTGdata$Biomass_kg_ha <- RSQTGdata$Weight * 10 / frame_area

# --- Log-transforming the variables (natural log) ---

RSQTGdata$log_Biomass_kg_ha <- log(RSQTGdata$Biomass_kg_ha)
RSQTGdata$log_DPH_Height  <- log(RSQTGdata$DPH_Height)

# 1. Free-intercept robust model
Rmodel_free <- RobustLinearReg::theil_sen_regression(log_Biomass_kg_ha ~ log_DPH_Height, data = RSQTGdata)

int_free   <- round(coef(Rmodel_free)[["(Intercept)"]], 2)
slope_free <- round(coef(Rmodel_free)[["log_DPH_Height"]], 2)

# Clean equation: log(y) = intercept + slope * log(x)
Req_free <- sprintf("log(y) = %.2f %+.2f log(x)", int_free, slope_free)

# 2. Zero-intercept robust model (forced through origin)
Rmodel_zero <- RobustLinearReg::theil_sen_regression(log_Biomass_kg_ha ~ 0 + log_DPH_Height, data = RSQTGdata)

slope_zero <- round(coef(Rmodel_zero)[["log_DPH_Height"]], 2)

# Clean equation: no intercept term
Req_zero <- sprintf("log(y) = %.2f log(x)", slope_zero)


#### --- ggplot with both models 
RLMB <- ggplot(RSQTGdata, aes(x = log_DPH_Height, y = log_Biomass_kg_ha)) +
  geom_point(color = "black", alpha = 0.54) +
  
  # Free-intercept line (solid blue)
  geom_smooth(method = "lm", formula = y ~ x,
              color = "blue", linewidth = 0.7, se = FALSE, fullrange = FALSE) +
  
  # Zero-intercept line (dashed red)
  geom_smooth(method = "lm", formula = y ~ x - 1,
              color = "red", linewidth = 0.7, linetype = "solid", se = FALSE, fullrange = FALSE) +
  
  
  ## adjusting font size for equations
  annotate("text", x = -Inf, y = Inf, label = Req_free,
           hjust = -0.1, vjust = 1.5, color = "blue", size = 5.0) +
  
  annotate("text", x = -Inf, y = Inf, label = Req_zero,
           hjust = -0.1, vjust = 3.5, color = "red", size = 5.0) +
  labs(x = "Log DPM Height (cm)",
       y = "Log Standing grass biomass ("*kg~ha^{-1}*")") +
  theme_classic()+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

## ggsave plot

#ggsave(RLMB, filename = "Plots/ LogBiomass - RLM.png",width = 16, height = 14, units = "cm")   


####
# Free-intercept model
model_log_free <- RobustLinearReg::theil_sen_regression(log_Biomass_kg_ha ~ log_DPH_Height, data = RSQTGdata)
int_free   <- round(coef(model_log_free)[["(Intercept)"]], 3)
slope_free <- round(coef(model_log_free)[["log_DPH_Height"]], 3)

# Zero-intercept model
model_log_zero <- RobustLinearReg::theil_sen_regression(log_Biomass_kg_ha ~ 0 + log_DPH_Height, data = RSQTGdata)
slope_zero <- round(coef(model_log_zero)[["log_DPH_Height"]], 3)


# ============================================
# 1. BIAS CORRECTION 
# ============================================

intercept <- 4.67
slope <- 1.141

# Step 1: Calculating the median predictions from the Theil-Sen model
# (Using  intercept of 4.67 and slope of 1.14)
pred_median <- exp(4.67 + 1.14 * RSQTGdata$log_DPH_Height)

# Step 2: Calculating the ratio of observed biomass to predicted median biomass
ratios <- RSQTGdata$log_Biomass_kg_ha / pred_median

# Step 3: Calculating the bias correction factor (CF) as the MEAN of the ratios
# This is a non-parametric smearing factor to go from Median to Mean
CF <- mean(ratios)

# Step 4: Print the correction factor
print(CF)

# Step 5: Applying the correction to get MEAN biomass predictions
RSQTGdata$Biomass_Mean_Corrected <- pred_median * CF

# Checking if you have original-scale biomass
if (!"Biomass_kg_ha" %in% names(RSQTGdata)) {
  # If you only have log_Biomass_kg_ha, back-transform it
  RSQTGdata$Biomass_kg_ha <- exp(RSQTGdata$log_Biomass_kg_ha)
  cat("\nCreated Biomass_kg_ha from log_Biomass_kg_ha")
}

# Calculating median predictions (original scale)
pred_median <- exp(intercept + slope * RSQTGdata$log_DPH_Height)

# Calculating ratios (BOTH on original scale!) 
ratios <- RSQTGdata$Biomass_kg_ha / pred_median

# Check ratios
cat("\n===== RATIO STATISTICS =====")
cat("\nMin:", min(ratios, na.rm = TRUE))
cat("\nMax:", max(ratios, na.rm = TRUE))
cat("\nMean:", mean(ratios, na.rm = TRUE))
cat("\nMedian:", median(ratios, na.rm = TRUE))

# Fitting LOESS to ratios
ratio_model <- loess(ratios ~ RSQTGdata$log_DPH_Height, span = 0.75)

# Calculating CF for the data
RSQTGdata$CF <- predict(ratio_model, RSQTGdata$log_DPH_Height)

# Bias-corrected predictions
RSQTGdata$Biomass_Mean_Corrected <- pred_median * RSQTGdata$CF

# ============================================
# 2. CREATE PREDICTION GRID
# ============================================

x_new <- seq(min(RSQTGdata$DPH_Height), 
             max(RSQTGdata$DPH_Height), 
             length.out = 120)

# Predicting CF for grid
CF_grid <- predict(ratio_model, log(x_new))

# Checking if CF is reasonable
cat("\n===== CF GRID STATISTICS =====")
cat("\nCF min:", min(CF_grid, na.rm = TRUE)) 
cat("\nCF max:", max(CF_grid, na.rm = TRUE))
cat("\nCF mean:", mean(CF_grid, na.rm = TRUE))

# If CF is 0, there's a problem with ratios
if (all(CF_grid == 0, na.rm = TRUE)) {
  stop("ERROR: All CF values are 0. Check your ratios calculation.")
}

# ============================================
# 3. GENERATE PREDICTIONS
# ============================================

pred_df <- data.frame(
  x = x_new,
  SHR = exp(intercept + slope * log(x_new)) * CF_grid,
  Trollope = -3019 + 2260 * sqrt(x_new),
  Zambatis = (31.7176 * 0.3218^(1 / x_new) * x_new^0.2834)^2
)

# ============================================
#  CHECK PREDICTIONS
# ============================================

cat("\n===== PREDICTION STATISTICS =====")
cat("\nSaf range:", range(pred_df$Saf, na.rm = TRUE))
cat("\nTrollope range:", range(pred_df$Trollope, na.rm = TRUE))
cat("\nZambatis range:", range(pred_df$Zambatis, na.rm = TRUE))


# ============================================
# 4. PREPARE FOR PLOTTING - plot with EQUATION
# ============================================


plot_df <- pred_df %>%
  pivot_longer(cols = -x, names_to = "Model", values_to = "Biomass")

# ============================================
# 5. CREATE EQUATIONS WITH N ABOVE
# ============================================

n_obs <- nrow(RSQTGdata)

# Get y-axis limits
y_max <- max(RSQTGdata$Biomass_kg_ha, na.rm = TRUE)
y_min <- min(RSQTGdata$Biomass_kg_ha, na.rm = TRUE)
y_range <- y_max - y_min

# Small gap from the y-axis (3% of x range keeps text clear of the axis)
x_gap <- min(RSQTGdata$DPH_Height) + 0.01 * diff(range(RSQTGdata$DPH_Height))

# Create equation labels - NOW AT THE TOP
equations <- data.frame(
  Model = c("SHR", "Trollope", "Zambatis"),
  label = c(
    "y = exp(4.67 + 1.14·log(x)) × CF(x)",
    "y = -3019 + 2260·√x",
    "y = (31.7176·0.3218^(1/x)·x^0.2834)²"
  ),
  x = x_gap,
  y = c(
    y_max - 0.10 * y_range,  # SHR (very top)
    y_max - 0.20 * y_range,  # Trollope (slightly below)
    y_max - 0.30 * y_range   # Zambatis (still near top)
  )
)

# Creating n label (positioned ABOVE or AT THE VERY TOP of equations)
n_label <- data.frame(
  x     = x_gap,
  y     = y_max - 0.00 * y_range,  # Just below top of plot
  label = paste0("n = ", n_obs)
)

# For even tighter spacing at the top:
# n_label at very top, equations immediately below

model_colors <- c(
  "SHR" = "blue",      # Blue
  "Trollope" = "brown", # Purple
  "Zambatis" = "black"  # Orange
)

# ============================================================
# MODEL COMPARISON MULTIPANEL  (RLMB / BiasC2)
# ============================================================

# Assign the model comparison plot to a named object
BiasC2_bc <- ggplot() +
  geom_point(data = RSQTGdata,
             aes(x = DPH_Height, y = Biomass_kg_ha),
             alpha = 0.3, size = 1.5, color = "gray50") +
  geom_line(data = plot_df,
            aes(x = x, y = Biomass, color = Model, linetype = Model),
            linewidth = 0.7) +
  geom_text(data = n_label,
            aes(x = x, y = y, label = label),
            hjust = 0, vjust = 1, size = 3.8,
            fontface = "plain", color = "black") +
  geom_text(data = equations,
            aes(x = x, y = y, label = label, color = Model),
            hjust = 0, vjust = 1,
            size = 3.0, lineheight = 1.0,
            show.legend = FALSE) +
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = c("solid", "solid", "solid")) +
  labs(x = "DPM height (cm)",
       y = expression("Standing grass biomass ("*kg~ha^{-1}*")"),
       color    = "Model",
       linetype = "Model") +
  theme_classic() +
  theme(
    legend.position      = c(0, 1),
    legend.justification = c(0, 0.8),
    legend.box.just      = "left",
    axis.title           = element_text(size = 12),
    axis.text            = element_text(size = 12)
  )


# Multipanel: RLMB (log-log) on top, BiasC2 (original scale) below
multi_pBiomass2_bc <- (RLMB / BiasC2_bc) +
  plot_layout(
    nrow   = 2,
    guides = "collect"
  ) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = theme(
      plot.tag = element_text(size = 6, face = "plain", hjust = 0)
    )
  ) &
  theme_classic() &
  theme(
    axis.text        = element_text(size = 10),
    axis.title       = element_text(size = 10),
    strip.text       = element_text(size = 10, face = "plain"),
    panel.grid.minor = element_blank(),
    plot.margin      = margin(3, 3)
  )


# saving plot
ggsave(multi_pBiomass2_bc,
       filename = "Plots/ModelComparison-BIOMASS-BiasC.png",
       width = 16, height = 14, units = "cm")




#################################################################################################
#############################

######   GRASS BIOMASS GRASS BIOMASS GRASS BIOMASS GRASS BIOMASS

# Loading data
heights_data <-  read_csv("DATA/March2026/2Grasses2426.csv")

Grasses <- read_csv("DATA/March2026/Grasses2426Updated1.csv")


# defining intercept and slope
INTERCEPT <- 4.67
SLOPE <- 1.14

# Ensure height column
if (!"DPM_Height" %in% names(heights_data)) {
  heights_data$DPM_Height <- heights_data[[names(heights_data)[1]]]
  cat("Using first column as Height_cm\n")
}

# Predicting biomass using log-transformed model
predict_biomass <- function(h) {
  
  # avoid log(0) or negative values
  h <- pmax(h, 1e-6)
  
  log_b <- INTERCEPT + SLOPE * log(h)
  
  # back-transform to biomass scale
  biomass <- exp(log_b)
  
  return(round(biomass, 2))
}
options(scipen = 999) # to remove scientific notation

# round off 
heights_data$Biomass_kg_ha <- round(predict_biomass(heights_data$DPM_Height),2)

# Save
#write.csv(heights_data, "biomass_predictions.csv", row.names = FALSE)


# ============================================
# CONVERT MEDIAN TO MEAN (WITH BIAS CORRECTION)
# ============================================

# converting median to mean 
heights_data <- heights_data %>%
  mutate(
    Biomass_mean_kg_ha = case_when(
      DPM_Height > 0 ~ Biomass_kg_ha * predict(ratio_model, log(DPM_Height)),
      DPM_Height == 0 ~ Biomass_kg_ha  # Keep as 0
    )
  )

# Summarizing using mean()
Biomasssummary2_mean <- heights_data %>%
  filter(Year %in% c(2024, 2026)) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(
    mean_Biomass = mean(Biomass_mean_kg_ha, na.rm = TRUE), 
    .groups = "drop"
  )

# ============================================
# GRASS BIOMASS ANALYSIS (BIAS-CORRECTED)
# ============================================

### Converting character variables to factors
heights_data$Treatment <- as.factor(heights_data$Treatment)
heights_data$Fencing   <- as.factor(heights_data$Fencing)

# ---------------------------------------------------
# 1. Pre vs Post violin plot
# ---------------------------------------------------

biomass2_bc <- heights_data %>%
  filter(Year %in% c(2024, 2026)) %>%
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(
    mean_Biomass = mean(Biomass_mean_kg_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment")) %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

# Reordering Fencing factor
biomass2_bc$Fencing <- factor(biomass2_bc$Fencing,
                              levels = c("Unfenced", "Fenced"))

#plot

GbA_bc <- ggplot(biomass2_bc,
                 aes(x = Treatment, y = mean_Biomass, fill = Fencing)) +
  facet_wrap(~Period) +
  geom_violin(trim = TRUE) +
  stat_summary(fun = mean, geom = "point",
               position = position_dodge(0.8),
               size = 1.5, color = "black") +
  labs(x = "Treatment",
       y = expression("Grass biomass ("*kg~ha^{-1}*")")) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text  = element_text(size = 8)
  ) +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))


# ---------------------------------------------------
# 2. Delta biomass (Δ = Post - Pre)
# ---------------------------------------------------

delta_GRBiomass_bc <- heights_data %>%
  filter(Year %in% c(2024, 2026)) %>%
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_Biomass = mean(Biomass_mean_kg_ha, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = Year,
    values_from = mean_Biomass,
    names_glue  = "Biomass_{Year}"
  ) %>%
  mutate(delta_GR = Biomass_2026 - Biomass_2024) %>%
  drop_na(delta_GR)

### Converting to factors and set reference level
delta_GRBiomass_bc$Treatment <- as.factor(delta_GRBiomass_bc$Treatment)
delta_GRBiomass_bc$Fencing   <- factor(delta_GRBiomass_bc$Fencing,
                                       levels = c("Unfenced", "Fenced"),
                                       labels = c("Unfenced", "Fenced"))
delta_GRBiomass_bc$Fencing   <- relevel(delta_GRBiomass_bc$Fencing, ref = "Unfenced")

## LMM for delta grass biomass
Grasbiom_bc <- lmer(delta_GR ~ Treatment * Fencing + (1 | Site),
                    data = delta_GRBiomass_bc)
summary(Grasbiom_bc)

# Model diagnostics
  # performance::check_singularity(Grasbiom_bc)
  # performance::check_convergence(Grasbiom_bc)
  # check_model(Grasbiom_bc, check = "qq")
  # check_model(Grasbiom_bc, check = "normality")
  # check_model(Grasbiom_bc, check = "homogeneity")

## Violin plot of Δ grass biomass
Gbb_bc <- ggplot(delta_GRBiomass_bc,
                 aes(x = Treatment, y = delta_GR, fill = Fencing)) +
  geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point",
               position = position_dodge(0.8),
               size = 1.5, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment",
       y = expression("Δ Grass biomass ("*kg~ha^{-1}*")")) +
  theme_classic() +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
  theme(
    axis.title = element_text(size = 8),
    axis.text  = element_text(size = 8)
  )


# ---------------------------------------------------
# 3. Log Response Ratio (LRR)
# ---------------------------------------------------

Gbiomass_LRR_bc <- heights_data %>%
  filter(Year %in% c(2024, 2026)) %>%
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(
    mean_biomass = mean(Biomass_mean_kg_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Treatment_Group = ifelse(Treatment == "C", "C", "Treated")) %>%
  pivot_wider(
    names_from  = Year,
    values_from = mean_biomass,
    names_prefix = "Year_"
  ) %>%
  rename(Pre = Year_2024, Post = Year_2026) %>%
  group_by(Site, Fencing) %>%
  mutate(
    Control_Pre  = mean(Pre[Treatment_Group == "C"],  na.rm = TRUE),
    Control_Post = mean(Post[Treatment_Group == "C"], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(Treatment_Group == "Treated") %>%
  mutate(
    LRR = log((Post / Pre) / (Control_Post / Control_Pre))
  ) %>%
  dplyr::select(Site, Plot, Subplot, Treatment, Fencing, Pre, Post, LRR)

# If zeros are present, add a small constant to avoid -Inf:
# log( (Post + 0.1) / (Pre + 0.1) / ((C_post + 0.1) / (C_pre + 0.1)) )

## LMM on LRR
GBiomlog_bc <- lmer(LRR ~ Treatment * Fencing + (1 | Site),
                    data = Gbiomass_LRR_bc)
summary(GBiomlog_bc)
  
# check_model(GBiomlog_bc, check = "qq")
# check_model(GBiomlog_bc, check = "normality")
# check_model(GBiomlog_bc, check = "homogeneity")
 


# ---------------------------------------------------
# 4. emmeans – lnRR point-range plot
# ---------------------------------------------------

GBemm_bc <- emmeans(GBiomlog_bc, ~ Treatment | Fencing)
GBplot_data_bc <- as.data.frame(GBemm_bc)

GBplot_data_bc$Treatment <- factor(GBplot_data_bc$Treatment,
                                   levels = c("F", "TF", "TFB", "THF"))
GBplot_data_bc$Fencing   <- factor(GBplot_data_bc$Fencing,
                                   levels = c("Unfenced", "Fenced"),
                                   labels = c("Unfenced", "Fenced"))


# ---------------------------------------------------
# 5. Convert lnRR to % change
# ---------------------------------------------------

GBemm_bc2      <- emmeans(GBiomlog_bc, ~ Treatment | Fencing)
GBraw_bc       <- as.data.frame(GBemm_bc2)

GBpct_bc <- GBraw_bc %>%
  mutate(
    pct_change   = (exp(emmean)    - 1) * 100,
    CI_lower_pct = (exp(lower.CL)  - 1) * 100,
    CI_upper_pct = (exp(upper.CL)  - 1) * 100
  ) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(
    Treatment = factor(Treatment, levels = c("F", "TF", "TFB", "THF")),
    Fencing   = factor(Fencing,   levels = c("Unfenced", "Fenced"),
                       labels = c("Unfenced", "Fenced"))
  )


## % change plot (Treatment on y-axis)
Gbc_bc <- ggplot(GBpct_bc, aes(x = pct_change, y = Treatment, color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.5) +
  geom_point(size = 2.0, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
                 height = 0.5, linewidth = 0.9, position = position_dodge(0.5)) +
  scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
  labs(x = "Change in aboveground grass biomass relative to Control (%)",
       y = "Treatment") +
  theme_classic() +
  ggtitle(NULL) +
  theme(
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 12)
  )


# ---------------------------------------------------
# 7. Multipanel plot and save
# ---------------------------------------------------

multi_panelBiom_bc <- (GbA_bc / Gbb_bc / Gbc_bc) +
  plot_layout(heights = c(1, 1, 1)) +
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 8, hjust = 0))
  ) &
  theme(
    axis.text  = element_text(size = 10),
    axis.title = element_text(size = 10),
    plot.tag   = element_text(size = 10, hjust = 0)
  )

# save multi-panel plot
ggsave(multi_panelBiom_bc,
       filename = "Plots/GrassBIOMASS.png",
       width = 16, height = 14, units = "cm")



## Direct planned contrasts: THF vs each other treatment within Fenced.

emm_gb_fenced <- emmeans(GBiomlog_bc, ~ Treatment, at = list(Fencing = "Fenced"))

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

### Fencing effect within each treatment ------------------------------------
# Contrasts Fenced vs Unfenced for F, TF, TFB, THF 

emm_gb_fence_by_trt <- emmeans(GBiomlog_bc, ~ Fencing | Treatment)

fencing_gb_within_trt <- pairs(
  emm_gb_fence_by_trt,
  reverse = TRUE,    # Fenced - Unfenced (positive = Fenced has higher LRR)
  adjust  = "holm"
)

print(fencing_gb_within_trt)
confint(fencing_gb_within_trt)



######################## GRASS Biomass vs Seedling density

#grass biomass combined with seedlings data


gb <- delta_GRBiomass_bc |>
  dplyr::select(Site, Plot, Subplot, Treatment, Fencing, delta_GR)

scatter_data <- Seedlings_Delta1 |>
  inner_join(gb, by = c("Site", "Plot", "Subplot", "Treatment", "Fencing"))


# Scatter plot (faceted by Fencing, with trend line) ───────────

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
    x = expression("Δ Aboveground grass biomass (kg "*ha^{-1}*")"),
    y = expression("Δ Seedling density "*ha^{-1}),
    color = "Treatment", shape = "Treatment"
  ) +
  theme_classic() +
  theme(axis.title   = element_text(size = 11),
        axis.text    = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text  = element_text(size = 9),
        strip.text   = element_text(size = 11))

# SAVING
ggsave(plot_seed, filename = "Plots/GrassBiomass VS SeedlingDensity.png",
       width = 16, height = 14, units = "cm", dpi = 300, bg = "white")


##################################################################################
##################################################################################

### GRASS SPECIES RICHNESS
# 1. Preparing data: richness per Site x Treatment x Year
Grass_rich <- Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2026)) %>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop")%>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

##reordering so that pre-treatment appears first then post treatment second on the plots
Grass_rich <- Grass_rich %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

#Grass species richness violin 

Grass_rich$Fencing <- factor( Grass_rich$Fencing,
  levels = c("Unfenced", "Fenced")) #ordering Fencing level to start with Unfenced


# plot
GrRa <- ggplot(Grass_rich, 
                    aes(x = Treatment, y = spp_richness, fill = Fencing)) + facet_wrap(~Period)+ 
  geom_violin(trim = TRUE)+
  #geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1.5, color = "black") +   
  labs(x = "Treatment", 
       y = "Grass species richness") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8)        # Axis tick labels
  ) +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))


###### LOG PROPORTIONAL CHANGE - spp richness

Grass_F <- Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2026)) %>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Year, Fencing) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop")

##Pivoting the two years side‑by‑side and compute Δ GRASS RICHNESS ─────────────
grassR_delta <- Grass_F %>%
  pivot_wider(names_from = Year, values_from = spp_richness, names_prefix = "Y") %>%
  mutate(delta = Y2026 - Y2024)


## LMM to test effect of treatment * fencing on Grass richness ##################

# Making "Unfenced" the reference level 

class(grassR_delta$Fencing)  # Likely "character" or "ordered factor"

# Converting to unordered factor explicitly
grassR_delta$Fencing <- factor(grassR_delta$Fencing, ordered = FALSE)

# Verifying
levels(grassR_delta$Fencing)  # Should show "fenced" "unfenced" (or vice versa)

# Setting "Fenced" as the reference level 
grassR_delta$Fencing <- relevel(grassR_delta$Fencing, ref = "Unfenced")

### Converting character variables to factors
grassR_delta$Treatment <- as.factor(grassR_delta$Treatment)
grassR_delta$Fencing <- as.factor(grassR_delta$Fencing)

grassR_delta$Fencing <- factor(grassR_delta$Fencing, 
                                  levels = c("Unfenced", "Fenced"),
                                  labels = c("Unfenced", "Fenced"))

##LMM for grass richness
GrasRich1 <- lmer(delta ~ Treatment * Fencing + (1|Site),  
                  data = grassR_delta)
summary(GrasRich1)

# Model performance
# check_model(GrasRich1, check = "normality")
# check_model(GrasRich1, check = "homogeneity")
# check_model(GrasRich1, check = "qq")

# qqnorm(residuals(GrasRich1)) #whether residuals are approximately normal.
# qqline(residuals(GrasRich1))


## Plot LMM output
GrRb <- ggplot(grassR_delta,aes(x = Treatment, y = delta, fill = Fencing)) +
  geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1.5, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Grass species richness") +
  theme_classic() +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))+
  theme(
    axis.title = element_text(size = 8),  # Axis titles size
    axis.text = element_text(size = 8)) 
  

#########
############# USING LOG RESPONSE RATIO FOR GRASS RICHNESS  #####

###  Step 1: Calculating lnRR for each site, treatment, fencing ,
  
  GRich_LRR <- Grasses %>%
  filter(Year %in% c(2024, 2026)) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop") %>% 
  # Separate control and treatment
  mutate(Treatment_Group = ifelse(Treatment == "C", "C", "Treated")) %>%
  # Wide format: one row per Plot/Subquadrat with Pre and Post columns
  pivot_wider(
    names_from = Year,
    values_from = spp_richness,
    names_prefix = "Year_"
  ) %>%
  rename(Pre = Year_2024, Post = Year_2026) %>%
  # Calculating LRR for each treated plot using its paired control at the same Location
  group_by(Site, Fencing) %>%
  mutate(
    Control_Pre = mean(Pre[Treatment_Group == "C"], na.rm = TRUE),
    Control_Post = mean(Post[Treatment_Group == "C"], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(Treatment_Group == "Treated") %>%
  mutate(
    GrLRR = log( (Post / Pre) / (Control_Post / Control_Pre) )
  ) %>%
  dplyr::select(Site, Plot, Subplot, Treatment, Fencing, Pre, Post, GrLRR) 


# If zeros are present, add a small constant to avoid -Inf:  
#log( (Post + 0.1) / (Pre + 0.1) ) / (C_post + 0.1) / (C_pre + 0.1)

# run LMM
GRilog <- lmer(GrLRR ~ Treatment * Fencing + (1|Site), data = GRich_LRR)

summary(GRilog)


# Model performance
 #plot(GRilog)
 #check_model(GRilog, check = "homogeneity")
 #check_model(GRilog, check = "normality")
 #check_model(GRilog, check = "qq")

 #qqnorm(residuals(GRilog)) #whether residuals are approximately normal.
 #qqline(residuals(GRilog))


#####  visualising lnRR results using emmeans

# Getting estimated marginal means for both factors
GRemm_interaction <- emmeans(GRilog, ~ Treatment | Fencing)

# Converting to dataframe
GRplot_data <- as.data.frame(GRemm_interaction)

# Ensuring factors are properly labeled
GRplot_data$Treatment <- factor(GRplot_data$Treatment, 
                                levels = c("F", "TF", "TFB", "THF")) 

GRplot_data$Fencing <- factor(GRplot_data$Fencing, 
                              levels = c("Unfenced", "Fenced"),
                              labels = c("Unfenced", "Fenced"))

####### CONVERTING lnRR to PERCENTAGE CHANGE #####
# Get estimated marginal means for Treatment × Fencing interaction
GRemm_interaction <- emmeans(GRilog, ~ Treatment | Fencing)
plot_data_raw <- as.data.frame(GRemm_interaction)

# Converting to percentage change 
plot_data <- plot_data_raw
plot_data$pct_change <- (exp(plot_data_raw$emmean) - 1) * 100
plot_data$CI_lower_pct <- (exp(plot_data_raw$lower.CL) - 1) * 100
plot_data$CI_upper_pct <- (exp(plot_data_raw$upper.CL) - 1) * 100


# rounding off to 2 decimaL places
plot_data <- plot_data %>%
  mutate(across(where(is.numeric), round, 2))

# Clean up factors
plot_data$Treatment <- factor(plot_data$Treatment, 
                              levels = c("F", "TF", "TFB", "THF"))
plot_data$Fencing <- factor(plot_data$Fencing, 
                            levels = c("Unfenced", "Fenced"),
                            labels = c("Unfenced", "Fenced"))

# Check the data
head(plot_data)


# Percent change plot. y = Treatment
GrRD<-ggplot(plot_data, aes(x = pct_change, y = Treatment , color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.5) +
  geom_point(size = 2.0, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
                 height = 0.5, size = 0.9, position = position_dodge(0.5)) +
  scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
  #scale_x_continuous(breaks = seq(-50, 150, 20)) +
  labs(x = "Change in grass species richness relative to Control (%)",
       y = "Treatment") +
  # theme(legend.position = "top") +
  theme_classic()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))


#### multi-panel plots in a single layout
multi_panelRich <- (GrRa/GrRb/ GrRD) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1, 1, 1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 6, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 9),        # Increase axis label font size
    axis.title = element_text(size = 11),       # Increase axis title font size
    plot.tag = element_text(size = 10, hjust = 0)  # Ensure left alignment
  )

#saving plot
  ggsave(multi_panelRich,filename ="Plots/GrassRICHNESS ViolinLog1A.png",
       width = 16, height = 14, units = "cm")  



#### Is TFB in Fenced subplots the treatment with the greatest richness increase?
# Direct planned contrasts: TFB vs each other treatment within Fenced.

emm_gR_fenced <- emmeans(GRilog, ~ Treatment, at = list(Fencing = "Fenced"))

gR_tfb_contrasts <- contrast(
  emm_gR_fenced,
  list(
    "TFB vs F"   = c(-1,  0,  1,  0),
    "TFB vs TF"  = c( 0, -1,  1,  0),
    "TFB vs THF" = c( 0,  0,  1, -1)
  ),
  side = ">",   
  adjust = "holm"
)

print(gR_tfb_contrasts)


### EFFECT of fencing on grass richness
# Fencing effect within each treatment ------------------------------------
# Contrasts Fenced vs Unfenced for F, TF, TFB, THF

emm_gR_fence_by_trt <- emmeans(GRilog, ~ Fencing | Treatment)

fencing_gR_within_trt <- pairs(
  emm_gR_fence_by_trt,
  reverse = TRUE,    
  adjust  = "holm"
)

print(fencing_gR_within_trt)
confint(fencing_gR_within_trt)


 
################################################################################  

#### GRASS DIVERSITY

# Calculating Shannon-Wiener Diversity Index at Site and Plot level
GrSWeiner <- Grasses%>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2026)) %>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Year, Fencing, Number, Species_name)%>%
  summarise( 
    total_abundance = sum(Number, na.rm = TRUE),
    .groups = "drop_last"
  )


## Calculating Shannon-Wiener Diversity Index at treatment level

    # Step 1: Aggregate to subplot × species level
    subplot_abundance <- Grasses %>%
      filter(!is.na(Species_name), Year %in% c(2024, 2026)) %>%
      group_by(Site, Plot,Subplot, Treatment, Fencing, Year, Species_name) %>%
      summarise(
        total_abundance = sum(Number, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Step 2: Calculating Shannon directly (no pivot_wider needed)
    grSWdiversity <- subplot_abundance %>%
      group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
      summarise(
        Shannon = diversity(total_abundance, index = "shannon"),
        Richness = n_distinct(Species_name), 
        Total_grass = sum(total_abundance),
        .groups = "drop"
     )%>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

##reordering so that pre-treatment appears first then post treatment second on the plots
grSWdiversity <- grSWdiversity %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

###### violin pre vs post treatment
grSWdiversity$Fencing <- factor( grSWdiversity$Fencing,
                              levels = c("Unfenced", "Fenced")) #ordering Fencing level to start with Unfenced


# violin plot showing pre and post treatment

GSWa <- ggplot(grSWdiversity,aes(x = Treatment, y =Shannon, fill = Fencing))+ facet_wrap(~Period)+ 
    geom_violin(trim = TRUE)+
    #geom_hline(yintercept = 0, linetype = "dashed") +  
    stat_summary(fun = mean, geom = "point", 
                 position = position_dodge(0.8), 
                 size = 1.5, color = "black") +   
    labs(x = "Treatment", 
         y = "Shannon-Weiner index") +
    theme_classic() +
    theme(
      axis.title = element_text(size = 8),      # Axis titles
      axis.text = element_text(size = 8)        # Axis tick labels
    ) +
    scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))
  

####### DELTA SHANNON-WEINER DIVERSITY INDEX

  Gdiv <- subplot_abundance %>%
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(
    Shannon = diversity(total_abundance, index = "shannon"),
    Richness = n_distinct(Species_name),
    Total_grass = sum(total_abundance),
    .groups = "drop"
  )%>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


# Method 1: Using pivot_wider
  SW_Delta <- grSWdiversity %>%
    dplyr::select(Site, Subplot, Treatment, Fencing, Year, Shannon) %>%
  pivot_wider(
    names_from = Year,
    values_from = Shannon,
    names_prefix = "Shannon_"
  ) %>%
  mutate(
    delta_SW = Shannon_2026 - Shannon_2024
  ) %>%
  # Remove any rows with missing data
  filter(!is.na(delta_SW))


### Converting character variables to factors
SW_Delta$Treatment <- as.factor(SW_Delta$Treatment)
SW_Delta$Fencing <- as.factor(SW_Delta$Fencing)
SW_Delta$Fencing <- factor(SW_Delta$Fencing, 
                    levels = c("Unfenced", "Fenced"),
                     labels = c("Unfenced", "Fenced"))

### GLMM to test effect of treatment * fencing on Grass diversity ##################


class(SW_Delta$Fencing)  # Likely "character" or "ordered factor"

# Converting  to unordered factor explicitly
SW_Delta$Fencing <- factor(SW_Delta$Fencing, ordered = FALSE)

# Verify
levels(SW_Delta$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Unfenced" as the reference level 
SW_Delta$Fencing <- relevel(SW_Delta$Fencing, ref = "Unfenced")


### Converting character variables to factors
SW_Delta$Treatment <- as.factor(SW_Delta$Treatment)
SW_Delta$Fencing <- as.factor(SW_Delta$Fencing)


##LMM
GrasD <- lmer(delta_SW ~ Treatment * Fencing + (1|Site),  
              data = SW_Delta)

summary(GrasD)


# Model performance
 #plot(GrasD)
 #check_model(GrasD, check = "homogeneity")
 #check_model(GrasD, check = "normality")
 #check_model(GrasD, check = "qq")

 #qqnorm(residuals(GrasD)) #whether residuals are approximately normal.
 #qqline(residuals(GrasD))

# Plot LMM output grass diversity (delta)

GSWb <- ggplot(SW_Delta,aes(x = Treatment, y = delta_SW, fill = Fencing)) +
  geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1.5, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Shannon-Weiner index") +
  theme_classic() +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))+
  theme(
    axis.title = element_text(size = 8),  # Axis titles size
    axis.text = element_text(size = 8)) 



################ USING LOG RESPONSE RATIO FOR GRASS diversity  #####

################# Calculating lnRR using a constant of 0.01


# --- D1. Calculating Shannon diversity LRR --------------------------------------

# Step 1: Species-level abundance per subplot × year
subplot_abundance <- Grasses |>
  filter(!is.na(Species_name), Year %in% c(2024, 2026)) |>
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year, Species_name)  |>
  summarise(total_abundance = sum(Number, na.rm = TRUE), .groups = "drop")

# Step 2: Shannon index per subplot × year
sw_diversity <- subplot_abundance |>
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) |>
  summarise(
    Shannon = diversity(total_abundance, index = "shannon"),
    .groups = "drop"
  )

# Step 3: Computing LRR
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

# Checking for Inf / NaN
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

GDilog <- lmer(GdLRR ~ Treatment * Fencing + (1 | Site),
                data = GDiv_lnRR)

summary(GDilog)


# Model performance
# plot(GDilog)
# check_model(GDilog, check = "homogeneity")
# check_model(GDilog, check = "normality")
# check_model(GDilog, check = "qq")
 
# qqnorm(residuals(GDilog)) #whether residuals are approximately normal.
# qqline(residuals(GDilog))


##### Visualising lnRR results using emmeans

# Getting estimated marginal means for both factors
GDemm_interaction <- emmeans(GDilog, ~ Treatment | Fencing)

# Converting to dataframe
GDplot_data <- as.data.frame(GDemm_interaction)

# Ensuring factors are properly labeled
GDplot_data$Treatment <- factor(GDplot_data$Treatment, 
                                levels = c("F", "TF", "TFB", "THF")) 

GDplot_data$Fencing <- factor(GDplot_data$Fencing, 
                              levels = c("Unfenced", "Fenced"), 
                              labels = c("Unfenced", "Fenced"))

####### CONVERTING lnRR to PERCENTAGE CHANGE #####
# Getting estimated marginal means for Treatment × Fencing interaction
GDemm_interaction <- emmeans(GDilog, ~ Treatment | Fencing)
plot_data_raw <- as.data.frame(GDemm_interaction)

# Converting to percentage change
GDplot_data <- plot_data_raw 
GDplot_data$pct_change <- (exp(plot_data_raw$emmean) - 1) * 100
GDplot_data$CI_lower_pct <- (exp(plot_data_raw$lower.CL) - 1) * 100
GDplot_data$CI_upper_pct <- (exp(plot_data_raw$upper.CL) - 1) * 100

# rounding off to 2 decimaL places
GDplot_data <- GDplot_data %>%
  mutate(across(where(is.numeric), round, 2))

# Clean up factors
GDplot_data$Treatment <- factor(GDplot_data$Treatment, 
                              levels = c("F", "TF", "TFB", "THF"))
GDplot_data$Fencing <- factor(GDplot_data$Fencing, 
                            levels = c("Unfenced", "Fenced"),
                            labels = c("Unfenced", "Fenced"))


# Percent change plot. y = Treatment
GSWD<-ggplot(GDplot_data, aes(x = pct_change, y = Treatment , color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.5) +
  geom_point(size = 2.0, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
                 height = 0.5, size = 0.9, position = position_dodge(0.5)) +
  scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
  #scale_x_continuous(breaks = seq(-10, 25, 10)) +
  labs(x = "Change in grass Shannon-Weiner diversity relative to Control (%)",
       y = "Treatment") +
  theme_classic()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))


# # multipanel  plots in a single layout
SWmulti_panel <- (GSWa/GSWb/GSWD) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1, 1, 1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 8, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 8),        # Increase axis label font size
    axis.title = element_text(size = 10),       # Increase axis title font size
    plot.tag = element_text(size = 10, hjust = 0))  # Ensure left alignment
  
##ggsave multipanel plot
 ggsave(SWmulti_panel,filename ="Plots/Grass DIVERSITY ViolinLog1D.png",
       width = 16, height = 14, units = "cm")


#### Is TFB in Fenced subplots the treatment with the greatest diversity increase?
# Direct planned contrasts: TFB vs each other treatment within Fenced.

emm_gd_fenced <- emmeans(GDilog, ~ Treatment, at = list(Fencing = "Fenced"))

gd_tfb_contrasts <- contrast(
  emm_gd_fenced,
  list(
    "TFB vs F"   = c(-1,  0,  1,  0),
    "TFB vs TF"  = c( 0, -1,  1,  0),
    "TFB vs THF" = c( 0,  0,  1, -1)
  ),
  side = ">",   
  adjust = "holm"
)
print(gd_tfb_contrasts)


### EFFECT of fencing on grass diversity
# Fencing effect within each treatment ------------------------------------
# Contrasts Fenced vs Unfenced for F, TF, TFB, THF

emm_gd_fence_by_trt <- emmeans(GDilog, ~ Fencing | Treatment)

fencing_gd_within_trt <- pairs(
  emm_gd_fence_by_trt,
  reverse = TRUE,    
  adjust  = "holm"
)

print(fencing_gd_within_trt)
confint(fencing_gd_within_trt)


############################################################################################


