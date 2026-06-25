############# DETERMINING WOODY PLANTS DENSITY LOG RESPONSE RATIO

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


#---------------------------------------------------
# 1. Calculate weed density
#---------------------------------------------------

# Read data
A <- read_csv("DATA/GEODE_Subplot_area.csv")
B <- read.csv("DATA/March2026/WOODYP2426.csv", stringsAsFactors = FALSE)


# Ensure consistent column names (case-sensitive)
colnames(A) <- c("Site", "Plot", "Subplot", "Area")


# Merge Area into B
B_merged <- B %>%
  dplyr::left_join(
    A %>% dplyr::select(Site, Plot, Subplot, Area),
    by = c("Site", "Plot", "Subplot")
  )

####################################### DETERMINING WOODY PLANTS DENSITY 
# prepare data for seedlings
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

#sort pre and post treatment
strt_comparison2 <- Seedlings %>%
  filter(Year %in% c(2024, 2026)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


#reorder so that pre-treatment appears first then post treatment second on the plots
strt_comparison2 <- strt_comparison2 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

##################################DELTA SEEDLING DENSITY

#Pivot the two years side‑by‑side and compute Δ seedling density ─────────────
Seedlings_Delta1 <- Seed_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Seeddens = dens_2026 - dens_2024) 



##### to test effect of treatment * fencing on seedling density###################

# Make "Unfenced" the reference level 

class(Seedlings_Delta1$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
Seedlings_Delta1$Fencing <- factor(Seedlings_Delta1$Fencing, ordered = FALSE)

# Verify
levels(Seedlings_Delta1$Fencing)  


### Convert character variables to factors
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

## check model performance

performance::check_model(Seedl5) # this one not displaying plots

check_model(Seedl5, check = "qq")
check_model(Seedl5, check = "normality")
check_model(Seedl5, check = "homogeneity")
plot(Seedl5)

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


# prepare seedlings data for LOG RESPONSE RATIO
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


check_model(Seedllog, check = "qq")
check_model(Seedllog, check = "normality")
check_model(Seedllog, check = "homogeneity")
plot(Seedllog)

#---------------------------------------------------
# 10. Plot lnRR
#---------------------------------------------------

ggplot(SSeed_lnRR,
       aes(x = Treatment, y = lnRR, fill = Fencing))+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       y = expression("Log response Seedling density "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 6),  # Axis titles reduced from 12 to 8
    axis.text = element_text(size = 6)) +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))


##### OPTION 2 visualising lnRR results using emmeans

# Get estimated marginal means for both factors
emm_interaction <- emmeans(Seedllog, ~ Treatment | Fencing)

# Convert to dataframe
plot_data <- as.data.frame(emm_interaction)

# Ensure factors are properly labeled
plot_data$Treatment <- factor(plot_data$Treatment, 
                              levels = c("F", "TF", "TFB", "THF"))

# plot_data$Fencing <- factor(plot_data$Fencing, 
#                              levels = c("Fenced", "Unfenced"),
#                              labels = c("Fenced", "Unfenced"))


# reodering fencing level 
plot_data$Fencing <- factor(plot_data$Fencing, 
                            levels = c("Unfenced", "Fenced"),
                            labels = c("Unfenced", "Fenced"))

#### visualisation
ggplot(plot_data, aes(x = emmean, y = Treatment, color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.8) +
  geom_point(size = 3.5, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL),
                 height = 0.2, size = 0.8, position = position_dodge(0.5)) +
  scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
  scale_x_continuous(breaks = seq(-2, 2, 0.5)) +
  labs(x = "lnRR Seedling density relative to Control",
       y = "Treatments") +
 #theme_beautiful() +
  theme(legend.position = "top") +
theme_classic()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))


# Save high-resolution versions
#ggsave("treatment_kraaling_interaction_point.png", p, width = 8, height = 5, dpi = 300, bg = "white")


####### CONVERT lnRR to PERCENTAGE CHANGE #####
# Get estimated marginal means for Treatment × Fencing interaction
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



##### Percentage change plot 
# SeedVc<- ggplot(plot_data, aes(x = Treatment, y = pct_change, color = Fencing, group = Fencing)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_point(position = position_dodge(0.3), size = 1.4) +
#   geom_errorbar(aes(ymin = CI_lower_pct, ymax = CI_upper_pct),
#                 position = position_dodge(0.3), width = 0.15) +
#   #geom_line(position = position_dodge(0.3)) +
#   scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"),name = "Fencing") +
#   scale_y_continuous(
#     breaks = seq(-100, 200, 50),  # Breaks every 50 from -100 to 200
#     #labels = function(x) paste0(x, "%")
#   )+
#   # scale_y_continuous(labels = function(x) paste0(x, "%")) +
#   labs(x = "Treatment", y = " % proprtional change Seedling density") +
#   theme_classic()

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


# Create multipanel plot

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


### LOLLIPOP PLOT
 
# ggplot(plot_data, aes(x = pct_change, y = Treatment, color = Fencing)) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 0.8) +
#   geom_segment(aes(x = 0, xend = pct_change, y = Treatment, yend = Treatment),
#                position = position_dodge(0.5), size = 0.8) +
#   geom_point(aes(x = pct_change), size = 4, position = position_dodge(0.5)) +
#   geom_errorbarh(aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
#                  height = 0.2, size = 0.6, position = position_dodge(0.5)) +
#   scale_color_manual(values = c("Unfenced" = "#D55E00", "Fenced" = "#009E73"),
#                      name = "Fencing") +
#   scale_x_continuous(breaks = seq(-100, 200, 25),
#                      labels = paste0(seq(-100, 200, 25), "%")) +
#   labs(x = " % change from the control",
#        y = NULL,
#        title = "Treatment effects on weed biomass") +
#   theme_minimal(base_size = 12) +
#   theme(plot.title = element_text(face = "bold", hjust = 0),
#         plot.subtitle = element_text(color = "gray40", hjust = 0),
#         axis.title = element_text(face = "bold"),
#         axis.text = element_text(color = "black"),
#         legend.position = "bottom",
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank())
#
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


#Summary stats for saplings 
Sapsummary_stats <- Saplings %>%
  group_by(Treatment,Fencing,Year) %>%
  summarise(
    N = n(),                                   # number of observations per treatment
    mean_density = mean(density_ha, na.rm = TRUE),
    sd_density = sd(density_ha, na.rm = TRUE)
  ) %>%
  ungroup()

#sort pre and post treatment
sptrt_comparison2 <- Saplings %>%
  filter(Year %in% c(2024, 2026)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


#reorder so that pre-treatment appears first then post treatment second on the plots
sptrt_comparison2 <- strt_comparison2 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))


#################DELTA SAPLING DENSITY

#Pivot the two years side‑by‑side and compute Δ sapling density ─────────────
Saplings_Delta1 <- Sap_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Sapdens = dens_2026 - dens_2024) 



##### to test effect of treatment * fencing on seedling density###################

# Make "Unfenced" the reference level 

class(Saplings_Delta1$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
Saplings_Delta1$Fencing <- factor(Saplings_Delta1$Fencing, ordered = FALSE)

# Verify
levels(Saplings_Delta1$Fencing)  


### Convert character variables to factors
Saplings_Delta1$Treatment <- as.factor(Saplings_Delta1$Treatment)

Saplings_Delta1$Fencing <- factor(Saplings_Delta1$Fencing, 
                                   levels = c("Unfenced", "Fenced"),
                                   labels = c("Unfenced", "Fenced"))

# Set "Fenced" as the reference level 
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
       # y = "Saplings density per ha",
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



# prepare saplings data for LOG RESPONSE RATIO
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
# 8. Treatment summaries
#---------------------------------------------------

saplingD_summary <- SSapl_lnRR %>%
  group_by(Treatment, Fencing) %>%
  summarise(
    mean_lnRR = mean(lnRR, na.rm = TRUE),
    sd_lnRR = sd(lnRR, na.rm = TRUE),
    n = n(),
    se_lnRR = sd_lnRR / sqrt(n)
  )

#print(seedlingD_summary)


### Calculating n for each Treatment × Fencing combination
sample_sizes2 <- SSapl_lnRR %>%
  group_by(Treatment, Fencing) %>%
  summarise(
    n = n(),
    .groups = 'drop'
  )

#---------------------------------------------------
# 9. Mixed-effects model
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
# 10. Plot lnRR
#---------------------------------------------------

ggplot(SSapl_lnRR,
       aes(x = Treatment, y = lnRR, fill = Fencing))+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       y = expression("Log response ratio: Sapling density "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),  # Axis titles reduced from 16 to 12
    axis.text = element_text(size = 12)) +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))


##### OPTION 2 visualising lnRR results using emmeans

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

#### visualisation
ggplot(Splot_data, aes(x = emmean, y = Treatment, color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.8) +
  geom_point(size = 3.5, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL),
                 height = 0.2, size = 0.8, position = position_dodge(0.5)) +
  scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
  scale_x_continuous(breaks = seq(-2, 2, 0.5)) +
  labs(x = "LnRR Sapling density relative to the control ",
       y = "Treatment") +
  #theme_beautiful() +
  theme(legend.position = "top") +
  theme_classic()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))


# Save high-resolution versions
#ggsave("treatment_kraaling_interaction_point.png", p, width = 8, height = 5, dpi = 300, bg = "white")


####### CONVERT lnRR to PERCENTAGE CHANGE #####
# Get estimated marginal means for Treatment × Fencing interaction
Spemm_interaction <- emmeans(Sapllog, ~ Treatment | Fencing)
splot_data_raw <- as.data.frame(Spemm_interaction)

# Convert to percentage change
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


##### Percentage change plot 
# SapVc<- ggplot(Splot_data, aes(x = Treatment, y = pct_change, color = Fencing, group = Fencing)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_point(position = position_dodge(0.3), size = 1.5) +
#   geom_errorbar(aes(ymin = CI_lower_pct, ymax = CI_upper_pct),
#                 position = position_dodge(0.3), width = 0.15) +
#   #geom_line(position = position_dodge(0.3)) +
#   scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"),
#                      name = "Fencing") +
#   scale_y_continuous(
#     breaks = seq(-100, 200, 25),  # Breaks every 50 from -100 to 200
#     #labels = function(x) paste0(x, "%")
#   )+
#   labs(x = "Treatment", y = "Proportional Δ sapling density(%)") +
#   theme_classic()


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


####  Combine the plots in a single layout
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





######################################################################################################
##################################################################################################

######   GRASS BIOMASS GRASS BIOMASS GRASS BIOMASS GRASS BIOMASS

# Load data
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

# round off 
heights_data$Biomass_kg_ha <- round(predict_biomass(heights_data$DPM_Height),2)

# Save
#write.csv(heights_data, "biomass_predictions.csv", row.names = FALSE)


########### determining Biomass

Biomasssummary2<- heights_data  %>%
  filter(Year %in% c(2024, 2026)) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(
    mean_Biomass = mean(Biomass_kg_ha, na.rm = TRUE),
    .groups = "drop"
  )

### Convert character variables to factors
heights_data$Treatment <- as.factor(heights_data$Treatment)
heights_data$Fencing <- as.factor(heights_data$Fencing)

# Calculate mean grass BIOMASS for pre and post treatment

biomass2 <- heights_data %>%
  filter(Year %in% c(2024, 2026)) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(
    mean_Biomass = mean(Biomass_kg_ha, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


##reorder so that pre-treatment appears first then post treatment second on the plots
biomass2 <- biomass2 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))


## VIOLIN plot- GRASS biomass post vs pre treatment 

biomass2$Fencing <- factor( sptrt_comparison2$Fencing,
  levels = c("Unfenced", "Fenced")) #ordering Fencing level to start with Unfenced


 # ggplot(biomass2, 
 #      aes(x = Treatment, y = mean_Biomass, fill = Period)) + facet_wrap(~Fencing)+ 
 #  geom_violin(trim = TRUE)+
 #  geom_hline(yintercept = 0, linetype = "dashed") +  
 #  stat_summary(fun = mean, geom = "point", 
 #               position = position_dodge(0.8), 
 #               size = 1.5, color = "black") + 
 #  labs(x = "Treatment", 
 #       #y = "Above-ground grass biomass (kgDM/ha)",
 #       y = expression("Above-ground grass biomass ("*kg~ha^{-1}*")")
 #  ) +
 #  theme_classic()+
 #  theme(
 #    axis.title = element_text(size = 10),      # Axis titles
 #    axis.text = element_text(size = 10))+
 #  scale_fill_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))


## changing facet wrap fencing to period
 
 GbA<- ggplot(biomass2,aes(x = Treatment, y = mean_Biomass, fill = Fencing)) + facet_wrap(~Period)+ 
  geom_violin(trim = TRUE)+
  #geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1.5, color = "black") + 
  labs(x = "Treatment", 
       #y = "Above-ground grass biomass (kgDM/ha)",
       y = expression("Grass biomass ("*kg~ha^{-1}*")")
  ) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8))+
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))




######DELTA GRASS BIOMASS

# using the mean BIOMASS within each grouping for each year.
delta_GRBiomass <- heights_data  %>%
  filter(Year %in% c(2024, 2026)) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise (mean_Biomass = mean(Biomass_kg_ha, na.rm = TRUE),.groups = "drop")%>%
  pivot_wider(
    names_from  = Year,
    values_from = mean_Biomass,
    names_glue  = "Biomass_{Year}"
  ) %>%
  mutate(delta_GR = Biomass_2026 - Biomass_2024) %>%
  drop_na(delta_GR)   # keep groups where both years are present


## GLMM to test effect of treatment * fencing on Grass biomass ##################

# Make "Unfenced" the reference level 

class(delta_GRBiomass$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
delta_GRBiomass$Fencing <- factor(delta_GRBiomass$Fencing, ordered = FALSE)

# Verify
levels(delta_GRBiomass$Fencing)  # Should show "fenced" "unfenced" (or vice versa)


### Convert character variables to factors
delta_GRBiomass$Treatment <- as.factor(delta_GRBiomass$Treatment)
delta_GRBiomass$Fencing <- factor(delta_GRBiomass$Fencing, 
                                  levels = c("Unfenced", "Fenced"),
                                  labels = c("Unfenced", "Fenced"))

# Set "UnFenced" as the reference level 
delta_GRBiomass$Fencing <- relevel(delta_GRBiomass$Fencing, ref = "Unfenced")



##LMM for grass height
Grasbiom1 <- lmer(delta_GR ~ Treatment * Fencing + (1|Site),  
                data = delta_GRBiomass)
summary(Grasbiom1)


#check for singularity
performance::check_singularity(Grasbiom1) # FALSE desired shows- all random effects have nonzero variance → stable

# check model convergence
performance::check_convergence(Grasbiom1)

## check model performance

performance::check_model(Grasbiom1)

plot(Grasbiom1)
check_model(Grasbiom1, check = "homogeneity")
check_model(Grasbiom1, check = "normality")
check_model(Grasbiom1, check = "qq")



###Visualise: Violin plot of Δ‑grass biomass


Gbb <- ggplot(delta_GRBiomass,
                  aes(x = Treatment, y = delta_GR, fill = Fencing))+ 
  geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1.5, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Grass biomass ("*kg~ha^{-1}*")") +
  theme_classic() +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8))



#############
############### USING LOG RESPONSE RATIO FOR GRASS BIOMASS  #####

# Step 1: Calculate means for each site, treatment type, and year
biomass_LRR <- heights_data %>%
  filter(Year %in% c(2024, 2026)) %>%
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(MeanBiomass = mean(Biomass_kg_ha, na.rm = TRUE), .groups = "drop") 
  

### 

Gbiomass_LRR <- heights_data %>%
  filter(Year %in% c(2024, 2026)) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(
    mean_biomass = mean(Biomass_kg_ha, na.rm = TRUE),  # average subsamples to plot level
    .groups = "drop"
  ) %>%
  # Separate control and treatment
  mutate(Treatment_Group = ifelse(Treatment == "C", "C", "Treated")) %>%
  # Wide format: one row per Plot/Subquadrat with Pre and Post columns
  pivot_wider(
    names_from = Year,
    values_from = mean_biomass,
    names_prefix = "Year_"
  ) %>%
  rename(Pre = Year_2024, Post = Year_2026) %>%
  # Calculate LRR for each treated plot using its paired control at the same Location
  group_by(Site, Fencing) %>%
  mutate(
    Control_Pre = mean(Pre[Treatment_Group == "C"], na.rm = TRUE),
    Control_Post = mean(Post[Treatment_Group == "C"], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(Treatment_Group == "Treated") %>%
  mutate(
    LRR = log( (Post / Pre) / (Control_Post / Control_Pre) )
  ) %>%
  dplyr::select(Site, Plot, Subplot, Treatment, Fencing, Pre, Post, LRR)


# If zeros are present, add a small constant to avoid -Inf:  
   #log( (Post + 0.1) / (Pre + 0.1) ) / (C_post + 0.1) / (C_pre + 0.1) )

# run LMM
GBiomlog <- lmer(LRR ~ Treatment * Fencing + (1|Site), data = Gbiomass_LRR)

summary(GBiomlog)

# Plot LRR
ggplot(Gbiomass_LRR,
       aes(x = Treatment, y = LRR, fill = Fencing))+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       y = expression("Log response Aboveground biomass "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),  # Axis titles size
    axis.text = element_text(size = 12)) +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))


##### OPTION 2 visualising lnRR results using emmeans

# Get estimated marginal means for both factors
GBemm_interaction <- emmeans(GBiomlog, ~ Treatment | Fencing)

# Convert to dataframe
GBplot_data <- as.data.frame(GBemm_interaction)

# Ensure factors are properly labeled
GBplot_data$Treatment <- factor(GBplot_data$Treatment, 
                              levels = c("F", "TF", "TFB", "THF"))
GBplot_data$Fencing <- factor(GBplot_data$Fencing, 
                            levels = c("Unfenced", "Fenced"),
                            labels = c("Unfenced", "Fenced"))

#### visualisation
ggplot(GBplot_data, aes(x = emmean, y = Treatment, color = Fencing)) +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "black",linewidth = 0.5) +
  # geom_point(size = 3.5, position = position_dodge(0.5)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.8) +
  geom_point(size = 3.5, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL),
                 height = 0.2, size = 0.8, position = position_dodge(0.5)) +
  scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
  scale_x_continuous(breaks = seq(-2, 2, 0.5)) +
  labs(x = "Log Response Aboveground biomass",
       y = "Treatments") +
  #theme_beautiful() +
  theme(legend.position = "top") +
  theme_classic()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))


# Save high-resolution versions
#ggsave("treatment_kraaling_interaction_point.png", p, width = 8, height = 5, dpi = 300, bg = "white")


####### CONVERT lnRR to PERCENTAGE CHANGE #####
# Get estimated marginal means for Treatment × Fencing interaction
GBemm_interaction <- emmeans(GBiomlog, ~ Treatment | Fencing)
plot_data_raw <- as.data.frame(GBemm_interaction)

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


##### Percentage change plot 
# Gbc <- ggplot(plot_data, aes(x = Treatment, y = pct_change, color = Fencing, group = Fencing)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_point(position = position_dodge(0.3), size = 3) +
#   geom_errorbar(aes(ymin = CI_lower_pct, ymax = CI_upper_pct),
#                 position = position_dodge(0.3), width = 0.15) +
#   #geom_line(position = position_dodge(0.3)) +
#   scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"),
#                      name = "Fencing") +
#   scale_y_continuous(
#     breaks = seq(-100, 200, 5)#,  # Breaks every 50 from -100 to 200
#     #labels = function(x) paste0(x, "%")  # to include % on the scale
#   )+
#   labs(x = "Treatment", y = "Biomass proportional change (%)") +
#   theme_classic()+
#   theme(
#     axis.title = element_text(size = 8),      # Axis titles
#     axis.text = element_text(size = 8))


## Percent change. Ineverted axis. y = Treatment  

Gbc <- ggplot(plot_data, aes(x = pct_change, y = Treatment , color = Fencing)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.5) +
  geom_point(size = 2.0, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
                 height = 0.5, size = 0.9, position = position_dodge(0.5)) +
  scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
  #scale_x_continuous(breaks = seq(-50, 150, 20)) +
  labs(x = "Change in above-ground grass biomass relative to Control (%)",
       y = "Treatment") +
  # theme(legend.position = "top") +
  theme_classic()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))



####  Combine the plots in a single layout
multi_panelBiom <- (GbA/Gbb/ Gbc) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1, 1, 1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 8, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 10),        # Increase axis label font size
    axis.title = element_text(size = 10),       # Increase axis title font size
    plot.tag = element_text(size = 10, hjust = 0))  # Ensure left alignment

#saving using ggsave
ggsave(multi_panelBiom,filename ="Plots/GrassBIOMASS violinLog1b.png",
       width = 16, height = 14, units = "cm")  



##################################################################################


### GRASS SPECIES RICHNESS
# 1. Prepare data: richness per Site x Treatment x Year
Grass_rich <- Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2026)) %>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop")%>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

##reorder so that pre-treatment appears first then post treatment second on the plots
Grass_rich <- Grass_rich %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

#Grass species richness violin 

Grass_rich$Fencing <- factor( Grass_rich$Fencing,
  levels = c("Unfenced", "Fenced")) #ordering Fencing level to start with Unfenced



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

##Pivot the two years side‑by‑side and compute Δ GRASS RICHNESS ─────────────
grassR_delta <- Grass_F %>%
  pivot_wider(names_from = Year, values_from = spp_richness, names_prefix = "Y") %>%
  mutate(delta = Y2026 - Y2024)


## LMM to test effect of treatment * fencing on Grass richness ##################

# Make "Unfenced" the reference level 

class(grassR_delta$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
grassR_delta$Fencing <- factor(grassR_delta$Fencing, ordered = FALSE)

# Verify
levels(grassR_delta$Fencing)  # Should show "fenced" "unfenced" (or vice versa)

# Set "Fenced" as the reference level 
grassR_delta$Fencing <- relevel(grassR_delta$Fencing, ref = "Unfenced")

### Convert character variables to factors
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
#plot(GrasRich1)
# check_model(GrasRich1, check = "homogeneity")
# check_model(GrasRich1, check = "normality")
# check_model(GrasRich1, check = "qq")
# 
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

  
###  Step 1: Calculate lnRR for each site, treatment, fencing ,
  
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
  # Calculate LRR for each treated plot using its paired control at the same Location
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
plot(GRilog)
check_model(GRilog, check = "homogeneity")
check_model(GRilog, check = "normality")
check_model(GRilog, check = "qq")

qqnorm(residuals(GRilog)) #whether residuals are approximately normal.
qqline(residuals(GRilog))

# Plot LRR
ggplot(GRich_LRR,
       aes(x = Treatment, y = GrLRR, fill = Fencing))+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       y = expression("Log response grass richness "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),  # Axis titles size
    axis.text = element_text(size = 12)) +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))


##### OPTION 2 visualising lnRR results using emmeans

# Get estimated marginal means for both factors
GRemm_interaction <- emmeans(GRilog, ~ Treatment | Fencing)

# Convert to dataframe
GRplot_data <- as.data.frame(GRemm_interaction)

# Ensure factors are properly labeled
GRplot_data$Treatment <- factor(GRplot_data$Treatment, 
                                levels = c("F", "TF", "TFB", "THF"))
GRplot_data$Fencing <- factor(GRplot_data$Fencing, 
                              levels = c("Unfenced", "Fenced"),
                              labels = c("Unfenced", "Fenced"))

#### visualisation
ggplot(GRplot_data, aes(x = emmean, y = Treatment, color = Fencing)) +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "black",linewidth = 0.5) +
  # geom_point(size = 3.5, position = position_dodge(0.5)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.8) +
  geom_point(size = 3.5, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL),
                 height = 0.2, size = 0.8, position = position_dodge(0.5)) +
  scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
  #scale_x_continuous(breaks = seq(-2, 0, 2)) +
  labs(x = "Log response grass richness",
       y = "Treatments") +
  #theme_beautiful() +
  theme(legend.position = "top") +
  theme_classic()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))


# Save high-resolution versions
#ggsave("treatment_kraaling_interaction_point.png", p, width = 8, height = 5, dpi = 300, bg = "white")


####### CONVERT lnRR to PERCENTAGE CHANGE #####
# Get estimated marginal means for Treatment × Fencing interaction
GRemm_interaction <- emmeans(GRilog, ~ Treatment | Fencing)
plot_data_raw <- as.data.frame(GRemm_interaction)

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

# Check the data
head(plot_data)


##### Percentage change plot 
# GrRC<-ggplot(plot_data, aes(x = Treatment, y = pct_change, color = Fencing, group = Fencing)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_point(position = position_dodge(0.3), size = 3) +
#   geom_errorbar(aes(ymin = CI_lower_pct, ymax = CI_upper_pct),
#                 position = position_dodge(0.3), width = 0.15) +
#   #geom_line(position = position_dodge(0.3)) +
#   scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"),
#                      name = "Fencing") +
#   scale_y_continuous(
#     breaks = seq(-100, 150, 5)#,  # Breaks every 50 from -100 to 200
#     #labels = function(x) paste0(x, "%")  # to include % on the scale
#   )+
#   labs(x = "Treatment", y = "Grass richness proportional change (%)") +
#   theme_classic()+
#   theme(axis.title = element_text(size = 8),  # Axis titles size
#    axis.text = element_text(size = 8)) 


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


####  Combine the plots in a single layout
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

#saving using ggsave
ggsave(multi_panelRich,filename ="Plots/GrassRICHNESS ViolinLog1A.png",
       width = 16, height = 14, units = "cm")  



 
###############################################################################  

#### GRASS DIVERSITY

# Calculate Shannon-Wiener Diversity Index at Site and Plot level
GrSWeiner <- Grasses%>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2026)) %>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Year, Fencing, Number, Species_name)%>%
  summarise(
    total_abundance = sum(Number, na.rm = TRUE),
    .groups = "drop_last"
  )


## Calculate Shannon-Wiener Diversity Index at treatment level

    # Step 1: Aggregate to subplot × species level
    subplot_abundance <- Grasses %>%
      filter(!is.na(Species_name), Year %in% c(2024, 2026)) %>%
      group_by(Site, Plot,Subplot, Treatment, Fencing, Year, Species_name) %>%
      summarise(
        total_abundance = sum(Number, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Step 2: Calculate Shannon directly (no pivot_wider needed)
    grSWdiversity <- subplot_abundance %>%
      group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
      summarise(
        Shannon = diversity(total_abundance, index = "shannon"),
        Richness = n_distinct(Species_name),
        Total_grass = sum(total_abundance),
        .groups = "drop"
     )%>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

##reorder so that pre-treatment appears first then post treatment second on the plots
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


### Convert character variables to factors
SW_Delta$Treatment <- as.factor(SW_Delta$Treatment)
SW_Delta$Fencing <- as.factor(SW_Delta$Fencing)
SW_Delta$Fencing <- factor(SW_Delta$Fencing, 
                    levels = c("Unfenced", "Fenced"),
                     labels = c("Unfenced", "Fenced"))

### GLMM to test effect of treatment * fencing on Grass diversity ##################

# Make "Unfenced" the reference level 

class(SW_Delta$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
SW_Delta$Fencing <- factor(SW_Delta$Fencing, ordered = FALSE)

# Verify
levels(SW_Delta$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
SW_Delta$Fencing <- relevel(SW_Delta$Fencing, ref = "Unfenced")


### Convert character variables to factors
SW_Delta$Treatment <- as.factor(SW_Delta$Treatment)
SW_Delta$Fencing <- as.factor(SW_Delta$Fencing)


##LMM
GrasD <- lmer(delta_SW ~ Treatment * Fencing + (1|Site),  
              data = SW_Delta)

summary(GrasD)


# Model performance
plot(GrasD)
check_model(GrasD, check = "homogeneity")
check_model(GrasD, check = "normality")
check_model(GrasD, check = "qq")

qqnorm(residuals(GrasD)) #whether residuals are approximately normal.
qqline(residuals(GrasD))

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
# 
# ###  Step 1: Calculate lnRR for each site, treatment, fencing ,
# lnRR4_results <- grSWdiversity  %>%
#   mutate(
#     Period = ifelse(Year == 2024, "Pre", "Post"),
#     Treatment_Group = ifelse(Treatment == "C", "Control", "Treatment")
#   ) %>%
#   pivot_wider(
#     id_cols = c(Site,Plot, Subplot, Treatment, Fencing, Treatment_Group),
#     names_from = Period,
#     values_from = Shannon
#   ) %>%
#   
#   # Calculate control means in a separate summarised dataframe
#   group_by(Site, Fencing) %>%
#   reframe(
#     Control_Pre = mean(Pre[Treatment_Group == "Control"], na.rm = TRUE),
#     Control_Post = mean(Post[Treatment_Group == "Control"], na.rm = TRUE)
#   ) %>%
#   
#   # Join back to the original data
#   right_join(
#     grSWdiversity  %>%
#       mutate(
#         Period = ifelse(Year == 2024, "Pre", "Post"),
#         Treatment_Group = ifelse(Treatment == "C", "Control", "Treatment")
#       ) %>%
#       pivot_wider(
#         id_cols = c(Site, Plot, Subplot, Treatment, Fencing, Treatment_Group),
#         names_from = Period,
#         values_from = Shannon
#       ),
#     by = c("Site", "Fencing")
#   ) %>%
#   
#   # Keep only treated plots
#   filter(Treatment_Group == "Treatment") %>%
#   
#   # Calculate lnRR
#   mutate(
#     Treatment_ratio = Post / Pre,
#     Control_ratio = Control_Post / Control_Pre,
#     lnRR = log(Treatment_ratio / Control_ratio)
#   ) %>%
#   select(Site, Subplot, Treatment, Fencing, 
#          Pre, Post, Treatment_ratio, Control_ratio, lnRR)
# 
# #head(lnRR4_results)



################# Calculating lnRR using a constant of 0.01


# --- D1. Calculate Shannon diversity LRR --------------------------------------

# Step 1: Species-level abundance per subplot × year
subplot_abundance <- Grasses |>
  filter(!is.na(Species_name), Year %in% c(2024, 2026)) |>
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year, Species_name) |>
  summarise(total_abundance = sum(Number, na.rm = TRUE), .groups = "drop")

# Step 2: Shannon index per subplot × year
sw_diversity <- subplot_abundance |>
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) |>
  summarise(
    Shannon = diversity(total_abundance, index = "shannon"),
    .groups = "drop"
  )

# Step 3: Compute LRR
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

# Check for Inf / NaN
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


# # run LMM
# GDilog <- lmer(GdLRR ~ Treatment * Fencing + (1|Site), data = GRDiv_LRR)
# 
# summary(GDilog)


# Model performance
# plot(GDilog)
# check_model(GDilog, check = "homogeneity")
# check_model(GDilog, check = "normality")
# check_model(GDilog, check = "qq")
# 
# qqnorm(residuals(GDilog)) #whether residuals are approximately normal.
# qqline(residuals(GDilog))


# Plot LRR
ggplot(GDiv_lnRR,
       aes(x = Treatment, y = GdLRR, fill = Fencing))+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       y = expression("Grass diversity proportional change "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),  # Axis titles size
    axis.text = element_text(size = 12)) +
  scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))


##### OPTION 2 visualising lnRR results using emmeans

# Get estimated marginal means for both factors
GDemm_interaction <- emmeans(GDilog, ~ Treatment | Fencing)

# Convert to dataframe
GDplot_data <- as.data.frame(GDemm_interaction)

# Ensure factors are properly labeled
GDplot_data$Treatment <- factor(GDplot_data$Treatment, 
                                levels = c("F", "TF", "TFB", "THF"))
GDplot_data$Fencing <- factor(GDplot_data$Fencing, 
                              levels = c("Unfenced", "Fenced"),
                              labels = c("Unfenced", "Fenced"))

#### visualisation
ggplot(GDplot_data, aes(x = emmean, y = Treatment, color = Fencing)) +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "black",linewidth = 0.5) +
  # geom_point(size = 3.5, position = position_dodge(0.5)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.8) +
  geom_point(size = 3.5, position = position_dodge(0.5)) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL),
                 height = 0.2, size = 0.8, position = position_dodge(0.5)) +
  scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
  #scale_x_continuous(breaks = seq(-2, 0, 0.1)) +
  labs(x = "Log response grass diversity",
       y = "Treatments") +
  #theme_beautiful() +
  theme(legend.position = "top") +
  theme_classic()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))


####### CONVERT lnRR to PERCENTAGE CHANGE #####
# Get estimated marginal means for Treatment × Fencing interaction
GDemm_interaction <- emmeans(GDilog, ~ Treatment | Fencing)
plot_data_raw <- as.data.frame(GDemm_interaction)

# Convert to percentage change
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

##### Percentage change plot 
# GSWc<- ggplot(plot_data, aes(x = Treatment, y = pct_change, color = Fencing, group = Fencing)) +
# ggplot(plot_data, aes(x = Treatment, y = pct_change, color = Fencing, group = Fencing)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_point(position = position_dodge(0.3), size = 3) +
#   geom_errorbar(aes(ymin = CI_lower_pct, ymax = CI_upper_pct),
#                 position = position_dodge(0.3), width = 0.15) +
#   #geom_line(position = position_dodge(0.3)) +
#   scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"),
#                      name = "Fencing") +
#   scale_y_continuous(
#     breaks = seq(-100, 150, 5)#,  # Breaks every 50 from -100 to 200
#     #labels = function(x) paste0(x, "%")  # to include % on the scale
#   )+
#   labs(x = "Treatment", y = "Diversity proportional change (%)") +
#   theme_classic()+
#   theme(
#     axis.title = element_text(size = 8),  # Axis titles size
#     axis.text = element_text(size = 8)) 


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


# # Combine the plots in a single layout
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
  
##ggsave multipanel grass richness
ggsave(SWmulti_panel,filename ="Plots/Grass DIVERSITY ViolinLog1D.png",
       width = 16, height = 14, units = "cm")



############################################################################################

