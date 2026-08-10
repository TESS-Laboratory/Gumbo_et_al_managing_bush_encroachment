###
library(MASS)
library(tidyverse)
library(vegan)
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
library(performance)  # model diagnostics
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
library(gtsummary)
library(multcomp)
library(grid) # for multi-panels

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

# Read data
A <- read_csv("DATA/GEODE_Subplot_area.csv")
#B <- read.csv("DATA/March2025/WOODY2426.csv", stringsAsFactors = FALSE)
B <- read.csv("DATA/March2026/Trees2426.csv", stringsAsFactors = FALSE)


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
      between(Max_height.m., 1.5, 21.0)           ~ "Trees",
      TRUE                             ~ NA_character_
    ),
    Treatment = factor(Treatment),
    Fencing = factor(Fencing)
  )

# calculating seedling density
Seedlings <- SapF %>%
  filter(
    woody_cat == "Seedlings",
    Year %in% c(2024, 2026),
    !Treatment %in% "ZZZ"
  ) %>%
  count(
    Site, Plot, Subplot, Treatment, Fencing, Year, Area,
    name = "Seedlings"
  ) %>%
  mutate(
    density_ha = Seedlings * 10000 / Area
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


#################################DELTA SEEDLING DENSITY

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

# Set "Fenced" as the reference level 
Seedlings_Delta1$Fencing <- relevel(Seedlings_Delta1$Fencing, ref = "Unfenced")

### Convert character variables to factors
Seedlings_Delta1$Treatment <- as.factor(Seedlings_Delta1$Treatment)
Seedlings_Delta1$Fencing <- as.factor(Seedlings_Delta1$Fencing)



# using the LMM for analysis
Seedl5 <- lmer(delta_Seeddens ~ Treatment * Fencing + (1|Site),  
               data = Seedlings_Delta1)

summary(Seedl5)

#
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


## violin plot seedling delta
Seedbviolin <- ggplot(Seedlings_Delta1,
                      aes(x = Treatment, y = delta_Seeddens, fill = Fencing))+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       #y = "Change in Seedlings density per ha",
       y = expression("Change in Seedling density "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 6),  # Axis titles reduced from 12 to 8
    axis.text = element_text(size = 6)) +
  scale_fill_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365"))


##marginal effects plot
Seeden1 <- ggpredict(Seedl5, terms = c("Treatment", "Fencing"))
See <- plot(Seeden1, colors = c( "#d8b365", "#8c510a"))  +    
  labs(#y = "Change in Seedlings density per ha",
    y = expression("Change in Seedling density "*ha^{-1}*""),
    x = "Treatment") +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 6),      # Axis titles
    axis.text = element_text(size = 6))


## saving marginal effects plot
#ggsave(See,filename ="Plots/ME Change in seedling density.png",
width = 16, height = 14, units = "cm")  


########### USING EMMEANS
# Estimated marginal means for Treatment within Kraaling (if needed)
Seedemm <- emmeans(Seedl5, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with dunnetts adjustment (or "none" if you only want vs control)
contrast_vs_control <- contrast(Seedemm, method = "trt.vs.ctrl", ref = "C")
summary(contrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
Seedcld_emm <- cld(Seedemm, adjust = "Dunnett", Letters = letters, type = "response")
cld_tbl <- as.data.frame(Seedcld_emm)


# prepare clean database for plotting
plot_df <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace



## Visualisation using ggplot
semm <- ggplot(plot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
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
    y = expression("Change in Seedling density "*ha^{-1}*"")
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8))



## Combine the plots in a single layout
multi_panelsE <- (SeedViolin/Seedbviolin/ semm) +   # "/" for stacking vertically, or "|" for side-by-side
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

#saving using ggsave
ggsave(multi_panelsE,filename ="Plots/AreaEMMViolin26b Seedlings.png",
       width = 16, height = 14, units = "cm")  


#########################################################################################

### SAPLINGS

# calculating sapling density
Saplings <- SapF %>%
  filter(
    woody_cat == "Saplings",
    Year %in% c(2024, 2026),
    !Treatment %in% "ZZZ"
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
strt_comparison2 <- Saplings %>%
  filter(Year %in% c(2024, 2026)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


#reorder so that pre-treatment appears first then post treatment second on the plots
strt_comparison2 <- strt_comparison2 %>%
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

# Set "Fenced" as the reference level 
Saplings_Delta1$Fencing <- relevel(Saplings_Delta1$Fencing, ref = "Unfenced")

### Convert character variables to factors
Saplings_Delta1$Treatment <- as.factor(Saplings_Delta1$Treatment)
Saplings_Delta1$Fencing <- as.factor(Saplings_Delta1$Fencing)



# using the LMM for analysis
Sapl5 <- lmer(delta_Sapdens ~ Treatment * Fencing + (1|Site),  
               data = Saplings_Delta1)

summary(Sapl5)

#
## increasing font size for x and y axis
SapViolin<- ggplot(strt_comparison2, 
                    aes(x = Treatment, y = density_ha, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_violin(trim = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed") +    
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1.4, color = "black") +
  labs(x = "Treatment", 
       # y = "Saplings density per ha",
       y = expression("Sapling density "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8)        # Axis tick labels
  )+
  scale_fill_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))


## violin plot seedling delta
Sapbviolin <- ggplot(Saplings_Delta1,
                      aes(x = Treatment, y = delta_Sapdens, fill = Fencing))+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       #y = "Change in Saplings density per ha",
       y = expression("Change in Sapling density "*ha^{-1}*"")
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 6),  # Axis titles reduced from 12 to 8
    axis.text = element_text(size = 6)) +
  scale_fill_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365"))


##marginal effects plot
Sapden1 <- ggpredict(Sapl5, terms = c("Treatment", "Fencing"))
Sap <- plot(Sapden1, colors = c( "#d8b365", "#8c510a"))  +    
  labs(#y = "Change in Saplings density per ha",
    y = expression("Change in Sapling density "*ha^{-1}*""),
    x = "Treatment") +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 6),      # Axis titles
    axis.text = element_text(size = 6))


## saving marginal effects plot
#ggsave(Sap,filename ="Plots/ME Change in sapling density.png",
width = 16, height = 14, units = "cm")  



########### USING EMMEANS
# Estimated marginal means for Treatment within Fencing (if needed)
Sapemm <- emmeans(Sapl5, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with Dunnett adjustment (or "none" if you only want vs control)
contrast_vs_control <- contrast(Sapemm, method = "trt.vs.ctrl", ref = "C")
summary(contrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
Sapcld_emm <- cld(Sapemm, adjust = "Dunnett", Letters = letters, type = "response")
cld_tbl <- as.data.frame(Sapcld_emm)


# prepare clean database for plotting
spplot_df <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace


### Visualisation using ggplot
sapmm <- ggplot(spplot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
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
    y = expression("Change in Sapling density "*ha^{-1}*"")
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8))



## Combine the plots in a single layout
multi_panelsap <- (SapViolin/Sapbviolin/ sapmm) +   # "/" for stacking vertically, or "|" for side-by-side
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

#saving using ggsave
ggsave(multi_panelsap,filename ="Plots/AreaEMMViolin26b Seedlings.png",
       width = 16, height = 14, units = "cm")  


############################################################################################


############################ RESPROUTS  RESPROUTS RESPROUTS ####################

# Step 1: Filter Cut stumps only and years 
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


## check which species had more resprouts
Species_resprouts <- resprouts_df %>%
  group_by(Species_name, Treatment) %>%
  summarise(
    Total_resprouts = sum(No_of_resprouts, na.rm = TRUE),
    N = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_resprouts))



### Make "Unfenced" the reference level (to see "fenced" coefficients)
# Check current class of FieldType

class(resprouts_df$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
resprouts_df$Fencing <- factor(resprouts_df$Fencing, ordered = FALSE)

# Verify
levels(resprouts_df$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Unfenced" as the reference level (to see "Unfenced" coefficients)
resprouts_df$Fencing <- relevel(resprouts_df$Fencing, ref = "Unfenced")

### Convert character variables to factors
resprouts_df$Treatment <- as.factor(resprouts_df$Treatment)
resprouts_df$Fencing <- as.factor(resprouts_df$Fencing)


# Scale Variables to ensure convergence:
#resprouts_df$deltens_scaled <- as.numeric(scale(resprouts_df$No_of_resprouts))

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
       width = 16, height = 14, units = "cm")  



##GLMM for resprouts on cut stumps  

##using poisson family 
# Respr5b <- glmmTMB(No_of_resprouts ~ Treatment * Fencing + (1|Site),  
#                    data = resprouts_df, family = poisson(link = log))
# summary(Respr5b)
# 
# 
# # option 2 using negative binomial
# Respr5bc <- glmmTMB(No_of_resprouts ~ Treatment * Fencing + (1|Site),  
#                    data = resprouts_df, family = nbinom2(link = "log"))



# option 3 using tweedie distribution - to handle zeros

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

# 
# 
# ## LMM analysis
# Respr5D <- glmmTMB(No_of_resprouts ~ Treatment * Fencing + (1|Site),  
#                    data = resprouts_df, family = Gamma(link = "log"))
# 
# summary(Respr5D)
# # 
# # # Model diagnostics
# #  check_model(Respr5b, check = "qq")
# #  check_model(R5b_tweedie, check = "normality")
# # # check_model(Respr5b, check = "homogeneity")
# # # plot(Respr5b)
# 
#  qqnorm(residuals(R5b_tweedie)) #whether residuals are approximately normal.
# qqline(residuals(R5b_tweedie))



#### Using LMM instead of glmm
# Respr5c <- lmer(No_of_resprouts ~ Treatment * Fencing + (1|Site),  
#                 data = resprouts_df)
# 
# summary(Respr5c)

# Model diagnostics
# check_model(Respr5c, check = "qq")
# check_model(Respr5c, check = "normality")
# check_model(Respr5c, check = "homogeneity")
# plot(Respr5c)

# qqnorm(residuals(Respr5c)) #whether residuals are approximately normal.
# # qqline(residuals(Respr5c))
# 
# ## check model performance
# performance::check_model(Respr5c)
# 


### POST HOC ANALYSIS FOR RESPROUTS 
# pairwise comparisons
Rstreat_comparisons3 <- emmeans(R5b_tweedie, specs = pairwise ~ Treatment | Fencing, adjust = "Dunnet")
summary(Rstreat_comparisons3$contrasts)

# Estimated marginal means for Treatment within Fencing (if needed)
Rstreat_comparisons3 <- emmeans(R5b_tweedie, ~ Treatment | Fencing, type = "response")


## Test effect of fencing within each treatment level
 pairs(Rstreat_comparisons3, by = "Treatment")

# Compare each treatment to Control with Dunnett adjustment (or "none" if you only want vs control)
contrast_vs_control <- contrast(Rstreat_comparisons3, method = "trt.vs.ctrl", ref = "TF")
summary(contrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
Respcld_emm <- cld(Rstreat_comparisons3, adjust = "Dunnett", Letters = letters, type = "response")
cld_tbl <- as.data.frame(Respcld_emm)


# prepare clean database for plotting
Rspplot_df <- cld_tbl %>%
  rename(
    EMM = response,
    CI_lower = asymp.LCL, # tweedie used different typology for EMM and CIs
    CI_upper = asymp.UCL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace


# to check if log scale has been back transformed
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
 
 # # Combine the plots in a single layout
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

 
 
 

####### Creating new panel with share y-axis title
 
 # Remove internal left spacing
 RespVio <- RespVio +
   ylab(NULL) +
   theme(
     plot.margin = margin(0, 0, 0, 0),
     axis.title.y = element_blank()
   )
 
 Resp <- Resp +
   ylab(NULL) +
   theme(
     plot.margin = margin(0, 0, 0, 0),
     axis.title.y = element_blank()
   )
 
 # Shared y-axis title
 yleft <- wrap_elements(
   panel = textGrob(
     "Average number of resprouts per cut stump",
     rot = 90,
     gp = gpar(fontsize = 12)
   )
 )
 
 # Add annotations DIRECTLY to plots
 A_tag <- RespVio + labs(tag = "(a)")
 B_tag <- Resp + labs(tag = "(b)")
 
 # Combine plots
 panels <- A_tag / B_tag
 
 # Final layout
 multi_panelRessp <- yleft + panels +
   plot_layout(
     widths = c(0.05, 1)
   ) &
   theme(
     plot.tag = element_text(
       size = 11,
       face = "plain"
     ),
     #plot.tag.position = c(0.02, 0.98)
     plot.tag.position = c(0.1, 0.999)
   )
 
 multi_panelRessp
 
 
 
 
 # ## saving marginal effects plot
 ggsave(multi_panelRessp,filename ="Plots/ MultiP3 Resprouts.png",
        width = 16, height = 14, units = "cm") 
 

 
 
 
##################################################################################
###################################################################################

#### TREE DENSITY 
 
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
 
 
# #exclude
# Trees2 <- SapF %>%
#   filter(
#     woody_cat == "Trees ",
#     Year %in% c(2024, 2026),
#     BD != "Y" | is.na(BD))
#  %>%
#   count(
#     Site, Plot, Subplot, Treatment, Fencing, Year, Area,
#     name = "Trees"
#   ) %>%
#   mutate(
#     density_ha = Trees * 10000 / Area
#   )

#comparing at treatment level

Trees_treat <- Trees %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>% 
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop")  


#Summary stats for seedlings 
Treessummary_stats <- Trees %>%
  group_by(Treatment,Fencing,Year) %>%
  summarise(
    N = n(),                                   # number of observations per treatment
    mean_density = mean(density_ha, na.rm = TRUE),
    sd_density = sd(density_ha, na.rm = TRUE)
  ) %>%
  ungroup()

#sort pre and post treatment
tree_comparison2 <- Trees %>%
  filter(Year %in% c(2024, 2026)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


#reorder so that pre-treatment appears first then post treatment second on the plots
tree_comparison2 <- tree_comparison2 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

##################################DELTA SEEDLING DENSITY

#Pivot the two years side‑by‑side and compute Δ seedling density ─────────────
Trees_Delta1 <- Trees_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Treedens = dens_2026 - dens_2024) 



##### to test effect of treatment * fencing on seedling density###################

# Make "Unfenced" the reference level 

class(Trees_Delta1$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
Trees_Delta1$Fencing <- factor(Trees_Delta1$Fencing, ordered = FALSE)

# Verify
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


 
 
 # prepare trees data for LOG RESPONSE RATIO
 TreeLOG <- SapF %>%
   filter(
     woody_cat == "Trees",
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
     name = "Trees"
   ) %>%
   mutate(
     density_ha = Trees * 10000 / Area
   )
 
 #---------------------------------------------------
 # 2. Convert to wide format
 #---------------------------------------------------
 
 Tree_wide <- TreeLOG %>%
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
 
 Tcontrol_data <- Tree_wide %>%
   filter(Treatment == "C") %>%
   dplyr::select(
     Site,Plot,Subplot, Fencing,
     C_pre = Y2024,
     C_post = Y2026
   )
     
 
 ##Create control means
 Tcontrol_data <- Tree_wide %>%
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
 Tree_lnRR <- Tree_wide %>%
   filter(Treatment != "C") %>%
   left_join(
     Tcontrol_data,
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
 
 Tree_lnRR <- Tree_lnRR %>%
   mutate(
     lnRR = log(
       (Y2026 / Y2024) /
         (C_post / C_pre)))
 
 #---------------------------------------------------
 # 8. Treatment summaries
 #---------------------------------------------------
 
 Tree_summary <- Tree_lnRR %>%
   group_by(Treatment, Fencing) %>%
   summarise(
     mean_lnRR = mean(lnRR, na.rm = TRUE),
     sd_lnRR = sd(lnRR, na.rm = TRUE),
     n = n(),
     se_lnRR = sd_lnRR / sqrt(n)
   )
 
 #print(seedlingD_summary)
 
 
 ### Calculating n for each Treatment × Fencing combination
 sample_sizes2 <- Tree_lnRR %>%
   group_by(Treatment, Fencing) %>%
   summarise(
     n = n(),
     .groups = 'drop'
   )
 
 #---------------------------------------------------
 # 9. Mixed-effects model
 #---------------------------------------------------
 Treelog <- lmer(lnRR  ~ Treatment * Fencing + (1 | Site),
                 data = Tree_lnRR)
 
 summary(Treelog)
 
 # check_model(Treelog, check = "homogeneity")
 # check_model(Treelog, check = "normality")
 # check_model(Treelog, check = "qq")
 
 qqnorm(residuals(Treelog)) #whether residuals are approximately normal.
 qqline(residuals(Treelog))
 
 #---------------------------------------------------
 # 10. Plot lnRR
 #---------------------------------------------------
 
 ggplot(Tree_lnRR,
        aes(x = Treatment, y = lnRR, fill = Fencing))+ 
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +  
   stat_summary(fun = mean, geom = "point", 
                position = position_dodge(0.8), 
                size = 1, color = "black") +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Treatment", 
        y = expression("Log response ratio: Tree density "*ha^{-1}*"")
   ) +
   theme_classic() +
   theme(
     axis.title = element_text(size = 12),  # Axis titles reduced from 16 to 12
     axis.text = element_text(size = 12)) +
   scale_fill_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta"))
 
 
 ##### OPTION 2 visualising lnRR results using emmeans
 
 # Get estimated marginal means for both factors
 Tremm_interaction <- emmeans(Treelog, ~ Treatment | Fencing)
 
 ## Test effect of fencing within each treatment level
  pairs(Tremm_interaction, by = "Treatment")
 
 
 # Convert to dataframe
 Tplot_data <- as.data.frame(Tremm_interaction)
 
 # Ensure factors are properly labeled
 Tplot_data$Treatment <- factor(Tplot_data$Treatment, 
                                levels = c("F", "TF", "TFB", "THF"))
 Tplot_data$Fencing <- factor(Tplot_data$Fencing, 
                              levels = c("Unfenced", "Fenced"),
                              labels = c("Unfenced", "Fenced"))
 
 #### visualisation
 ggplot(Tplot_data, aes(x = emmean, y = Treatment, color = Fencing)) +
   geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.8) +
   geom_point(size = 3.5, position = position_dodge(0.5)) +
   geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL),
                  height = 0.2, size = 0.8, position = position_dodge(0.5)) +
   scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
   scale_x_continuous(breaks = seq(-2, 2, 0.5)) +
   labs(x = "LnRR Tree density relative to the control ",
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
 Tremm_interaction <- emmeans(Treelog, ~ Treatment | Fencing)
 tplot_data_raw <- as.data.frame(Tremm_interaction)
 
 # Convert to percentage change
 Tplot_data <- tplot_data_raw
 Tplot_data$pct_change <- (exp(tplot_data_raw$emmean) - 1) * 100
 Tplot_data$CI_lower_pct <- (exp(tplot_data_raw$lower.CL) - 1) * 100
 Tplot_data$CI_upper_pct <- (exp(tplot_data_raw$upper.CL) - 1) * 100
 
 # rounding off to 2 decimaL places
 Tplot_data <- Tplot_data %>%
   mutate(across(where(is.numeric), round, 2))
 
 # Clean up factors
 Tplot_data$Treatment <- factor(Tplot_data$Treatment, 
                                levels = c("F", "TF", "TFB", "THF"))
 Tplot_data$Fencing <- factor(Tplot_data$Fencing, 
                              levels = c("Unfenced", "Fenced"),
                              labels = c("Unfenced", "Fenced"))
 
 # Check the data
 #head(Tplot_data)
 
 # TESTING statistical difference on fenced and unfenced per treatment
 
 Tremm_interaction <- emmeans(Treelog, ~ Treatment | Fencing)
 
 pairs(Tremm_interaction, simple = "each", adjust = "tukey")
 
 
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
 Treed <- ggplot(Tplot_data, aes(x = pct_change, y = Treatment , color = Fencing)) +
   geom_vline(xintercept = 0, linetype = "longdash", color = "black", linewidth = 0.5) +
   geom_point(size = 2.0, position = position_dodge(0.5)) +
   geom_errorbarh(aes(xmin = CI_lower_pct, xmax = CI_upper_pct),
                  height = 0.5, size = 0.9, position = position_dodge(0.5)) +
   scale_color_manual(values = c("Fenced" = "#1B5", "Unfenced" = "magenta")) +
   scale_x_continuous(breaks = seq(-80, 50, 20)) +
   labs(x = "Change in tree density relative to Control (%)",
        y = "Treatment") +
   # theme(legend.position = "top") +
   theme_classic()+ 
   ggtitle(NULL)+
   theme(
     axis.title = element_text(size = 12),      # Axis titles
     axis.text = element_text(size = 12))
 
 
 
 ####  Combine the plots in a single layout
 multi_panelTree <- (Treea/Treeb/Treed) +   # "/" for stacking vertically, or "|" for side-by-side
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
 

 ##saving using ggsave
 ggsave(multi_panelTree,filename ="Plots/Tree2 density.png",
        width = 16, height = 14, units = "cm")  
 
