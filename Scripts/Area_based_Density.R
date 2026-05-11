###
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

# Read data
A <- read_csv("DATA/GEODE_Subplot_area.csv")
B <- read.csv("DATA/March2025/Woody2426b.csv", stringsAsFactors = FALSE)


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

# Compare each treatment to Control with Tukey adjustment (or "none" if you only want vs control)
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

### Make "Unfenced" the reference level (to see "fenced" coefficients)
# Check current class of FieldType

class(resprouts_df$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
resprouts_df$Fencing <- factor(resprouts_df$Fencing, ordered = FALSE)

# Verify
levels(resprouts_df$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level (to see "Unfenced" coefficients)
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
               size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Average no.of resprouts per cut stump") +
  theme_beautiful() +
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)) +
    scale_fill_manual(values = c("Fenced"   = "green", "Unfenced" = "magenta"))


##saving Violin PLOT - resprouts
ggsave(RespVio,filename ="Plots/RESPROUTS26 VIOLIN plot.png",
       width = 16, height = 14, units = "cm")  



##GLMM for resprouts on cut stumps  

##using poisson family 
Respr5b <- glmmTMB(No_of_resprouts ~ Treatment * Fencing + (1|Site),  
                   data = resprouts_df, family = poisson(link = log))
summary(Respr5b)


#### Using LMM instead of glmm
Respr5c <- lmer(No_of_resprouts ~ Treatment * Fencing + (1|Site),  
                data = resprouts_df)

summary(Respr5c)


## check model performance
performance::check_model(Respr5c)



### POST HOC ANALYSIS FOR RESPROUTS 
# Tukey HSD pairwise comparisons
Rstreat_comparisons3 <- emmeans(Respr5c, specs = pairwise ~ Treatment | Fencing, adjust = "tukey")
summary(Rstreat_comparisons3$contrasts)

# Using ggeffects on resprouts
Resprouts <- ggpredict(Respr5c, terms = c("Treatment", "Fencing"))
#plot(preds)
Resprouts <- ggpredict(Respr5c, terms = c("Treatment", "Fencing"))
Res <- plot(Resprouts) + 
  labs(y = "Average number of resprouts per cut stump",
       x = "Treatment") +
  theme_beautiful()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

# ## saving marginal effects plot
# ggsave(Res,filename ="Plots/ME Violin Resprouts.png",
#        width = 16, height = 14, units = "cm") 


  