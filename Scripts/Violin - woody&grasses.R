### THIS SCRIPT HAS BEEN USED TO CREATE A MULTI-PANEL FIGURE FOR EACH RESPONSE VARIABLE 
  # THAT SHOWS COLOR CODED VIOLIN PLOTS and Marginal effects. 


##### data used in this section is found in script "Refined GLMM - woody& grasses" 


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
 ggsave(multi_panelsE,filename ="Plots/Multipanel 2EMMViolin Seedlings.png",
       width = 16, height = 14, units = "cm")  




################### Violin SAPLINGS density
SapViolin<- ggplot(sptrt_comparison2, 
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


#Sapling density  Delta - simple Mean points on the violin
SpViolin <- ggplot(Saplings_Delta,aes(x = Treatment, y = delta_Sapsdens,fill = Fencing))+
  geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       #y = "Change in Sapling density per ha"
       y = expression("Change in Sapling density "*ha^{-1}*"")) +
  theme_classic() +
  #scale_fill_manual(values = c("Fenced" = "saddlebrown", "Unfenced" = "navajowhite"))
  #scale_fill_brewer(palette = "YlOrBr")  # Yellow-Orange-Brown palette
  #scale_fill_manual(values = c("#8c510a", "#d8b365"))  # Dark brown, light brown
  scale_fill_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365"))



### Marginal effects ggpredict for sapling density

Sapden1 <- ggpredict(Sapl5, terms = c("Treatment", "Fencing"))
Sap <- plot(Sapden1,colors = c( "#d8b365", "#8c510a"))  +    
  labs(#y = "Change in Sapling density per ha",
    y = expression("Change in Sapling density "*ha^{-1}*""),
       x = "Treatment") +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8))




###
##### EMM FOR SAPLING DENSITY
# Estimated marginal means for Treatment within Fencing 
Saplemm <- emmeans(Sapl5, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with Tukey adjustment (or "none" if you only want vs control)
spcontrast_vs_control <- contrast(Saplemm, method = "trt.vs.ctrl", ref = "C")
summary(spcontrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
Saplcld_emm <- cld(Saplemm, adjust = "Dunnett", Letters = letters, type = "response")
cld_tbl <- as.data.frame(Saplcld_emm)


# prepare clean database for plotting
splot_df <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace



## Visualisation using ggplot for saplings
spemm <- ggplot(splot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.35), width = 0.12) +
  geom_text(aes(label = Group,
                y = CI_upper + 1.5 * max(EMM)),
            position = position_dodge(width = 0.35), size = 3, color = "black") +
  scale_color_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365")) +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    #y = "Change in sapling density per ha",
    y = expression("Change in Sapling density "*ha^{-1}*"")
  ) +
  theme_classic()+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))




# Combine the plots in a single layout - SAPLINGS
multi_panel <- (SapViolin / SpViolin/ spemm) +   # "/" for stacking vertically, or "|" for side-by-side
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
 ggsave(multi_panel,filename ="Plots/Multipanel 2EMMViolin Saplings.png",
       width = 16, height = 14, units = "cm")  




####### RESPROUTS

##violin plot
RespVio <- ggplot(resprouts_df,
                  aes(x = Treatment, y = No_of_resprouts)) + facet_wrap(~Fencing)+ 
  geom_violin(trim = TRUE)+
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Average no.of resprouts per cut stump") +
  theme_beautiful() +
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )



##saving Violin PLOT - resprouts
ggsave(RespVio,filename ="Plots/RESPROUTS VIOLIN plot.png",
       width = 16, height = 14, units = "cm")  



#########################

#### Grass HEIGHT
Ghviolin<- ggplot(grass_height, 
               aes(x = Treatment, y = mean_DPM_Height, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_violin(trim = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") + 
  labs(x = "Treatment", 
       y = "Grass height (cm)",
  ) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 9),      # Axis titles
    axis.text = element_text(size = 9))+
  scale_fill_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))


###Visualise: Violin plot of Δ‑grass height. FONT INCREASED
GrasVio <- ggplot(delta_GRHeight,
                  aes(x = Treatment, y = delta_GR, fill = Fencing))+ 
  geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in Grass height (cm)") +
  theme_classic() +
  #scale_fill_manual(values = c("Fenced" = "saddlebrown", "Unfenced" = "navajowhite"))
  #scale_fill_brewer(palette = "YlOrBr")  # Yellow-Orange-Brown palette
  #scale_fill_manual(values = c("#8c510a", "#d8b365"))  # Dark brown, light brown
  scale_fill_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365"))



##Marginal effects grass height

Gheight <- ggpredict(Grashg1, terms = c("Treatment", "Fencing"))
ght1 <-plot(Gheight,colors = c( "#d8b365", "#8c510a")) + 
  labs(y = "Change in Grass height (cm)",
       x = "Treatment") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8)        # Axis tick labels
  )


# Combine the plots in a single layout - grass height
multi_panelR <- (Ghviolin / GrasVio/ ght1) +   # "/" for stacking vertically, or "|" for side-by-side
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
#ggsave(multi_panelR,filename ="Plots/Multipanel OMViolin Grass height.png",
       width = 16, height = 14, units = "cm")  


########## GRASS BIOMASS
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





#############Grass species richness violin 

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



#Grass species richness Delta - simple Mean points on the violin
Grspp <- ggplot(grassR_delta,aes(x = Treatment, y = delta, fill = Fencing)) +
  geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in grass species richness") +
  theme_classic() +
  #scale_fill_manual(values = c("Fenced" = "saddlebrown", "Unfenced" = "navajowhite"))
  #scale_fill_brewer(palette = "YlOrBr")  # Yellow-Orange-Brown palette
  #scale_fill_manual(values = c("#8c510a", "#d8b365"))  # Dark brown, light brown
  scale_fill_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365"))



## Marginal effects for grass species richness
SppR2 <- ggpredict(modelRich, terms = c("Treatment", "Fencing"))
grsp1 <-plot(SppR2, colors = c( "#d8b365", "#8c510a")) + 
  labs(y = "Change in grass species richness",
       x = "Treatment") +
  theme_classic()+ 
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8)        # Axis tick labels
  )


######## EMM FOR GRASS RICHNESS
# Estimated marginal means for Treatment within Fencing 
GRemm <- emmeans(modelRich, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with Tukey adjustment (or "none" if you only want vs control)
grcontrast_vs_control <- contrast(Saplemm, method = "trt.vs.ctrl", ref = "C")
summary(grcontrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
Grlcld_emm <- cld(GRemm, adjust = "tukey", Letters = letters, type = "response")
cld_tbl <- as.data.frame(Grlcld_emm)


# prepare clean database for plotting
gplot_df <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace



## Visualisation using ggplot for grass richness
Grlemm <- ggplot(gplot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
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
    y = "Change in grass species richness",
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))



# Combine the plots in a single layout species richness
multi_panelR <- (GrRViolin / Grspp/ Grlemm) +   # "/" for stacking vertically, or "|" for side-by-side
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
ggsave(multi_panelR,filename ="Plots/Multipanel EMMViolin Grass richness.png",
       width = 16, height = 14, units = "cm")  




########## VIOLIN GRASS DIVERSITY
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


#### Grass Diversity Mean points on the violin
GrSW <- ggplot(SW_Delta,aes(x = Treatment, y =delta_SW, fill = Fencing)) +
  geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in Shannon-Weiner diversity") +
  theme_classic() +
  #scale_fill_manual(values = c("Fenced" = "saddlebrown", "Unfenced" = "navajowhite"))
  #scale_fill_brewer(palette = "YlOrBr")  # Yellow-Orange-Brown palette
  scale_fill_manual(values = c("#8c510a", "#d8b365"))  # Dark brown, light brown


##Marginal effects grass diversity
preds <- ggpredict(GrasD, terms = c("Treatment", "Fencing"))
grdiv1 <- plot(preds,colors = c( "#d8b365", "#8c510a")) +  
  labs(y = "Change in Shannon-Weiner diversity",
       x = "Treatment") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8)        # Axis tick labels
  )



######
## ### EMM FOR GRASS DIVERSITY
# Estimated marginal means for Treatment within Fencing 
GDemm <- emmeans(modelRich, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with Tukey adjustment (or "none" if you only want vs control)
gDcontrast_vs_control <- contrast(GDemm, method = "trt.vs.ctrl", ref = "C")
summary(gDcontrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
GDcld_emm <- cld(GDemm, adjust = "Dunnett", Letters = letters, type = "response")
cld_tbl <- as.data.frame(GDcld_emm)  # 


# prepare clean database for plotting
gdplot_df <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace



## Visualisation using ggplot for grass diversity
Gdm <- ggplot(gdplot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
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
    y = "Change in Shannon-Weiner index",
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))



#### Combine the plots in a single layout
multi_panelSWd <- (GSWViolin / GrSW/ Gdm) +   # "/" for stacking vertically, or "|" for side-by-side
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
ggsave(multi_panelSWd,filename ="Plots/Multipanel EMMViolin Grass diversity.png",
       width = 16, height = 14, units = "cm")  




#####################################################################
