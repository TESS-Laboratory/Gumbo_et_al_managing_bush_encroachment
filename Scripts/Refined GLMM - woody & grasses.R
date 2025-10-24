#### this script analyses data,with the TFB treatment excluded. 
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

SapF <- read_csv("DATA/March2025/WoodyPC4.csv")

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
#+
  #ggtitle("(a)")

##saving pre& post treatment BOXPLOT  -  excluding TFB
 # ggsave(Seedpp3,filename ="Plots/TFB PP Seedling Density BOXplot.png",
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



### Calculate absolute percentage change between 2024 and 2025 for each treatment
percentage_change <- Seedsummary_stats %>%
  pivot_wider(
    names_from = Year,
    values_from = c(mean_density, sd_density, N)
  ) %>%
  mutate(
    percentage_change = ((mean_density_2025 - mean_density_2024) / mean_density_2024) * 100,
    absolute_change = mean_density_2025 - mean_density_2024
  ) %>%
  select(Treatment, Fencing, mean_density_2024, mean_density_2025, absolute_change, percentage_change)

percentage_change

# Create a formatted flextable
word_table <- percentage_change %>%
  flextable() %>%
  set_caption("Table 1: Changes in Seedling Density (2024-2025)") %>%
  theme_zebra() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  colformat_num(j = c(2:5), digits = 3) %>%  # Adjust columns and decimal places
  autofit()

# Save as Word document
  #save_as_docx(word_table, path = "absolute_change_table.docx")


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


## glmm nested structure
See5 <- lmer(delta_Seeddens ~ Treatment * Fencing + (1|Site/Plot),  
                  data = Seedlings_Delta1)
summary(See5)



# using the LMM
Seed5 <- lmer(delta_Seeddens ~ Treatment * Fencing + (1|Site)+ (1|Plot),  # Crossed effects,
                  data = Seedlings_Delta1)

summary(Seed5)

# check model convergence
performance::check_convergence(Seed5)

## check model performance

performance::check_model(Seedl4)


### POST HOC ANALYSIS FOR SEEDLINGS 
# Tukey HSD pairwise comparisons
treat_comparisons <- emmeans(Seedl5, specs = pairwise ~ Treatment | Fencing, adjust = "tukey")
summary(treat_comparisons$contrasts)

### Marginal effects for treatment * fencing on seedlings
Seeden <- ggpredict(Seedl5, terms = c("Treatment", "Fencing"))
Se <- plot(Seeden) +     #Store the plot in an object for saving using ggsave later
  labs(y = "Change in seedling density per ha",
       x = "Treatment") +
  theme_beautiful()+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )
#+
  #ggtitle("(b)")

## saving marginal effects plot
ggsave(Se,filename ="Plots/ME Change in seedling density.png",
       width = 16, height = 14, units = "cm")  



# Combine the plots in a single layout
multi_panel <- (Seedpp3 / Se) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(3, 4)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 12, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 12),        # Increase axis label font size
    axis.title = element_text(size = 12),       # Increase axis title font size
    plot.tag = element_text(size = 12, hjust = 0)  # Ensure left alignment
  )

#saving using ggsave
ggsave(multi_panel,filename ="Plots/Multipanel Seedlings.png",
       width = 16, height = 14, units = "cm")  



#####another way of determining marginal effects 
# Calculate marginal effects for Treatment and Fencing
me_Treatment <- ggpredict(Seedl5, terms = "Treatment")
me_Fencing <- ggpredict(Seedl5, terms = "Fencing")


#Visualize marginal effects

# Plot marginal effects for Treatment
plot(me_Treatment)

# Plot marginal effects for Kraaling
plot(me_Fencing)


# Customize plot for Treatment
# Plot marginal effects with customization
plot(me_Treatment) +
  labs(title = "Marginal Effects of Treatment",
       x = "Treatment",
       y = "Predicted Outcome")

# Customize plot for Kraaling
plot(me_Fencing) +
  labs(title = "Marginal Effects of Treatment",
       x = "Fencing",
       y = "Predicted Outcome")


#Combine plots (optional)
#library(patchwork)
plot(me_Treatment) + plot(me_Fencing) # this does not show how treatment performs in fenced or unfenced




##SEEDLINGS demographics
##arrange seedlings count by treatmnet, fencing, year
SeedlingCOUNT <- SapF %>% 
  filter(woody_cat == "Seedlings", Year %in% c(2024, 2025),
         !Treatment %in% c("TFB")) %>% 
  group_by(Treatment, Fencing,Year, name = "Seedlings") %>% 
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Treatment, Fencing, Year, desc(Count))  # Sort by count descending



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

#Summary stats for saplings 
Saplsummary_stats <- Saplings %>%
  group_by(Treatment, Year) %>%
  summarise(
    N = n(),                                   # number of observations per treatment
    mean_density = mean(density_ha, na.rm = TRUE),
    sd_density = sd(density_ha, na.rm = TRUE)
  ) %>%
  ungroup()


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


##### Calculate absolute percentage change between 2024 and 2025 for each treatment
Sappercentage_change <- Saplsummary_stats %>%
  pivot_wider(
    names_from = Year,
    values_from = c(mean_density, sd_density, N)
  ) %>%
  mutate(
    percentage_change = ((mean_density_2025 - mean_density_2024) / mean_density_2024) * 100,
    absolute_change = mean_density_2025 - mean_density_2024
  ) %>%
  select(Treatment, mean_density_2024, mean_density_2025, absolute_change, percentage_change)

# percentage_change

# Create a formatted flextable
word_table <- Sappercentage_change %>%
  flextable() %>%
  set_caption("Table 2: Changes in Sapling Density (2024-2025)") %>%
  theme_zebra() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  colformat_num(j = c(2:5), digits = 3) %>%  # Adjust columns and decimal places
  autofit()

# Save as Word document
  #save_as_docx(word_table, path = "Sapling density absolute_change_table.docx")


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


#### Convert character variables to factors
Saplings_Delta$Treatment <- as.factor(Saplings_Delta$Treatment)
Saplings_Delta$Fencing <- as.factor(Saplings_Delta$Fencing)

#GLMM for sapling density 
Sapl1 <- glmmTMB(delta_Sapsdens ~ Treatment * Fencing + (1|Site) + (1|Plot),  
                 data = Saplings_Delta, family = gaussian(link = "identity"))
     #summary(Sapl1)   # model failed to converge


# second option without Plot 
Sapl4 <- glmmTMB(delta_Sapsdens ~ Treatment * Fencing + (1|Site),  
                  data = Saplings_Delta, family = gaussian(link = "identity"))
summary(Sapl4)

# LMM for sapling
Sapl5 <- lmer(delta_Sapsdens ~ Treatment * Fencing + (1|Site),  
                 data = Saplings_Delta)
summary(Sapl5)

#check if model converged
performance::check_convergence(Sapl5) # TRUE the model converged

# MODEL performance
performance::check_model(Sapl5)

### POST HOC ANALYSIS FOR SAPLINGS 

# Tukey HSD pairwise comparisons
Sptreat_comparisons <- emmeans(Sapl5, specs = pairwise ~ Treatment | Fencing, adjust = "tukey")
summary(Sptreat_comparisons$contrasts)


##visualising post-hoc analysis
Sp1treat_comparisons <- emmeans(Sapl5, ~ Treatment | Fencing)
plot(Sp1treat_comparisons) + geom_hline(yintercept=0, linetype="dashed")


## Marginal effects for saplings.   
Sapden <- ggpredict(Sapl5, terms = c("Treatment", "Fencing"))
Sp <- plot(Sapden) +     #Store the plot in an object for saving using ggsave later
  labs(y = "Change in sapling density per ha",
       x = "Treatment") +
  theme_beautiful()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)

## saving marginal effects plot 
  #ggsave(Sp,filename ="Plots/ME Change in sapling density.png",
       width = 16, height = 14, units = "cm")  



# Combine the plots in a single layout
multi_panel <- (Sapp1 / Sp) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(3, 4)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 12, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 12),        # Increase axis label font size
    axis.title = element_text(size = 12),       # Increase axis title font size
    plot.tag = element_text(size = 12, hjust = 0)  # Ensure left alignment
  )


ggsave(multi_panel,filename ="Plots/Multipanel Saplings.png",
       width = 16, height = 14, units = "cm")  



##SAPLINGS demographics
##arrange saplings count by treatmnet, fencing, year
SaplingCOUNT <- SapF %>% 
  filter(woody_cat == "Saplings", Year %in% c(2024, 2025),
         !Treatment %in% c("TFB")) %>% 
  group_by(Treatment, Fencing,Year, name = "Saplings") %>% 
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Treatment, Fencing, Year, desc(Count))  # Sort by count descending



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

### Convert character variables to factors
resprouts_df$Treatment <- as.factor(resprouts_df$Treatment)
resprouts_df$Fencing <- as.factor(resprouts_df$Fencing)


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


##using poisson family 
Respr5b <- glmmTMB(No_of_resprouts ~ Treatment * Fencing + (1|Site)+(1|Plot),  
                  data = resprouts_df, family = poisson(link = log))
summary(Respr5b)


#### Using LMM instead of glmm
Respr5c <- lmer(No_of_resprouts ~ Treatment * Fencing + (1|Site)+ (1|Plot),  
                  data = resprouts_df)

summary(Respr5c)

espr5c <- lme(No_of_resprouts ~ Treatment * Fencing + (1|Site)+ (1|Plot),  
               data = resprouts_df)


## check model performance
performance::check_model(Respr4)



### POST HOC ANALYSIS FOR RESPROUTS 
# Tukey HSD pairwise comparisons
Rstreat_comparisons3 <- emmeans(Respr4, specs = pairwise ~ Treatment | Fencing, adjust = "tukey")
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
## saving marginal effects plot
ggsave(Res,filename ="Plots/ME Resprouts.png",
       width = 16, height = 14, units = "cm") 


##RESPROUTS demographics
##arrange resprouts count by treatmnet, fencing, year
RespCOUNT <- resprouts_df %>% 
  group_by(Treatment, Fencing) %>%
  summarise(Total_Resprouts = sum(No_of_resprouts, na.rm = TRUE))
  



  

################################################################################################
## GRASSES     GRASSES   GRASSES GRASSES

Grasses <- read_csv("DATA/March2025/GrassesCombinedCleaned3.csv")

GrassHEIGHT<- read_csv("DATA/March2025/GrassHeight.csv")



summary_Gr <- GrassHEIGHT %>%
  filter(!is.na(DPM_Height),
         Year %in% c(2024, 2025),!Treatment %in% c("TFB")) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_DPM_Height = mean(DPM_Height, na.rm = TRUE)) %>%
  ungroup() 

### Convert character variables to factors
GrassHEIGHT$Treatment <- as.factor(GrassHEIGHT$Treatment)
GrassHEIGHT$Fencing <- as.factor(GrassHEIGHT$Fencing)

# Calculate mean grass height for pre and post treatment
grass_height2 <- GrassHEIGHT %>%
  filter(!is.na(DPM_Height),
         Year %in% c(2024, 2025),!Treatment %in% c("TFB"))%>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_DPM_Height = mean(DPM_Height, na.rm = TRUE),
            #n_observations = n(),
            .groups = "drop") %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

##reorder so that pre-treatment appears first then post treatment second on the plots
grass_height <- grass_height2 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))


#Summary stats for Grass height 
 Grasssummary_stats2 <- GrassHEIGHT %>%
   group_by(Treatment, Fencing, Year) %>%
    summarise(
         N = n(),                                   # number of observations per treatment
         mean_DPM_height = mean(DPM_Height, na.rm = TRUE),
         sd_DPM_height = sd(DPM_Height, na.rm = TRUE)
       ) %>%
     ungroup()

 
 ### Calculate absolute percentage change between 2024 and 2025 for each treatment
 GHpercentage_change2 <- Grasssummary_stats2 %>%
   pivot_wider(
     names_from = Year,
     values_from = c(mean_DPM_height, sd_DPM_height, N)
   ) %>%
   mutate(
     percentage_change = ((mean_DPM_height_2025 - mean_DPM_height_2024) / mean_DPM_height_2024) * 100,
     absolute_change = mean_DPM_height_2025 - mean_DPM_height_2024
   ) %>%
   select(Treatment, Fencing, mean_DPM_height_2024, mean_DPM_height_2025, absolute_change, percentage_change)
 
 
 # Create a formatted flextable
 word_table <- GHpercentage_change2 %>%
   flextable() %>%
   set_caption("Table : Changes in Grass DPM height") %>%
   theme_zebra() %>%
   bold(part = "header") %>%
   align(align = "center", part = "all") %>%
   colformat_num(j = c(2:5), digits = 3) %>%  # Adjust columns and decimal places
   autofit()
 
# Save as Word document
  #save_as_docx(word_table, path = "Grass3 DPM height absolute_change_table.docx")
 
 

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
delta_GRHeight <- GrassHEIGHT  %>%
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

### Convert character variables to factors
delta_GRHeight$Treatment <- as.factor(delta_GRHeight$Treatment)
delta_GRHeight$Fencing <- as.factor(delta_GRHeight$Fencing)

#GLMM for grass height 
Grashg <- glmmTMB(delta_GR ~ Treatment * Fencing + (1|Site),  
                 data = delta_GRHeight, family = gaussian(link = "identity"))
summary(Grashg)


##LMM for grass height
Grashg1 <- lmer(delta_GR ~ Treatment * Fencing + (1|Site) + (Plot),  
                  data = delta_GRHeight)
summary(Grashg1)


##Marginal effects grass height
Gheight <- ggpredict(Grashg, terms = c("Treatment", "Fencing"))
ght <-plot(Gheight) + 
  labs(y = "Change in grass DPM height (cm)",
      x = "Treatment") +
  theme_beautiful()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

## saving marginal effects plot
ggsave(ght,filename ="Plots/ME Change in Grass height.png",
       width = 16, height = 14, units = "cm") 



### Combine the plots in a single layout
multi_panel4 <- (Grbx1 / ght) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(3, 4)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 12, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 12),        # Increase axis label font size
    axis.title = element_text(size = 12),       # Increase axis title font size
    plot.tag = element_text(size = 12, hjust = 0)  # Ensure left alignment
  )


##ggsave multipanel grass height
ggsave(multi_panel4,filename ="Plots/Multipanel Grass Height.png",
       width = 16, height = 14, units = "cm")  




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



###### GLMM to test effect of treatment * fencing on Grass species richness ##################

# Make "Unfenced" the reference level 

class(grassR_delta$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
grassR_delta$Fencing <- factor(grassR_delta$Fencing, ordered = FALSE)

# Verify
levels(grassR_delta$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
grassR_delta$Fencing <- relevel(grassR_delta$Fencing, ref = "Unfenced")

# convert character to factor variables
### Convert character variables to factors
grassR_delta$Treatment <- as.factor(grassR_delta$Treatment)
grassR_delta$Fencing <- as.factor(grassR_delta$Fencing)


#GLMM for species richness 
Grasrich <- glmmTMB(delta ~ Treatment * Fencing + (1|Site)+ (1|Plot),  
                  data = grassR_delta, family = gaussian(link = "identity"))
summary(Grasrich)


## Using LMM instead of glmm
modelRich <- lmer(delta ~ Treatment * Fencing + (1|Site),  #including Plot produced error message
              data = grassR_delta)

summary(modelRich)


# check convergence
performance::check_convergence(modelRich)


# Tukey HSD pairwise comparisons
Richness_comparisons <- emmeans(modelRich, specs = pairwise ~ Treatment | Fencing, adjust = "tukey")
summary(Richness_comparisons$contrasts)



#Using ggeffects. Grass Species richness
SppR <- ggpredict(Grasrich, terms = c("Treatment", "Fencing"))
              # plot(SppR)
SppR2 <- ggpredict(modelRich, terms = c("Treatment", "Fencing"))
grsp <-plot(SppR2) + 
  labs(y = "Change in grass species richness",
       x = "Treatment") +
  theme_beautiful()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

## saving marginal effects plot
ggsave(grsp,filename ="Plots/ME Change in Grass species richness.png",
       width = 16, height = 14, units = "cm") 



# effects with separate plots for fenced and unfenced
eff1 <- allEffects(Grasrich)
summary(eff1)
plot(eff1)


### Combine the plots in a single layout
multi_panel5 <- (Grrbx1 / grsp) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(3, 4)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 12, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 12),        # Increase axis label font size
    axis.title = element_text(size = 12),       # Increase axis title font size
    plot.tag = element_text(size = 12, hjust = 0)  # Ensure left alignment
  )


##ggsave multipanel grass richness
ggsave(multi_panel5,filename ="Plots/Multipanel Grass richness.png",
       width = 16, height = 14, units = "cm")  



#####.  Grass species richness changes
Grass_rich <- Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025), !Treatment %in% c("TFB")) %>%   
  group_by(Treatment, Fencing, Year) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop") %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

# 2a. Calculate changes using pivot_wider 
Richness_changes <- Grasses %>%
  select(Year, Treatment) %>%
  pivot_wider(
    names_from = Year, 
    values_from = spp_richness,
    names_prefix = "richness_"
  ) %>%
  mutate(
    absolute_change = richness_2025 - richness_2024,
    percent_change = ((richness_2025 - richness_2024) / richness_2024) * 100
  ) %>%
  rename(
    pre_richness = richness_2024,
    post_richness = richness_2025
  )


### 3. Species list per treatment
Species_by_treatment <- Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025), !Treatment %in% c("TFB")) %>%
  distinct(Site, Treatment, Year, Species_name) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

# 4. Site-level composition
Site_richness <- Species_by_treatment %>%
  group_by(Site, Period) %>%
  reframe(
    total_species = n_distinct(Species_name),
    species_list = paste(sort(unique(Species_name)), collapse = ", ")
  )



#Total unique species by year and treatment
total_species <- Grass_rich %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025), 
         !Treatment %in% c("TFB")) %>%
  group_by(Treatment, Year) %>%
  summarise(total_unique_species = n_distinct(Species_name))


# Create a comprehensive species comparison
species_comparison <- Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025),
         !Treatment %in% c("TFB")) %>%
  distinct(Year, Species_name) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = Year, values_from = present, values_fill = 0) %>%
  rename(present_2024 = `2024`, present_2025 = `2025`) %>%
  mutate(
    status = case_when(
      present_2024 == 1 & present_2025 == 1 ~ "Present in both years",
      present_2024 == 1 & present_2025 == 0 ~ "Only in 2024",
      present_2024 == 0 & present_2025 == 1 ~ "Only in 2025",
      TRUE ~ "Unknown"
    )
  ) %>%
  arrange(status, Species_name)

# View the comparison
species_comparison

# Get summary statistics
species_summary <- species_comparison %>%
  group_by(status) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

print("Species turnover summary:")
print(species_summary)



#### Simple approach without creating intermediate objects
Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025),
         !Treatment %in% c("TFB")) %>%
  group_by(Treatment, Fencing, Year) %>%
  summarise(
    unique_species = list(sort(unique(Species_name))),
    count_species = n_distinct(Species_name)
  ) %>%
  ungroup()

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
  labs(x = "Treatment", y = "Shannon-Weiner diversity") +  #rename the y-axis
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

# Calculate Shannon-Wiener Diversity Index log transforming
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

### Convert character variables to factors
SW_Delta$Treatment <- as.factor(SW_Delta$Treatment)
SW_Delta$Fencing <- as.factor(SW_Delta$Fencing)


## Boxplot Grass Shannon_Weiner index
GSWc1 <- ggplot(SW_Delta,
               aes(x = Treatment, y =delta_SW)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in grass Shannon-Weiner diversity index") +
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

### Convert character variables to factors
SW_Delta$Treatment <- as.factor(SW_Delta$Treatment)
SW_Delta$Fencing <- as.factor(SW_Delta$Fencing)


#GLMM for grass diversity 
Grasdiv <- glmmTMB(delta_SW ~ Treatment * Fencing + (1|Site)+ (1|Plot),  
                  data = SW_Delta, family = gaussian(link = "identity"))

summary(Grasdiv)



#LMM
GrasD <- lmer(delta_SW ~ Treatment * Fencing + (1|Site),  
                   data = SW_Delta)

summary(GrasD)



## check model performance
performance::check_model(Grasdiv)

############# POST HOC ANALYSIS FOR GRASS DIVERSITY 
# Tukey HSD pairwise comparisons
Grass_diversity <- emmeans(GrasD, specs = pairwise ~ Treatment | Fencing, adjust = "tukey")
summary(Grass_diversity$contrasts)


# Using ggeffects on grass species diversity
preds <- ggpredict(GrasD, terms = c("Treatment", "Fencing"))
                  #plot(preds)
preds <- ggpredict(GrasD, terms = c("Treatment", "Fencing"))
grdiv <- plot(preds) +  
  labs(y = "Change in Shannon-Weiner Diversity",
       x = "Treatment") +
  theme_beautiful()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

## saving marginal effects plot
ggsave(grdiv,filename ="Plots/ME Change in Grass species Diversity.png",
       width = 16, height = 14, units = "cm") 



### Combine the plots in a single layout
multi_panel6 <- (GSW1 / grdiv) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(3, 4)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 12, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 10),        # Increase axis label font size
    axis.title = element_text(size = 10),       # Increase axis title font size
    plot.tag = element_text(size = 10, hjust = 0)  # Ensure left alignment
  )


##ggsave multipanel grass richness
ggsave(multi_panel6,filename ="Plots/Multipanel Grass Shannon diversity.png",
       width = 16, height = 14, units = "cm")  




### absolute change

##Summary stats for seedlings 
GrassDsummary_stats <- grSWdiversity %>%
  group_by(Treatment,Fencing, Year) %>%
  summarise(
    N = n(),                                   # number of observations per treatment
    ShannonDiversity = mean(Shannon_Diversity, na.rm = TRUE),
    sd_diversity = sd(Shannon_Diversity, na.rm = TRUE)
  ) %>%
  ungroup()

### Calculate absolute percentage change between 2024 and 2025 for each treatment
Shannonpercentage_change <- GrassDsummary_stats %>%
  pivot_wider(
    names_from = Year,
    values_from = c(ShannonDiversity, sd_diversity, N)
  ) %>%
  mutate(
    percentage_change = ((ShannonDiversity_2025 - ShannonDiversity_2024) / ShannonDiversity_2024) * 100,
    absolute_change = ShannonDiversity_2025 - ShannonDiversity_2024
  ) %>%
  select(Treatment, Fencing, ShannonDiversity_2024, ShannonDiversity_2025, absolute_change, percentage_change)



# Create a formatted flextable
word_table <- Shannonpercentage_change %>%
  flextable() %>%
  set_caption("Table: Changes in Grass Shannon-Weiner diversity (2024-2025)") %>%
  theme_zebra() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  colformat_num(j = c(2:5), digits = 3) %>%  # Adjust columns and decimal places
  autofit()

# Save as Word document
save_as_docx(word_table, path = "Shanno-Weiner absolute_change_table.docx")



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

#check number of NA
sum(is.na(Seedlings_Delta1$delta_Seeddens))
sum(is.na(Seedlings_Delta1$Treatment))
sum(is.na(Seedlings_Delta1$Fencing))

### Comprehensive check for N observations
comprehensive_N <- Grasses %>%
  filter(!is.na(DPM_Height),
         Year %in% c(2024, 2025),
         !Treatment %in% c("SPG")) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(
    N = n(),
    .groups = 'drop'
  ) %>%
  arrange(Site, Plot, Subplot, Treatment, Fencing, Year)


### steps to inserting astericks to show significance
# Extract coefficient table
coefs <- summary(GrasD)$coefficients

# Convert to data frame
pvals_df <- data.frame(
  term = rownames(coefs),
  p.value = coefs[, "Pr(>|t|)"]
)

# Create significance stars
pvals_df$stars <- cut(
  pvals_df$p.value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "")
)

pvals_df

#get marginal predictions
preds <- as.data.frame(ggpredict(GrasD, terms = c("Treatment", "Fencing")))
head(preds)


##matching 
# Create a helper column for matching
preds$term <- paste0("Treatment", preds$x)

# Merge stars into the prediction data
preds_annotated <- merge(preds, pvals_df, by = "term", all.x = TRUE)

# Replace NAs with empty strings
preds_annotated$stars[is.na(preds_annotated$stars)] <- ""


##add significance stars  
ggplot(preds_annotated, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, color = NA) +
  geom_text(
    aes(label = stars, y = conf.high + 1),  # stars above whiskers
    size = 6, color = "black"
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Marginal Effects of Treatment × Kraaling",
    x = "Treatment",
    y = "Predicted Height (cm)",
    color = "Fencing",
    fill = "Fencing"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )
