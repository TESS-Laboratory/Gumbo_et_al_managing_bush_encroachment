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
library(here) ## MOST IMPORTANT NOT TO FORGET THIS ONE
library(robustbase)
library(sjPlot)
library(flextable)
library(officer)
library(glmmTMB)
library(performance)  # model diagnostics

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

#Loading data
Ttdata <- read_csv("DATA/March2025/woody_with_separate_columns25.csv")

Ttdata <- Ttdata %>%
  filter(!is.na(Trees) %>%  # Exclude NAs in dpm_height
           mutate(
             Year = as.factor(Year),
             Site = as.factor(Site),
             Plot = as.factor(Plot),
             Subplot = as.factor(Subplot),
             Treatment = as.factor(Treatment),
             Fenced = as.factor(Fenced),
             Trees = as.numeric(Trees) # Ensure numeric
           )
         
        # Check missing values
         summary(Ttdata$Trees)
         
     ###2. Create Pre/Post Variable
  Ttdata <- Ttdata %>%
  mutate(period = ifelse(as.numeric(as.character(Year)) < 2025, "PRE", "POST")) %>%
  mutate(period = factor(period, levels = c("PRE", "POST"))
         
 ### Summarize by Subplot or Plot..Average height per subplot and period:
         
 summary_Ttrdata <- Ttdata %>%
 group_by(Site, Plot, Subplot, Treatment, period, Fenced) %>%
           summarise(mean_height = mean(Trees, na.rm = TRUE)) %>%
           ungroup()
         
####4. Calculate Change in Height (Post - Pre)
Ttheight_change <- summary_Ttrdata %>%
 pivot_wider(names_from = period, values_from = mean_height) %>%
  mutate(delta = POST - PRE)
         
### Compare Treatment Effects (ANOVA on Delta)
Ttanova_model <- aov(delta ~ Treatment, data = Ttheight_change)
 summary(Ttanova_model)
  # the results show that the treatment groups do not differ significantly in their effect on tree height
         
## Optional: Tukey post-hoc test
   TukeyHSD(Ttanova_model)
         
#### USE OF Mixed-Effects Model (Handles Repeated Measures)
 #This is more robust as it uses the original data, accounts for plot/subplot as random effects, and tests interaction between time and treatment:
         
  Ttlmm <- lmer(Trees ~ period * Treatment * Fenced + (1 | Site/Plot/Subplot), data = Ttdata)
         summary(Ttlmm)
         
  ##Plotting boxplot for seedlings before and after treatment
ggplot(Ttdata[!is.na(Ttdata$Trees), ],
 aes(x = Fenced, y = Trees, fill = Fenced)) + facet_wrap(~Treatment)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
           #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
 geom_hline(yintercept = 0, linetype = "dashed") +
 labs(x = "Treatment", y = "Tree max.height (m)") +
 theme_beautiful() +
 theme(legend.position = "none")
         
  # ### Violin plot for  height
ggplot(Ttdata[!is.na(Ttdata$Trees), ],
        aes(x = Fenced, y = Trees, fill = Fenced)) + facet_wrap(~Treatment)+
   geom_violin(trim = FALSE)+
   labs(x = "Treatment", y = "Tree max.height (m)") +
   theme_beautiful() +
   theme(legend.position = "none")
   
#saving violin plot
 #ggsave(TV,filename ="Plots/Height TREES FENCED Violinplot.png",
  #     width = 16, height = 14, units = "cm")


##################### DELTA CHANGE TREE HEIGHT
delta_density <- Tf %>%                             # your raw data frame
  group_by(Site, Plot, Subplot, Treatment, Fenced, Year) %>% 
  summarise(Trees = sum(!is.na(Trees)), .groups = "drop") %>%   # count trees
  mutate(Trees = Trees * 10000 / 600) %>%                       # m² → ha
  

Delta_height <- Ttdata %>%
  group_by(Site, Plot, Subplot, Treatment, Year, Fenced) %>%
  summarise(mean_height = mean(Trees, na.rm = TRUE)) %>%
  ungroup() %>%
filter(Year %in% c(2024, 2025)) %>%                           # keep the two years
  pivot_wider(names_from = Year,
              values_from = mean_height,
              names_glue = "d_{Year}") %>%                   
  mutate(delta_height25 = d_2024 - d_2025)   


# ### Violin plot for tree density
TH <- ggplot(Delta_height,
              aes(x = Fenced, y = delta_height25, fill = Fenced)) + facet_wrap(~Treatment)+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fencing", y = "Δ Trees max. height (m)") +
  theme_beautiful() +
  theme(legend.position = "none")

#saving violin plot
ggsave(TH,filename ="Plots/DELTA TREE Height FENCED Violinplot.png",
       width = 16, height = 14, units = "cm")


############################################## TREES  DENSITY /HA CHANGE 

Tf <- read_csv("DATA/March2025/woody_with_separate_columns25.csv")
#Group by Site and sum Seedlings, Saplings, and Trees
# Count the number of non-NA entries for Seedlings, Saplings, and Trees per site

site_counts <- Tf %>%
  group_by(Site, Plot, Subplot, Treatment, Year, Fenced) %>%
  summarise(
    Trees = sum(!is.na(Trees)))%>%
  mutate(across(c(Trees), ~ . * 10000 / 600))


# Reshape data for plotting
site_density_long <- site_counts %>%
  pivot_longer(cols = c(Trees), 
               names_to = "Tree", values_to = "Density")

# Plot the boxplot
ggplot(site_density_long,
       aes(x = Fenced, y = Density, fill = Fenced)) + facet_wrap(~Treatment)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fenced", y = "Trees per hectare") +
  theme_beautiful() +
  theme(legend.position = "none")


############################### DELTA CHANGE TREE DENSITY

delta_density <- Tf %>%                             # your raw data frame
  group_by(Site, Plot, Subplot, Treatment, Fenced, Year) %>% 
  summarise(Trees = sum(!is.na(Trees)), .groups = "drop") %>%   # count trees
  mutate(Trees = Trees * 10000 / 600) %>%                       # m² → ha
  filter(Year %in% c(2024, 2025)) %>%                           # keep the two years
  pivot_wider(names_from = Year,
              values_from = Trees,
              names_glue = "d_{Year}") %>%                   
  mutate(delta_dens_24_25 = d_2024 - d_2025)   

# Plot the box plot
ggplot(delta_density,
       aes(x = Fenced, y = delta_dens_24_25, fill = Fenced)) + facet_wrap(~Treatment)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fenced", y = "Trees per hectare") +
  theme_beautiful() +
  theme(legend.position = "none")

# plot violin
# ### Violin plot for tree density
Trd <- ggplot(delta_density,
aes(x = Fenced, y = delta_dens_24_25, fill = Fenced)) + facet_wrap(~Treatment)+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fenced", y = "Δ Trees per hectare") +
  theme_beautiful() +
  theme(legend.position = "none")

#saving violin plot
ggsave(Trd,filename ="Plots/TREE Density FENCED Violinplot.png",
     width = 16, height = 14, units = "cm")


###########################################################################################
#################################### SAPLING DENSITY  SAPLING DENSITY SALING DENSITY

