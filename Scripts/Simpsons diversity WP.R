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


#FOR all woody plants
AllSD <- SapF %>% 
  filter( Year %in% c(2024, 2025),
         !Treatment %in% c("TFB")) %>% 
  group_by(Treatment, Fencing,Year, Species_name) %>% 
  summarise(abundance = n(), .groups = 'drop')


## Calculate species abundance per plot

# Calculate  Simpson's Index  
ASimp_diversity_simpson <- AllSD  %>%
  group_by(Treatment, Fencing, Year) %>% 
  summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')




# Calculate Simpson's Index for woody plants
ASisimpson <- AllSD %>%
  filter(Year %in% c(2024, 2025)) %>%
  group_by(Treatment, Fencing, Year) %>%
  summarise(
    total_individuals = sum(abundance, na.rm = TRUE),  
    sum_n_squared = sum(abundance * (abundance - 1), na.rm = TRUE),
    simpson_index = 1 - (sum_n_squared / (total_individuals * (total_individuals - 1))),
    .groups = 'drop'
  )

#sort pre and post treatment for woody plants
AsD_comparison3 <- ASisimpson %>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


#reorder so that pre-treatment appears first then post treatment second on the plots
AsD_comparison3 <- AsD_comparison3 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))




# Create point plot with facets for woody plants - 
  #ASIMP <- ggplot(AsD_comparison3, aes(x = Treatment, y = simpson_index, 
                           #color = as.factor(Period), shape = as.factor(Period))) +
  #geom_point(size = 2, position = position_dodge(0.3)) +
  #facet_wrap(~Fencing, ncol = 1) +
  #labs(x = "Treatment", 
   #    y = "Woody plants Simpson's diversity index", 
    #   color = "Period",
     #  shape = "Period") +
  #theme_classic() +
   #scale_color_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))



####### REFINED ggplot for combined treatment, period and fencing
# Ensure Period is ordered correctly

SDw <- ggplot(AsD_comparison3, aes(x = Period, y = simpson_index, 
                   color = Fencing, group = Fencing)) +
  
  # Add lines connecting Pre to Post for each Kraaling type
  geom_line(size = 1.2, alpha = 0.7) +
  
  # Add points
  geom_point(size = 2.5) +
  
  # Facet by Treatment - each treatment gets its own panel
  facet_grid(. ~ Treatment, scales = "free_x", space = "free_x") +
  
  # Y-axis limits
  scale_y_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
  
  # Customize colors
  scale_color_manual(values = c("Fenced" = "#D55E00", "Unfenced" = "#0072B2"),
                     name = "Fencing") +
  
  labs(x = "Period", y = "Woody plants Simpson's diversity index") +
  
  theme_beautiful() +
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 9.5, hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(0.2, "lines")  # Space between treatment facets
  )

### save plot
 #ggsave(SDw, filename = "Plots/S2impdiversityALL.png", width = 16, height = 12, units = "cm")



#################################################################################
############         CALCULATING SIMPSONS DIVERSITY INDEX FOR SEEDLINGS

###SEEDLINGS


#Filter SEEDLINGS  
Seedlings <- SapF %>% 
  filter(woody_cat == "Seedlings", Year %in% c(2024, 2025)) %>% 
  count(Site, Plot, Subplot, Treatment, Fencing,Year, name = "Seedlings") %>% 
  mutate(density_ha = Seedlings * 10000 / 600)     # convert to ha⁻¹

#filter TFB treatment
SDsimp <- SapF %>% 
  filter( Year %in% c(2024, 2025),
          !Treatment %in% c("TFB"), 
          woody_cat == "Seedlings")%>% 
  group_by(Treatment, Fencing,Year, Species_name) %>% 
  summarise(abundance = n(), .groups = 'drop')


# Calculate seedlings Simpson's diversity index for each plot by year
SeedSimp <- SDsimp %>%
  filter(Year %in% c(2024, 2025)) %>%
  group_by(Treatment, Fencing, Year) %>%
  summarise(
    total_individuals = sum(abundance, na.rm = TRUE),  
    sum_n_squared = sum(abundance * (abundance - 1), na.rm = TRUE),
    simpson_index = 1 - (sum_n_squared / (total_individuals * (total_individuals - 1))),
    .groups = 'drop'
  )


#sort pre and post treatment for woody plants
SED_comparison3 <- SeedSimp %>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))

#reorder so that pre-treatment appears first then post treatment second on the plots
SED_comparison3 <- SED_comparison3 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))


##GGPLOT
# Ensure Period is ordered correctly

Seedw <- ggplot(SED_comparison3, aes(x = Period, y = simpson_index, 
                                     color = Fencing, group = Fencing)) +
  
  # Add lines connecting Pre to Post for each Kraaling type
  geom_line(size = 1.2, alpha = 0.7) +
  
  # Add points
  geom_point(size = 2.5) +
  
  # Facet by Treatment - each treatment gets its own panel
  facet_grid(. ~ Treatment, scales = "free_x", space = "free_x") +
  
  # Y-axis limits
  scale_y_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
  
  # Customize colors
  scale_color_manual(values = c("Fenced" = "#D55E00", "Unfenced" = "#0072B2"),
                     name = "Fencing") +
  
  labs(x = "Period", y = "Seedlings Simpson's diversity index") +
  
  theme_beautiful() +
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 9.5, hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(0.2, "lines")  # Space between treatment facets
  )

### save plot
#ggsave(Seedw, filename = "Plots/SEEDSIMPpdiversityALL.png", width = 16, height = 12, units = "cm")


####################################
##### DETERMINING SAPLINGS SIMPSONS DIVERSITY

#Filter SAPLINGS  
Saplings <- SapF %>% 
  filter(woody_cat == "Saplings", Year %in% c(2024, 2025)) %>% 
  count(Site, Plot, Subplot, Treatment, Fencing,Year, name = "Saplings") %>% 
  mutate(density_ha = Saplings * 10000 / 600)     # convert to ha⁻¹

#filter TFB treatment
SPsimp <- SapF %>% 
  filter( Year %in% c(2024, 2025),
          !Treatment %in% c("TFB"), 
          woody_cat == "Saplings")%>% 
  group_by(Treatment, Fencing,Year, Species_name) %>% 
  summarise(abundance = n(), .groups = 'drop')


# Calculate seedlings Simpson's diversity index for each plot by year
SapSimp <- SPsimp %>%
  filter(Year %in% c(2024, 2025)) %>%
  group_by(Treatment, Fencing, Year) %>%
  summarise(
    total_individuals = sum(abundance, na.rm = TRUE),  
    sum_n_squared = sum(abundance * (abundance - 1), na.rm = TRUE),
    simpson_index = 1 - (sum_n_squared / (total_individuals * (total_individuals - 1))),
    .groups = 'drop'
  )


#sort pre and post treatment for woody plants
SAP_comparison3 <- SapSimp %>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


#reorder so that pre-treatment appears first then post treatment second on the plots
SAP_comparison3 <- SAP_comparison3 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))


##GGPLOT
# Ensure Period is ordered correctly

Sapw <- ggplot(SAP_comparison3, aes(x = Period, y = simpson_index, 
                                     color = Fencing, group = Fencing)) +
  
  # Add lines connecting Pre to Post for each Kraaling type
  geom_line(size = 1.2, alpha = 0.7) +
  
  # Add points
  geom_point(size = 2.5) +
  
  # Facet by Treatment - each treatment gets its own panel
  facet_grid(. ~ Treatment, scales = "free_x", space = "free_x") +
  
  # Y-axis limits
  scale_y_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
  
  # Customize colors
  scale_color_manual(values = c("Fenced" = "#D55E00", "Unfenced" = "#0072B2"),
                     name = "Fencing") +
  
  labs(x = "Period", y = "Saplings Simpson's diversity index") +
  
  theme_beautiful() +
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 9.5, hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(0.2, "lines")  # Space between treatment facets
  )

### save plot
#ggsave(Sapw, filename = "Plots/SAPsimpdiversityALL.png", width = 16, height = 12, units = "cm")


## Combine the plots in a single layout
 # removing legend on panel b and c

# Remove legend from plot Seedw
Seedw <- Seedw + theme(legend.position = "none")

# Remove legend from plot C  
Sapw <- Sapw + theme(legend.position = "none")

# Keep legend on plot A (if not already there)
SDw <- SDw + theme(legend.position = "top")

## removing facets on panel b and c
# First, remove facet strips from plots 
Seedw <- Seedw + theme(strip.text = element_blank(), strip.background = element_blank())
Sapw <- Sapw + theme(strip.text = element_blank(), strip.background = element_blank())

# Keep facet on plot A (if it has facet)
SDw <- SDw  # Keep as is

# removing period nn panel b and c
# Remove x-axis from plots B and C

SDw <- SDw + theme(
  axis.text.x = element_blank(),      # Remove x-axis text
  axis.ticks.x = element_blank(),     # Remove x-axis ticks  
  axis.title.x = element_blank()      # Remove x-axis title
)
 # for panel b
Seedw <- Seedw + theme(
  axis.text.x = element_blank(),      # Remove x-axis text
  axis.ticks.x = element_blank(),     # Remove x-axis ticks
  axis.title.x = element_blank()      # Remove x-axis title
)

# Keep x-axis on panel c
Sapw <- Sapw  # Keep as is

# Now combine them
multi_panelsIMP <- (SDw / Seedw / Sapw) +   # "/" for stacking vertically
  plot_layout(heights = c(1, 1, 1), guides = "collect") +
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 6, hjust = 0))
  ) &
  theme(
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 5.5),
    plot.tag = element_text(size = 6, hjust = 0),
    legend.position = "top"
  )

##saving using ggsave
ggsave(multi_panelsIMP,filename ="Plots/Multipanel WoodySIMP.png",
       width = 16, height = 14, units = "cm")  

################################################################################
