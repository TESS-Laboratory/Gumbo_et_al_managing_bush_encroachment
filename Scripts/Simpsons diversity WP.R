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

#Filter SEEDLINGS  
SeedlingsSD <- SapF %>% 
  filter(woody_cat == "Seedlings", Year %in% c(2024, 2025),
         !Treatment %in% c("TFB")) %>% 
  group_by(Treatment, Fencing,Year, Species_name, name = "Seedlings") %>% 
  summarise(abundance = n(), .groups = 'drop')


#FOR all woody plants
AllSD <- SapF %>% 
  filter( Year %in% c(2024, 2025),
         !Treatment %in% c("TFB")) %>% 
  group_by(Treatment, Fencing,Year, Species_name) %>% 
  summarise(abundance = n(), .groups = 'drop')


## Calculate species abundance per plot

# Calculate seedlings Simpson's Index for each plot 
ASimp_diversity_simpson <- AllSD  %>%
  group_by(Treatment, Fencing, Year) %>% 
  summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')

#sort pre and post treatment
AsD_comparison <- ASimp_diversity_simpson %>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))



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
AsD_comparison3 <- sD_comparison3 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))




# Create point plot with facets for woody plants
ASIMP <- ggplot(AsD_comparison3, aes(x = Treatment, y = simpson_index, 
                           color = as.factor(Period), shape = as.factor(Period))) +
  geom_point(size = 2, position = position_dodge(0.3)) +
  facet_wrap(~Fencing, ncol = 1) +
  labs(x = "Treatment", 
       y = "Woody plants Simpson's diversity index", 
       color = "Period",
       shape = "Period") +
  theme_classic() +
  scale_color_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))


# Saving as png
ggsave(ASIMP,
       filename = "Plots/Woodyplant SimpDiversity.png",
       width = 16, height = 14, units = "cm" )



################### SITE-level analysis

AllSD2 <- SapF %>% 
  filter( Year %in% c(2024, 2025),
          !Treatment %in% c("TFB")) %>% 
  group_by(Site,Year, Species_name) %>% 
  summarise(abundance = n(), .groups = 'drop')


## simpsons  
A2Simp_diversity_simpson <- AllSD2  %>%
  group_by(Site, Year) %>% 
  summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')




#create vector
AllSD3 <- SapF %>% 
  filter( Year %in% c(2024, 2025),!is.na(Species_name)) %>%  
  group_by(Site, Plot, Subplot, Fencing, Treatment, Year, Species_name) %>% 
  summarise(abundance = n())%>%
  summarise(
    Simpson_index1 = 1 - sum((abundance / sum(abundance))^2),
    .groups = "drop")

##Visualisation; Boxplot for 1-D  
tsr <- ggplot(AllSD3, aes(x = Site, y = Simpson_index1, fill = Fencing)) + facet_wrap(~Fencing) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Site", y = "Simpsons index of diversity (1-D)") +
  theme_beautiful() +
  theme(legend.position = "none")


#sort pre and post treatment at site level
AsD_comparison2 <- A2Simp_diversity_simpson %>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


# Calculate Simpson's Index for woody plants at site level
A2Sisimpson <- AllSD2 %>%
  filter(Year %in% c(2024, 2025)) %>%
  group_by(Site, Year) %>%
  summarise(
    total_individuals = sum(abundance, na.rm = TRUE),  
    sum_n_squared = sum(abundance * (abundance - 1), na.rm = TRUE),
    simpson_index = 1 - (sum_n_squared / (total_individuals * (total_individuals - 1))),
    .groups = 'drop'
  )



#sort pre and post treatment for woody plants
A2sD_comparison3 <- A2Sisimpson %>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


#reorder so that pre-treatment appears first then post treatment second on the plots
A2sD_comparison3 <- A2sD_comparison3 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))


# Create point plot with facets for woody plants
SIMPsite <- ggplot(A2sD_comparison3, aes(x = Site, y = simpson_index, 
                                     color = as.factor(Period), shape = as.factor(Period))) +
  geom_point(size = 2, position = position_dodge(0.3)) +
 # facet_wrap(~Fencing, ncol = 1) +
  labs(x = "Site", 
       y = "Woody plants Simpson's diversity index", 
       color = "Period",
       shape = "Period") +
  theme_classic() +
  scale_color_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))


##### bar plot
ggplot(A2sD_comparison3, aes(x = Site, y = simpson_index, 
                             color = as.factor(Period), shape = as.factor(Period))) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +  # facet_wrap(~Fencing, ncol = 1) +
  labs(x = "Site", 
       y = "Woody plants Simpson's diversity index", 
       color = "Period",
       shape = "Period") +
  theme_classic() +
  scale_color_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))


##### violin plot

#ggplot(strt_comparison2, 
 #      aes(x = Treatment, y = density_ha, fill = Period)) + facet_wrap(~Fencing)+ 
  
gplot(A2sD_comparison3, aes(x = Site, y = simpson_index, 
                            color = as.factor(Period), shape = as.factor(Period))) +
geom_violin(trim = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed") +    
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  labs(x = "Treatment", 
       y = "Seedlings density per ha",
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8)        # Axis tick labels
  )+
  scale_fill_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))

# Saving as png
ggsave(SIMPsite,
       filename = "Plots/Site SimpDiversity.png",
       width = 16, height = 14, units = "cm" )






#####
############         CALCULATING SIMPSONS DIVERSITY INDEX FOR SEEDLINGS

# Calculate species abundance per plot

# Calculate seedlings Simpson's Index for each plot 
Simp_diversity_simpson <- SeedlingsSD  %>%
  group_by(Treatment, Fencing, Year) %>% 
  summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')

#sort pre and post treatment
sD_comparison2 <- Simp_diversity_simpson %>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


#reorder so that pre-treatment appears first then post treatment second on the plots
sD_comparison2 <- sD_comparison2 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))



ggplot(sD_comparison2, aes(x = Treatment, y = simpson_index, fill = Period)) + facet_wrap(~Fencing)+
  geom_violin(trim = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +    
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  labs(x = "Treatment", y = "Simpsons diversity for seedlings") +
  theme_classic()


table(sD_comparison2$Treatment, sD_comparison2$Fencing)




sample_sizes <- SeedlingsSD %>%
  group_by(Treatment, Fencing, Year) %>%
  summarise(n_plots = n_distinct(Treatment), .groups = 'drop')
print(sample_sizes)


# Calculate Simpson's Index
Sisimpson <- SeedlingsSD %>%
  filter(Year %in% c(2024, 2025)) %>%
  group_by(Treatment, Fencing, Year) %>%
  summarise(
    total_individuals = sum(abundance, na.rm = TRUE),  # Changed from Count
    sum_n_squared = sum(abundance * (abundance - 1), na.rm = TRUE),
    simpson_index = 1 - (sum_n_squared / (total_individuals * (total_individuals - 1))),
    .groups = 'drop'
  )

#sort pre and post treatment
sD_comparison3 <- Sisimpson %>%
  filter(Year %in% c(2024, 2025)) %>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


#reorder so that pre-treatment appears first then post treatment second on the plots
sD_comparison3 <- sD_comparison3 %>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))




# Create point plot with facets
ggplot(sD_comparison3, aes(x = Treatment, y = simpson_index, 
                      color = as.factor(Year), shape = as.factor(Year))) +
  geom_point(size = 4, position = position_dodge(0.3)) +
  facet_wrap(~Fencing, ncol = 1) +
  labs(x = "Treatment", 
       y = "Simpson's Diversity Index", 
       color = "Year",
       shape = "Year") +
  theme_classic() +
  scale_color_manual(values = c("2024" = "#1b7837", "2025" = "#a6dba0"))


ggplot(sD_comparison3, aes(x = Treatment, y = simpson_index, 
                           color = as.factor(Period), shape = as.factor(Period))) +
  geom_point(size = 4, position = position_dodge(0.3)) +
  facet_wrap(~Fencing, ncol = 1) +
  labs(x = "Treatment", 
       y = "Simpson's Diversity Index", 
       color = "Period",
       shape = "Period") +
  theme_classic() +
  scale_color_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))


# Saving as png
ggsave(SsIMP,
       filename = "C:/workspace/gumbo_dev/Plots/ SEEDLINGSIMP Diversity Index SITE.png",
       width = 16, height = 14, units = "cm" )
