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

woody_species25 <- read_csv("DATA/March2025/WoodyPC.csv")  

##SEPARATING data by year
woody_species25 <- woody_species25%>%
  filter(Year == 2025)

# Data types
woody_species25 <- woody_species25%>%
  mutate(Site= as.factor(Site),
         Plot= as.factor(Plot),
         Subplot= as.factor(Subplot),
         `Max_height(m)`= as.numeric(`Max_height(m)`)) 

# Summarize height statistics by Site, Plot, and Subplot
height_summary <- woody_species25 %>%
  group_by(Site, Plot) %>%
  summarize(
    Mean_Height = mean(`Max_height(m)`, na.rm = TRUE),
    Median_Height = median(`Max_height(m)`, na.rm = TRUE),
    Min_Height = min(`Max_height(m)`, na.rm = TRUE),
    Max_Height = max(`Max_height(m)`, na.rm = TRUE),
    .groups = "drop"
  )

# View summary
head(height_summary)

# Create the boxplot at Site and Plot level
 ggplot(woody_species25, aes(x = interaction(Site, Plot), y = `Max_height(m)`, fill = Site)) +
  geom_boxplot() +
  labs(
    x = "Site and Plot",
    y = "Woody plants height (m)"
  ) +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8))

 
 ####################### Create the violin plot at site level
 
 # Wheight creates object that can be saved
  ggplot(woody_species25, aes(x = Site, y = `Max_height(m)`)) +
   geom_violin(fill = "lightblue", color = "darkblue") +
   labs(
     x = "Site",
     y = "Woody plants height (m)"
   ) +
   theme_beautiful()
 

  ### Create the violin plot at Site and Plot level
 ggplot(woody_species25, aes(x = interaction(Site, Plot), y = `Max_height(m)`)) +
    geom_violin(fill = "lightblue", color = "darkblue") +
    labs(
      x = "Site and Plot",
      y = "Woody plants height"
    ) +
    theme_beautiful() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8))
 
 
 
 ###############################################################################
 
 ###### COUNTING WOODY SPECIES
 
 
 woody_species25 <- read_csv("DATA/March2025/WoodyPC.csv") 
 # Grouping according to site
 WoodySpp <- woody_species25 %>%
   group_by(Site) %>%
   count(Species_name)
 
 #Number of woody plants species per site
 Woody_species_unique <- woody_species25 %>%
   group_by(Site) %>%
   summarise(n_species = length(unique(Species_name)))
 
 ## PLOTTING  
 ggplot(Woody_species_unique) + 
   geom_col(aes(x = Site, y = n_species), fill = "grey", colour = "grey", width = 0.75) +
   labs(x = "Site", y = "No. of woody species") + theme_beautiful()
 
 # Saving as png
   #ggsave(WSpp,
    #      filename = "C:/workspace/gumbo_dev/Plots/ Woody Species Site.png",
    #     width = 16, height = 14, units = "cm" )
 

 ###### To analyse at site and plot level
 # Count unique woody species at each site
 
 site_summary <- woody_species25 %>%
   group_by(Site) %>%
   summarise(
     species_count = n_distinct(Species_name),
     total_individuals = n()  # Counts the number of rows for each site
   )
 
 # View the result
 print(site_summary)
 
 ### Count unique woody species at each plot within each site
 plot_summary <- woody_species25  %>%
   group_by(Site, Plot) %>%
   summarise(
     species_count = n_distinct(Species_name),
     total_individuals = n ()  # Assuming 'Count' column exists
   )
 
 ### Plot species richness by site
 ggplot(site_summary, aes(x = Site, y = species_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
   labs(title = "Species Richness by Site", x = "Site", y = "Number of Species") +
   theme_beautiful() + theme(legend.position = "right")
 
 
### Plot species richness by plot within each site
ggplot(plot_summary, aes(x = Plot, y = species_count, fill = Site)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Plot", y = "No. of woody species") +
   theme_beautiful() + theme(legend.position = "right")
 
 
 
##### RICHNESS BY SITE AND TREATMENT 
plot_summaryT <- woody_species25  %>%
  group_by(Site, Treatment) %>%
  summarise(
    species_count = n_distinct(Species_name),
    total_individuals = n ()  # Assuming 'Count' column exists
  )
### Plot species richness by TREATMENT within each site
ggplot(plot_summaryT, aes(x = Treatment, y = species_count, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Treatments", y = "No. of woody species") +
  theme_beautiful() + theme(legend.position = "right")



###########################CALCULATING SPECIES DIVERSITY

##calculate species abundance per plot
plot_data2 <- woody_species25 %>%
  group_by(Site, Plot, Species_name) %>%
  summarise(abundance = n(), .groups = 'drop')

## calculate shannon diversity per plot
plot_diversity2 <- plot_data2 %>%
  group_by(Site, Plot) %>%
  summarise(shannon_index = -sum((abundance / sum(abundance)) * log(abundance / sum(abundance))), .groups = 'drop')

# Calculate site-level Shannon diversity (mean across plots)
site_diversity <- plot_diversity2 %>%
  group_by(Site) %>%
  summarise(Site_shannon_index = mean(shannon_index), .groups = 'drop')


# Visualisation site-level diversity

ggplot(site_diversity, aes(x = Site, y = Site_shannon_index)) +
  geom_bar(stat = "identity") +
  theme_beautiful() +
  labs(x = "Site", y = "Diversity index")



##AT PLOT LEVEL
# Plot plot-level Shannon diversity
ggplot(plot_diversity2, aes(x = interaction(Site, Plot), y = shannon_index, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs(x = "Site-Plot", y = "Diversity index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


###################### CALCULATING SIMPSONS DIVERSITY INDEX
# Calculate species abundance per plot
plot_data <- woody_species25 %>%
  group_by(Site, Plot, Species_name) %>% 
  summarise(abundance = n(), .groups = 'drop')

# Calculate Simpson's Index for each plot
plot_diversity_simpson <- plot_data %>%
  group_by(Site, Plot) %>%
  summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')
View(plot_diversity_simpson)

# Calculate site-level Simpson diversity (mean across plots)
site_diversity_simpson <- plot_diversity_simpson %>%
  group_by(Site) %>%
  summarise(site_simpson_index = mean(simpson_index), .groups = 'drop')


# SITE - level Simpson diversity index
ggplot(site_diversity_simpson, aes(x = Site, y = site_simpson_index)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs( y = "Simpson Diversity Index") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

#PLOT -level Simpson diversity index
 ggplot(plot_diversity_simpson, aes(x = interaction(Site, Plot), y = simpson_index, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs(x = "Site - Plot", y = "Simpson Diversity Index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#  +theme(legend.position = "right")

# Saving as png
ggsave(Simp,
       filename = "Plots/ .png",
       width = 16, height = 14, units = "cm" )



###############################################################################
##########################################################################################################
##################################################################################################################

##COMPARING YEAR 2024 AND 2025

woody_species225 <- read_csv("DATA/March2025/WoodyPC.csv")  

WGdata_filtered <- woody_species225  %>%
  filter(Year %in% c(2024, 2025))

WGdata_filtered %>%
  group_by(Year) %>%
  summarise(
    mean_height = mean(`Max_height(m)`, na.rm = TRUE),
    sd_height = sd(`Max_height(m)`, na.rm = TRUE),
    n = n()
  )

##Visualisation
ggplot(WGdata_filtered, aes(x = factor(Year), y = `Max_height(m)`)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Year", y = "Woody plants max. height (m)") +
  theme_beautiful()


# Data types
WGdata_filtered <- woody_species225%>%
  mutate(Site= as.factor(Site),
         Plot= as.factor(Plot),
         Subplot= as.factor(Subplot),
         Year= as.factor(Year),
         `Max_height(m)`= as.numeric(`Max_height(m)`)) 

# Summarize height statistics by Site, Plot, and Subplot
height_summary2 <- WGdata_filtered %>%
  group_by(Year, Site, Plot) %>%
  summarize(
    Mean_Height = mean(`Max_height(m)`, na.rm = TRUE),
    Median_Height = median(`Max_height(m)`, na.rm = TRUE),
    Min_Height = min(`Max_height(m)`, na.rm = TRUE),
    Max_Height = max(`Max_height(m)`, na.rm = TRUE),
    .groups = "drop"
  )

# Create the boxplot at Site and Plot level
ggplot(woody_species225, aes(x = interaction(Year, Site, Plot), y = `Max_height(m)`, fill = Site)) +
  geom_boxplot() +
  labs(
    x = "Site and Plot",
    y = "Woody plants height (m)"
  ) +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8))


########### Create the violin plot at site level

# Wheight creates object that can be saved
ggplot(woody_species225, aes(x = Site, y = `Max_height(m)`)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site",
    y = "Woody plants height (m)"
  ) +
  theme_beautiful()

#########################################################################################################
################################################################################################
########################################

#### CREATING NEW DATASET BY SPILTTING WOODY PLANTS INTO SIZE CLASSES

woody_split25 <- read_csv("DATA/March2025/WoodyPC.csv") 

# Create a new column for size class
woody_split25$Size_Class <- with(woody_split25, 
                               ifelse(`Max_height(m)` >= 0.01 & `Max_height(m)` <= 0.50, "Seedlings",
                                      ifelse(`Max_height(m)` >= 0.51 & `Max_height(m)` <= 1.49, "Saplings",
                                             ifelse(`Max_height(m)` >= 1.5, "Trees", NA))))

### Create separate columns for Seedlings, Saplings, and Trees
woody_split25$Seedlings <- ifelse(woody_split25$Size_Class == "Seedlings", woody_split25$`Max_height(m)`, NA)
woody_split25$Saplings <- ifelse(woody_split25$Size_Class == "Saplings",woody_split25$`Max_height(m)`, NA)
woody_split25$Trees <- ifelse(woody_split25$Size_Class == "Trees", woody_split25$`Max_height(m)`, NA)

### Save the new dataset with separate columns
     write.csv(woody_split25, "woody_with_separate_columns25.csv", row.names = FALSE)

######################################################################################################

################  DETERMINING METRICS FOR TREES
###### TREEE HEIGHT

Tdata <- read_csv("DATA/March2025/woody_with_separate_columns25.csv")

THdata_filtered <- Tdata  %>%
  filter(Year %in% c(2024, 2025)) %>%
filter(!is.na(Trees))%>%  # Keep only rows where Trees are recorded
  group_by(Year, Site, Plot, Subplot) %>%
  summarise(
    Mean_Height = mean(`Max_height(m)`, na.rm = TRUE),   
    Median_Height = median(`Max_height(m)`, na.rm = TRUE),
    Max_Height = max(`Max_height(m)`, na.rm = TRUE),     
    Min_Height = min(`Max_height(m)`, na.rm = TRUE),     
    Count = n(), 
    .groups = "drop"  )

#### Create a formatted table
Tt <- flextable(THdata_filtered) %>%
  theme_vanilla() %>%  # Apply a simple theme
  autofit()            # Auto-adjust column widths

# Create a Word document and add the table
doc <- read_docx() %>%
  body_add_par("Table 1: Tree Species Count by Site, Plot, and Subplot", style = "heading 2") %>%
  body_add_flextable(Tt)

## Save the document 
  #print(doc, target = "Plots/Trees count.docx")


###Plotting against site
ggplot(THdata_filtered, aes(x = interaction(Site), y = Max_Height)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site and Plot",
    y = "Tree height (m)"
  ) +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8))

### combined number of trees per site
ggplot(THdata_filtered, aes(x = interaction(Site), y = Count)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site",
    y = "No. of trees per site"
  ) +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8))

#########################################################################################################

## LOAD DATA
Tdata <- read_csv ("DATA/March2025/woody_with_separate_columns25.csv")


# Clean and format, FILTERING NAs

Tdata <- Tdata %>%
  filter(!is.na(`Max_height(m)`)) %>%  # Exclude NAs in dpm_height
  mutate(
    Year = as.factor(Year),
    Site = as.factor(Site),
    Plot = as.factor(Plot),
    Subplot = as.factor(Subplot),
    Treatment = as.factor(Treatment),
    `Max_height(m)` = as.numeric(`Max_height(m)`) # Ensure numeric
  )

# Check missing values
summary(Tdata$`Max_height(m)`)

###2. Create Pre/Post Variable

Tdata <- Tdata %>%
  mutate(period = ifelse(as.numeric(as.character(Year)) < 2025, "PRE", "POST")) %>%
  mutate(period = factor(period, levels = c("PRE", "POST")))

### Summarize by Subplot or Plot..Average height per subplot and period:

summary_Hdata <- Tdata %>%
  group_by(Site, Plot, Subplot, Treatment, period) %>%
  summarise(mean_height = mean(`Max_height(m)`, na.rm = TRUE)) %>%
  ungroup()

####4. Calculate Change in Height (Post - Pre)
height_change <- summary_Hdata %>%
  pivot_wider(names_from = period, values_from = mean_height) %>%
  mutate(delta = POST - PRE)

### Compare Treatment Effects (ANOVA on Delta)
Tanova_model <- aov(delta ~ Treatment, data = height_change)
summary(Tanova_model)
# the results show that the treatment groups do not differ significantly in their effect on the response variable.

## Optional: Tukey post-hoc test
TukeyHSD(Tanova_model)

#### USE OF Mixed-Effects Model (Handles Repeated Measures)
#This is more robust as it uses the original data, accounts for plot/subplot as random effects, and tests interaction between time and treatment:

Tlmm <- lmer(`Max_height(m)` ~ period * Treatment + (1 | Site/Plot/Subplot), data = Tdata)
summary(Tlmm)

#Tlmm2 <- lmer(`Max_height(m)` ~ period * Treatment + (1 | Site), data = Tdata)
#summary(Tlmm2)
# Look at the interaction terms (periodPOST:treatmentX) to assess whether any treatment caused a significant change post-treatment.

##Plotting boxplot for before and after treatment
ggplot(Tdata[!is.na(Tdata$`Max_height(m)`), ],
       aes(x = Treatment, y = `Max_height(m)`, fill = period)) +
  geom_boxplot() +
  theme_beautiful()

# ### Violin plot for  height
### Violin plot
 ggplot(Tdata[!is.na(Tdata$`Max_height(m)`), ],
       aes(x = Treatment, y = `Max_height(m)`, fill = period)) +
  geom_violin(trim = FALSE) +
  labs(y = "Max. height (m)", x = "Treatments") +
  theme_beautiful() 

#ggsave(HF,filename ="Plots/Violin Height WoodyPlants.png",
 #     width = 16, height = 14, units = "cm" )
############################################################################################
#####################################################
 
 #SEEDLINGS
 # Clean and format, FILTERING NAs
 
 Tdata <- Tdata %>%
   filter(!is.na(Seedlings) %>%  # Exclude NAs in dpm_height
   mutate(
     Year = as.factor(Year),
     Site = as.factor(Site),
     Plot = as.factor(Plot),
     Subplot = as.factor(Subplot),
     Treatment = as.factor(Treatment),
     Seedlings = as.numeric(Seedlings) # Ensure numeric
   )
 
 # Check missing values
 summary(Tdata$Seedlings)
 
 ###2. Create Pre/Post Variable
 
 Tdata <- Tdata %>%
   mutate(period = ifelse(as.numeric(as.character(Year)) < 2025, "PRE", "POST")) %>%
   mutate(period = factor(period, levels = c("PRE", "POST")))
 
 ### Summarize by Subplot or Plot..Average height per subplot and period:
 
 summary_Hdata <- Tdata %>%
   group_by(Site, Plot, Subplot, Treatment, period) %>%
   summarise(mean_height = mean(Seedlings, na.rm = TRUE)) %>%
   ungroup()
 
 ####4. Calculate Change in Height (Post - Pre)
 height_change <- summary_Hdata %>%
   pivot_wider(names_from = period, values_from = mean_height) %>%
   mutate(delta = POST - PRE)
 
 ### Compare Treatment Effects (ANOVA on Delta)
 Sanova_model <- aov(delta ~ Treatment, data = height_change)
 summary(Sanova_model)
 # the results show that the treatment groups differ significantly in their effect on the response variable.
 
 ## Optional: Tukey post-hoc test
 TukeyHSD(Sanova_model)
 
 #### USE OF Mixed-Effects Model (Handles Repeated Measures)
 #This is more robust as it uses the original data, accounts for plot/subplot as random effects, and tests interaction between time and treatment:
 
 Slmm <- lmer(Seedlings ~ period * Treatment + (1 | Site/Plot/Subplot), data = Tdata)
 summary(Slmm)
 
 #Tlmm2 <- lmer(`Max_height(m)` ~ period * Treatment + (1 | Site), data = Tdata)
 #summary(Tlmm2)
 # Look at the interaction terms (periodPOST:treatmentX) to assess whether any treatment caused a significant change post-treatment.
 
 ##Plotting boxplot for seedlings before and after treatment
 SD <- ggplot(Tdata[!is.na(Tdata$Seedlings), ],
        aes(x = Treatment, y = Seedlings, fill = period)) +
   geom_boxplot() +
   theme_beautiful()
 
 #ggsave(SD,filename ="Plots/Height Seedlings Boxplot.png",
  #          width = 16, height = 14, units = "cm")
 
 # ### Violin plot for  height
 ### Violin plot
 SDV <-ggplot(Tdata[!is.na(Tdata$Seedlings), ],
        aes(x = Treatment, y = Seedlings, fill = period)) +
   geom_violin(trim = FALSE) +
   labs(y = "Seedlings max.height (m)", x = "Treatments") +
   theme_beautiful() 
 
 #ggsave(SDV,filename ="Plots/Height Seedlings Violin.png",
  #      width = 16, height = 14, units = "cm")
 
 
 ####################################################################################################
 ####################################################################################################
 
 ### SAPLINGS SAPLINGS SAPLINGS SAPLINGS SAPLINGS
 
 Tdata <- Tdata %>%
   filter(!is.na(Saplings) %>%  # Exclude NAs in dpm_height
            mutate(
              Year = as.factor(Year),
              Site = as.factor(Site),
              Plot = as.factor(Plot),
              Subplot = as.factor(Subplot),
              Treatment = as.factor(Treatment),
              Saplings = as.numeric(Saplings) # Ensure numeric
            )
          
  # Check missing values
    summary(Tdata$Saplings)
          
  ###2. Create Pre/Post Variable
          
Tdata <- Tdata %>%
mutate(period = ifelse(as.numeric(as.character(Year)) < 2025, "PRE", "POST")) %>%
mutate(period = factor(period, levels = c("PRE", "POST")))
          
### Summarize by Subplot or Plot..Average height per subplot and period:
          
 summary_Hdata <- Tdata %>%
 group_by(Site, Plot, Subplot, Treatment, period) %>%
summarise(mean_height = mean(Saplings, na.rm = TRUE)) %>%
 ungroup()
          
  ####4. Calculate Change in Height (Post - Pre)
  height_change <- summary_Hdata %>%
  pivot_wider(names_from = period, values_from = mean_height) %>%
 mutate(delta = POST - PRE)
          
 ### Compare Treatment Effects (ANOVA on Delta)
Spanova_model <- aov(delta ~ Treatment, data = height_change)
summary(Spanova_model)
 # the results show that treatment has a statistically significant effect on the response variable (saplings).
          
 ## Optional: Tukey post-hoc test
 TukeyHSD(Spanova_model)
          
#### USE OF Mixed-Effects Model (Handles Repeated Measures)
 #This is more robust as it uses the original data, accounts for plot/subplot as random effects, and tests interaction between time and treatment:
          
 Splmm <- lmer(Saplings ~ period * Treatment + (1 | Site/Plot/Subplot), data = Tdata)
 summary(Splmm)
          
          #Tlmm2 <- lmer(`Max_height(m)` ~ period * Treatment + (1 | Site), data = Tdata)
          #summary(Tlmm2)
          # Look at the interaction terms (periodPOST:treatmentX) to assess whether any treatment caused a significant change post-treatment.
          
 ##Plotting boxplot for seedlings before and after treatment
SP <- ggplot(Tdata[!is.na(Tdata$Saplings),],
aes(x = Treatment, y = Saplings, fill = period)) +
 geom_boxplot() +
 theme_beautiful()
          
  ggsave(SP,filename ="Plots/Saplings Height Boxplot.png",
   width = 16, height = 14, units = "cm")
          
 # ### Violin plot for  height
 ### Violin plot
  SPV <-ggplot(Tdata[!is.na(Tdata$Saplings), ],
 aes(x = Treatment, y = Saplings, fill = period)) +
   geom_violin(trim = FALSE) +
   labs(y = "Saplings max.height (m)", x = "Treatments") +
    theme_beautiful() 
          
   #ggsave(SPV,filename ="Plots/Height Saplings Violin.png",
    # width = 16, height = 14, units = "cm")
          