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
 
  #### TREES
 # Clean and format, FILTERING NAs
 
 Tdata <- Tdata %>%
   filter(!is.na(Trees) %>%  # Exclude NAs in dpm_height
            mutate(
              Year = as.factor(Year),
              Site = as.factor(Site),
              Plot = as.factor(Plot),
              Subplot = as.factor(Subplot),
              Treatment = as.factor(Treatment),
              Seedlings = as.numeric(Trees) # Ensure numeric
            )
          
# Check missing values
 summary(Tdata$Trees)
          
  ###2. Create Pre/Post Variable
  Tdata <- Tdata %>%
            mutate(period = ifelse(as.numeric(as.character(Year)) < 2025, "PRE", "POST")) %>%
            mutate(period = factor(period, levels = c("PRE", "POST")))
          
          ### Summarize by Subplot or Plot..Average height per subplot and period:
          
          summary_Trdata <- Tdata %>%
            group_by(Site, Plot, Subplot, Treatment, period) %>%
            summarise(mean_height = mean(Trees, na.rm = TRUE)) %>%
            ungroup()
          
  ####4. Calculate Change in Height (Post - Pre)
 Theight_change <- summary_Trdata %>%
 pivot_wider(names_from = period, values_from = mean_height) %>%
  mutate(delta = POST - PRE)
 
 ### Compare Treatment Effects (ANOVA on Delta)
 Tanova_model <- aov(delta ~ Treatment, data = Theight_change)
 summary(Tanova_model)
 # the results show that the treatment groups do not differ significantly in their effect on the response variable.
 
 ## Optional: Tukey post-hoc test
 TukeyHSD(Tanova_model)
 
 #### USE OF Mixed-Effects Model (Handles Repeated Measures)
 #This is more robust as it uses the original data, accounts for plot/subplot as random effects, and tests interaction between time and treatment:
 
 Tlmm <- lmer(Trees ~ period * Treatment + (1 | Site/Plot/Subplot), data = Tdata)
 summary(Tlmm)
 
 ##Plotting boxplot for seedlings before and after treatment
TD <- ggplot(Tdata[!is.na(Tdata$Trees), ],
              aes(x = Treatment, y = Trees, fill = period)) +
   geom_boxplot() +
  labs(x = "Treatments", y = "Trees max.height (m)") +
   theme_beautiful()
 
 #ggsave(TD,filename ="Plots/Height TREES Boxplot.png",
  #         width = 16, height = 14, units = "cm")
 

 # ### Violin plot for  height
 ### Violin plot
TDV <-ggplot(Tdata[!is.na(Tdata$Trees), ],
              aes(x = Treatment, y = Trees, fill = period)) +
   geom_violin(trim = FALSE) +
   labs(y = "Trees max.height (m)", x = "Treatments") +
   theme_beautiful() 

#saving violin plot
 #ggsave(TDV,filename ="Plots/Height TREES Violinplot.png",
  #     width = 16, height = 14, units = "cm")

 
  ################################# SEEDLINGS SEEDLINGS SEEDLINGS
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
 
 #  SAPLINGS Height 
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
          
 ##Plotting boxplot for saplings before and after treatment
SP <- ggplot(Tdata[!is.na(Tdata$Saplings),],
aes(x = Treatment, y = Saplings, fill = period)) +
 geom_boxplot() +
 theme_beautiful()
          
 # ggsave(SP,filename ="Plots/Saplings Height Boxplot.png",
  # width = 16, height = 14, units = "cm")
          
 # ### Violin plot for  height
 ### Violin plot
  SPV <-ggplot(Tdata[!is.na(Tdata$Saplings), ],
 aes(x = Treatment, y = Saplings, fill = period)) +
   geom_violin(trim = FALSE) +
   labs(y = "Saplings max.height (m)", x = "Treatments") +
    theme_beautiful() 
          
   #ggsave(SPV,filename ="Plots/Height Saplings Violin.png",
    # width = 16, height = 14, units = "cm")
#############################################################################################
  
  ##Saplings population i.e saplings count per treatment 
 # Tdata <- Tdata %>%
    # Data prep 
    saplings_clean <- Tdata %>%
    filter(!is.na(Saplings)) %>%
    mutate(
      Year = as.factor(Year),
      Site = as.factor(Site),
      Plot = as.factor(Plot),        # Fixed from Pot
      Subplot = as.factor(Subplot),  # Fixed from Slot
      Treatment = as.factor(Treatment),  # Fixed from Ttt
      Saplings = as.numeric(Saplings),
      period = ifelse(as.numeric(as.character(Year)) < 2025, "PRE", "POST"),
      period = factor(period, levels = c("PRE", "POST"))
    )

## Calculate Delta (POST - PRE) for each Subplot within each Treatment using total saplings
  sapling_delta <- saplings_clean %>%
    group_by(Site, Plot, Subplot, Treatment, period) %>%
    summarise(total_saplings = sum(Saplings, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = period, values_from = total_saplings) %>%
    mutate(Delta = POST - PRE)
  
  # Plot Delta per Treatment
  SPP <- ggplot(sapling_delta, aes(x = Treatment, y = Delta, fill = Treatment)) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      x = "Treatment",
      y = expression(Delta~"Saplings")
    ) +
    theme_beautiful() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1))
  
  
  # ggsave(SPP,filename ="Plots/Saplings Delta Total NO.Boxplot.png",
        # width = 16, height = 14, units = "cm")
  
    

## Include random effect for Site/Plot/Subplot if repeated
Sp_model <- lmer(Saplings ~ Treatment * period + (1|Site/Plot/Subplot), data = Tdata)
summary(Sp_model)



#########################################################################################################
  ######################################################################################
  
# COMPARING SEEDLINGS richness, diversity
  
  ## LOAD DATA
  Sdata <- read_csv ("DATA/March2025/woody_with_separate_columns25.csv")
  

  # Make sure Year and Treatment are factors
  Sdata$Year <- as.factor(Sdata$Year)
  Sdata$Treatment <- as.factor(Sdata$Treatment)
  
  # Summarise species richness per Subplot
  richness_data <- Sdata %>%
    group_by(Year, Treatment, Site, Plot, Subplot) %>%
    summarise(richness = n_distinct(Species_name), .groups = "drop")

  #If Subplot is nested in Plot, include random effects:
  glmm_model <- glmer(
    richness ~ Year * Treatment + (1 | Site/Plot/Subplot),
    data = richness_data,
    family = poisson
  )
  summary(glmm_model)
  
  #Visualisation using boxplot
 Se <- ggplot(richness_data, aes(x = Treatment, y = richness, fill = Year)) +
    geom_boxplot() +
    labs(
         y = "No. of species") +
    theme_beautiful()
  
  #ggsave(Se,filename ="Plots/Boxplot SEEDLINGS species richness.png",
              #width = 16, height = 14, units = "cm")
 
  ## Visualistaion using VIOLIN plot
  
   ggplot(richness_data, aes(x = Treatment, y = richness, fill = Year)) +
    geom_violin(trim = FALSE, scale = "width", alpha = 0.7)  +
    labs(
      x = "Treatment",
      y = "No. of species"
    ) +
    theme_beautiful() + 
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0),
      plot.title = element_text(face = "bold")
    )
  #ggsave(Sv,filename ="Plots/Violin SEEDLINGS species richness.png",
                     # width = 16, height = 14, units = "cm")
  
  
#Check model assumptions
  # Check for overdispersion
  overdisp_fun <- function(model) {
    rdf <- df.residual(model)
    rp <- residuals(model, type = "pearson")
    sum(rp^2) / rdf
  }
  overdisp_fun(glmm_model)  # ~1 is good; >>1 = overdispersion
  

##Post-hoc pairwise comparisons to examine which Treatment-Year combinations differ:
  # Estimated marginal means
  emm <- emmeans(glmm_model, ~ Year | Treatment)
  pairs(emm)
  
  

  ###################### SAPLINGS SPECIES COMPOSITION PER TREATMENT

  ## LOAD DATA
  Spdata <- read_csv ("DATA/March2025/woody_with_separate_columns25.csv")
  
  # 1. Prepare data: richness per Site x Treatment x Year
  saplings_year <- Spdata %>%
    filter(!is.na(Saplings),
           Year %in% c(2024, 2025)) %>%   # keep only pre/post years
    group_by(Site, Plot, Subplot, Treatment, Year) %>%
    summarise(spp_richness = n_distinct(Species_name), .groups = "drop")
  
  # Quick look
  summary(saplings_year)
  
  # 2. Fit GLMM ----
  # Start with Poisson; switch to negative binomial if overdispersed
  m_pois <- glmmTMB(spp_richness ~ Year * Treatment + (1|Site/Plot/Subplot),
                    data = saplings_year,
                    family = poisson)
    
  # Check dispersion
  performance::check_overdispersion(m_pois)  #no overdispersion 
  
  
  # If overdispersed, refit with negative binomial:
     # m_nb <- update(m_pois, family = nbinom2)
  
  # Compare AIC
     #AIC(m_pois, m_nb)
  
  # Choose the best model (say m_nb) and inspect summary
     #  summary(m_nb)
  

  # 3. Post‑hoc comparisons ----
  # Estimated marginal means for Year within each Treatment
  emm <- emmeans(m_pois, ~ Year | Treatment, type = "response")
  pairs(emm)
  
  # 4. Plot results # Convert Year to factor so axis shows integers without decimals
  emm <- as.data.frame(emm) %>%
    mutate(Year = factor(Year))

  
  #  custom ggplot:
  ggplot(as.data.frame(emm), aes(Year, rate, group = Treatment, colour = Treatment)) +
    geom_line() +
    geom_point(size = 3) +
    labs(y = "Sapling species richness") +
    theme_beautiful()
  
  ### 4. Visualisation: Violin plot of observed sapling richness by Year within each Treatment
   saplings_year %>%
    mutate(Year = factor(Year)) %>%
    ggplot(aes(x = Year, y = spp_richness, fill = Year)) +
    geom_violin(trim = FALSE, alpha = 0.6, colour = NA) +
    geom_jitter(width = 0.1, height = 0, size = 1, alpha = 0.7) +
    stat_summary(fun = median, geom = "point", size = 3, colour = "black") +
    facet_wrap(~ Treatment) +
    labs(x = "Year", y = "Sapling species richness") +
    theme_beautiful() +
    theme(legend.position = "none")

  #ggsave(Spviolin,filename ="Plots/Violin Saplings species richness.png",
   #width = 16, height = 14, units = "cm")

#################################################################   
  ###  Δ‑change (2025 – 2024) analysis --------------------------------------
   # Pivot wider to compute site‑level change
   saplings_delta <- saplings_year %>%
     pivot_wider(names_from = Year, values_from = spp_richness, names_prefix = "Y") %>%
     mutate(delta = Y2025 - Y2024)
   
   # Fit linear mixed model for delta (can go negative, so Gaussian assumption)
   mod_delta <- lmer(delta ~ Treatment + (1|Site), data = saplings_delta)
   summary(mod_delta)
   
   # Post‑hoc comparisons on delta
   emm_delta <- emmeans(mod_delta, ~ Treatment)
   contrast(emm_delta, method = "pairwise")
   
   # 5. Box plot of Δ‑change --------------------------------------------------
   SpDELTA <- ggplot(saplings_delta, aes(x = Treatment, y = delta, fill = Treatment)) +
     geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
     #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
     geom_hline(yintercept = 0, linetype = "dashed") +
     labs(x = "Treatment", y = "Δ sapling richness (2025 – 2024)") +
     theme_beautiful() +
     theme(legend.position = "none")
   
   ggsave(SpDELTA,filename ="Plots/Delta Saplings species richness Box plot.png",
   width = 16, height = 14, units = "cm")
   
   
##############################################################################################
   ################## SPECIES RICHNESS    TREES  TREES TREES TREES TREES
   
   ## LOAD DATA
   TRdata <- read_csv ("DATA/March2025/woody_with_separate_columns25.csv")
   
   # 1. Prepare data: richness per Site x Treatment x Year
   trees_year <- TRdata %>%
     filter(!is.na(Trees),
            Year %in% c(2024, 2025)) %>%   # keep only pre/post years
     group_by(Site, Plot, Subplot, Treatment, Year) %>%
     summarise(spp_richness = n_distinct(Species_name), .groups = "drop")
   
   # Quick look
   summary(trees_year)
   
   # 2. Fit GLMM ----
   # Start with poisson and then fit gaussian
   t_pois <- glmmTMB(spp_richness ~ Treatment * Year + (1|Site/Plot/Subplot),
                     data = trees_year)  # no family fitted
   summary(t_pois)
   
 #t_pois2 <- glmmTMB(spp_richness ~ Treatment * Year + (1|Site/Plot/Subplot),
  #                 data = trees_year, 
   #                 family = gaussian)   #gaussian family
   #summary(t_pois2)
   
   # Check dispersion
      # performance::check_overdispersion(t_pois2)  #no overdispersion 
   
   # 3. Post‑hoc comparisons ----  # Estimated marginal means for Year within each Treatment
  temm <- emmeans(m_pois, ~ Year | Treatment, type = "response")
  pairs(temm)
  
  # 4. Plot results # Convert Year to factor so axis shows integers without decimals
   temm <- as.data.frame(temm) %>%
     mutate(Year = factor(Year))
      
   #  custom ggplot:
   ggplot(as.data.frame(temm), aes(Year, rate, group = Treatment, colour = Treatment)) +
     geom_line() +
     geom_point(size = 3) +
     labs(y = "Trees species richness") +
     theme_beautiful()
   
   ###  Visualisation: Violin plot of observed Tree richness by Year within each Treatment
   trees_year %>%
     mutate(Year = factor(Year)) %>%
     ggplot(aes(x = Year, y = spp_richness, fill = Year)) +
     geom_violin(trim = FALSE, alpha = 0.6, colour = NA) +
    # geom_jitter(width = 0.1, height = 0, size = 1, alpha = 0.7) +
     stat_summary(fun = median, geom = "point", size = 2, colour = "black") +
     facet_wrap(~ Treatment) +
     labs(x = "Year", y = "Trees species richness") +
     theme_beautiful() +
     theme(legend.position = "none")
    
   
  ###  Δ‑change (2025 – 2024) analysis --------------------------------------
   # Pivot wider to compute site‑level change
   trees_delta <- trees_year %>%
     pivot_wider(names_from = Year, values_from = spp_richness, names_prefix = "Y") %>%
     mutate(delta = Y2025 - Y2024)
   
   # Fit linear mixed model for delta (can go negative, so Gaussian assumption)
   tmod_delta <- lmer(delta ~ Treatment + (1|Site), data = trees_delta)
   summary(tmod_delta)
   
   
   # Post‑hoc comparisons on delta
   emm_delta <- emmeans(tmod_delta, ~ Treatment)
   contrast(emm_delta, method = "pairwise")
   
   # 5. Box plot of Δ‑change --------------------------------------------------
   TrDELTA <- ggplot(trees_delta, aes(x = Treatment, y = delta, fill = Treatment)) +
     geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
     #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
     geom_hline(yintercept = 0, linetype = "dashed") +
     labs(x = "Treatment", y = "Δ Trees spp richness") +
     theme_beautiful() +
     theme(legend.position = "none")
   
   ggsave(TrDELTA,filename ="Plots/Delta Trees species richness Box plot.png",
          width = 16, height = 14, units = "cm")
   
###########################################################################################################
  ##################################################################################################
  
# DETERMINING NO. OF RESPROUTS ON CUT STUMPS
  
 Resdata <- read_csv("DATA/March2025/WoodyPC.csv")
  
  colnames(Resdata)

  # Convert NA to 0
  Resdata <- Resdata %>%
    mutate(No_of_resprouts = ifelse(is.na(No_of_resprouts), 0, No_of_resprouts))
  
# Filtrer data to only include year 2025
  Resdata_2025 <- Resdata %>%
    filter( Treatment %in% c("THF", "TF", "TFB"),Year == "2025", Woody_class == "Cut stump")
  
# Visualising comparisons
  ResP<- ggplot(Resdata_2025 , aes(x = Treatment, y = No_of_resprouts, fill = Treatment)) +
    geom_violin(trim = FALSE) +
    scale_fill_manual(values = c("yellow", "brown","green")) +
    labs(x = "Treatments", y = "No. of resprouts") +
    theme_beautiful()
  
  ggsave(ResP,filename ="Plots/Violin Resprouts Cut Stumps.png",
   width = 16, height = 14, units = "cm")
  
  
  #If Subplot is nested in Plot, include random effects:
  Rglmm_model <- glmer(
    No_of_resprouts ~  Treatment + (1 | Site/Plot/Subplot),
    data = Resdata_2025,
    family = poisson
  )
  summary(Rglmm_model)
  
  #Check model assumptions
  # Check for overdispersion
  overdisp_fun <- function(model) {
    rdf <- df.residual(model)
    rp <- residuals(model, type = "pearson")
    sum(rp^2) / rdf
  }
  #check for dispersion
  overdisp_fun(Rglmm_model)  # ~1 is good; >>1 = overdispersion
  
  # switch to Negative binomial GLMM since there is overdispersion
  Rglmm_nb <- glmmTMB(
    No_of_resprouts ~  Treatment + (1 | Site/Plot/Subplot),
    data = Resdata_2025,
    family = nbinom2
  )
  
  # recheck dispersion
  overdisp_fun(Rglmm_nb)    # dispersion value is 1.2 
  
  #Check summary model
  summary(Rglmm_nb)
  
#Post hoc comparisons (if Treatment has multiple levels):
  emmeans(Rglmm_nb, pairwise ~ Treatment)  
 
# Plot model predictions or residuals:
  plot(residuals(Rglmm_nb))
  
# Visualize fitted values vs. observed:
  plot(fitted(Rglmm_nb), Resdata_2025$No_of_resprouts)
  