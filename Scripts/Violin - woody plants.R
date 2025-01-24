
library(tidyverse)
library(vegan)
library(multcompView)

woody_species <- read.csv("C:/workspace/gumbo_dev/DATA/WoodyPlants_C.csv")

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


# Data types
woody_species <- woody_species%>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot),
         Subplot = as.factor(Subplot),
         Height = as.numeric(Max_Height))

# Summarize height statistics by Site, Plot, and Subplot
height_summary <- woody_species %>%
  group_by(Site, Plot) %>%
  summarize(
    Mean_Height = mean(Max_Height, na.rm = TRUE),
    Median_Height = median(Max_Height, na.rm = TRUE),
    Min_Height = min(Max_Height, na.rm = TRUE),
    Max_Height = max(Max_Height, na.rm = TRUE),
    .groups = "drop"
  )

# View summary
head(height_summary)
tail (height_summary)


# Create the boxplot at Site and Plot level
WHeight <- ggplot(woody_species, aes(x = interaction(Site, Plot), y = Max_Height, fill = Site)) +
  geom_boxplot() +
  labs(
    x = "Site and Plot",
    y = "Woody plants height"
  ) +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8))

# Saving as png
ggsave(WHeight,
       filename = "C:/workspace/gumbo_dev/Plots/Boxplot - W Height SP.png",
       width = 16, height = 14, units = "cm" )

####################### Create the violin plot at site level

# Wheight creates object that can be saved
Wheight <- ggplot(woody_species, aes(x = Site, y = Max_Height)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site",
    y = "Woody plants height"
  ) +
  theme_beautiful()
  

# Save plot object

#ggsave(Wheight_site_level,
#       filename = "/Plots/Violin.png",
 #      width = 16, height = 14, units = "cm")

ggsave(Wheight,
       filename = "C:/workspace/gumbo_dev/Plots/Violin-Woody height.png",
       width = 16, height = 14, units = "cm" )



# Create the violin plot at Site and Plot level
Wheight_SP <- ggplot(woody_species, aes(x = interaction(Site, Plot), y = Max_Height)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site and Plot",
    y = "Woody plants height"
  ) +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8))

ggsave(Wheight_SP,
       filename = "C:/workspace/gumbo_dev/Plots/Violin woody height SP.png",
       width = 16, height = 14, units = "cm" )

##########################################

### Violin - with jitter
#ggplot(woody_species, aes(x = interaction(Site, Plot), y = Max_Height)) +
 # geom_violin(color = "Blue") +
  #geom_jitter(width = 0.2, alpha = 0.5) +
  #labs(
   # x = "Site and Plot",
    #y = "Woody plants height",
   # fill = "Site"
  #) +
  #theme_beautiful() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 0.8))

################################################################################
########################################################

###### Counting woody species

data <- read.csv("C:/workspace/gumbo_dev/DATA/WoodyPlants_C.csv")
  
# Grouping according to site
WoodySpp <- data %>%
  group_by(Site) %>%
  count(Spp_name)

#Number of woody plants species per site
Woody_species_unique <- data %>%
  group_by(Site) %>%
  summarise(n_species = length(unique(Spp_name)))

# 
WSpp <- ggplot(Woody_species_unique) + 
  geom_col(aes(x = Site, y = n_species), fill = "grey", colour = "grey", width = 0.75) +
  labs(x = "Site", y = "No. of woody species") + theme_beautiful()

# Saving as png
ggsave(WSpp,
       filename = "C:/workspace/gumbo_dev/Plots/ Woody Species Site.png",
       width = 16, height = 14, units = "cm" )


# To analyse at site and plot level
# Count unique woody species at each site

site_summary <- data %>%
  group_by(Site) %>%
  summarise(
    species_count = n_distinct(Spp_name),
    total_individuals = n()  # Counts the number of rows for each site
  )

# View the result
print(site_summary)

# Count unique woody species at each plot within each site
plot_summary <- data %>%
  group_by(Site, Plot) %>%
  summarise(
    species_count = n_distinct(Spp_name),
    total_individuals = n ()  # Assuming 'Count' column exists
  )

# Plot species richness by site
#ggplot(site_summary, aes(x = Site, y = species_count)) +
 # geom_bar(stat = "identity", fill = "steelblue") +
#  labs(title = "Species Richness by Site", x = "Site", y = "Number of Species") +
#  theme_beautiful() + theme(legend.position = "right")


# Plot species richness by plot within each site
WSpp2 <- ggplot(plot_summary, aes(x = Plot, y = species_count, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Plot", y = "No. of woody species") +
  theme_beautiful() + theme(legend.position = "right")

# Saving as png
ggsave(WSpp2,
       filename = "C:/workspace/gumbo_dev/Plots/ Woody Species Site & Plot.png",
       width = 16, height = 14, units = "cm" )


###########################CALCULATING SPECIES DIVERSITY

data <- read.csv("C:/workspace/gumbo_dev/DATA/WoodyPlants_C.csv")

library(tidyverse)
library(vegan)


##calculate species abundance per plot
plot_data <- data %>%
  group_by(Site, Plot, Spp_name) %>%
  summarise(abundance = n(), .groups = 'drop')

## calculate shannon diversity per plot
plot_diversity <- plot_data %>%
  group_by(Site, Plot) %>%
  summarise(shannon_index = -sum((abundance / sum(abundance)) * log(abundance / sum(abundance))), .groups = 'drop')

# Calculate site-level Shannon diversity (mean across plots)
site_diversity <- plot_diversity %>%
  group_by(Site) %>%
  summarise(Site_shannon_index = mean(shannon_index), .groups = 'drop')


# Visualisation site-level diversity

SWD <- ggplot(site_diversity, aes(x = Site, y = Site_shannon_index)) +
  geom_bar(stat = "identity") +
  theme_beautiful() +
  labs(x = "Site", y = "Diversity index")

# Saving as png
ggsave(SWD,
       filename = "C:/workspace/gumbo_dev/Plots/ Woody Diversity Index site.png",
       width = 16, height = 14, units = "cm" )

##AT PLOT LEVEL
# Plot plot-level Shannon diversity
SDI <- ggplot(plot_diversity, aes(x = interaction(Site, Plot), y = shannon_index, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs(x = "Site-Plot", y = "Diversity index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Saving as png
ggsave(SDI,
       filename = "C:/workspace/gumbo_dev/Plots/ Woody Diversity Index site-PLOT.png",
       width = 16, height = 14, units = "cm" )



######################### DETERMINING BETA DIVERSITY

# Prepare species abundance data
Bspecies_matrix <- data %>%
  group_by(Site, Spp_name) %>%
  summarise(abundance = n(), .groups = 'drop') %>%
  spread(key = Spp_name, value = abundance, fill = 0)



# Calculate Bray-Curtis dissimilarity
bc_dist <- vegdist(Bspecies_matrix[,-1], method = "bray")  # Remove site column

# View the dissimilarity matrix
bc_dist

#################### CALCULATING SIMPSONS DIVERSITY INDEX
# Calculate species abundance per plot
plot_data <- data %>%
  group_by(Site, Plot, Spp_name) %>% 
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
SIMP <- ggplot(site_diversity_simpson, aes(x = Site, y = site_simpson_index)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs( y = "Simpson Diversity Index") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Saving as png
ggsave(SIMP,
       filename = "C:/workspace/gumbo_dev/Plots/ Woody SIMP Diversity Index SITE.png",
       width = 16, height = 14, units = "cm" )


#PLOT -level Simpson diversity index
Simp <- ggplot(plot_diversity_simpson, aes(x = interaction(Site, Plot), y = simpson_index, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs(x = "Site - Plot", y = "Simpson Diversity Index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#  +theme(legend.position = "right")

# Saving as png
 ggsave(Simp,
       filename = "C:/workspace/gumbo_dev/Plots/ Woody SIMP Diversity Index PLOT.png",
       width = 16, height = 14, units = "cm" )
###################################################################################



###### WOODY PLANTS DENSITY 
# Create dataframe 
Woodydensity <- data.frame(Site = c("A", "B", "C", "D", "E", "F"),
                           Seedlings = c(1723, 1989, 2150, 1670, 3587, 988),
                           Saplings = c(2896, 1287, 971, 2190, 1544, 1361),
                           Trees = c(2223, 1617, 3298, 2986, 3126, 2043))

# Calculate density per site
Woodydensity2 <- Woodydensity %>%
  mutate(
    Seedlings_Density_Ha = (Seedlings * 10000) / 12000,
    Saplings_Density_Ha = (Saplings * 10000) / 12000,
    Trees_Density_Ha = (Trees * 10000) / 12000
  )
