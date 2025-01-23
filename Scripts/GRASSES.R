library(tidyverse)
library(vegan)

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


GrassH <- read.csv("C:/workspace/gumbo_dev/DATA/Grasses_C.csv")

# Data 
Grass_H <- GrassH %>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot),
         Subplot = as.factor(Subplot),
         Height = as.numeric(DPM.Height))

# Summarize height statistics by Site, Plot, and Subplot
Gheight_summary <- Grass_H %>%
  group_by(Site, Plot) %>%
  summarize(
    Mean_Height = mean(DPM.Height, na.rm = TRUE),
    Median_Height = median(DPM.Height, na.rm = TRUE),
    Min_Height = min(DPM.Height, na.rm = TRUE),
    Max_Height = max(DPM.Height, na.rm = TRUE),
    .groups = "drop"
  )

# View summary
head(height_summary)
tail (height_summary)

# Create the boxplot at Site and Plot level
GHeight <- ggplot(Grass_H, aes(x = interaction(Site, Plot), y = DPM.Height , fill = Site)) +
  geom_boxplot() +
  labs(
    x = "Site and Plot",
    y = "DPM height (cm)"
  ) +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  theme(legend.position = "right")

# Saving as png
ggsave(GHeight,
       filename = "C:/workspace/gumbo_dev/Plots/Boxplot - Grass Height SP.png",
       width = 16, height = 14, units = "cm" )

####################### Create the violin plot at site level

# Gheight creates object that can be saved
Gheight <- ggplot(Grass_H, aes(x = Site, y = DPM.Height)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site",
    y = "DPM height (cm)"
  ) +
  theme_beautiful()

# Saving as png
ggsave(GHeight,
       filename = "C:/workspace/gumbo_dev/Plots/Violin - Grass Height Site.png",
       width = 16, height = 14, units = "cm" )
  


# Create  summary data frame
Gheight_summary <- Grass_H %>%
  group_by(Site, Plot) %>%
  summarize(
    Mean_Height = mean(DPM.Height, na.rm = TRUE),
    .groups = "drop"
 )


### Create  bar graph for Site and Plot level
GpHeight <- ggplot(Gheight_summary, aes(x = interaction(Site, Plot), y = Mean_Height, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site-Plot", y = "DPM height (cm)") +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(legend.position = "right")

ggsave(GpHeight,
       filename = "C:/workspace/gumbo_dev/Plots/Bar - Grass Height SP.png",
       width = 16, height = 14, units = "cm" )

###############################################################################
library(vegan)


##calculate species abundance per plot
Gplot_data <- Grass_H %>%
  group_by(Site, Plot, Spp_name) %>%
  summarise(abundance = n(), .groups = 'drop')

## calculate shannon diversity per plot
Gplot_diversity <- Gplot_data %>%
  group_by(Site, Plot) %>%
  summarise(shannon_index = -sum((abundance / sum(abundance)) * log(abundance / sum(abundance))), .groups = 'drop')

# Calculate site-level Shannon diversity (mean across plots)
Gsite_diversity <- Gplot_diversity %>%
  group_by(Site) %>%
  summarise(Site_shannon_index = mean(shannon_index), .groups = 'drop')


# Visualising site-level diversity

SWGD <- ggplot(Gsite_diversity, aes(x = Site, y = Site_shannon_index)) +
  geom_bar(stat = "identity") +
  theme_beautiful() +
  labs(x = "Site", y = "Diversity index")

# Saving as png
ggsave(SWGD,
       filename = "C:/workspace/gumbo_dev/Plots/ Grass Diversity Index site.png",
       width = 16, height = 14, units = "cm" )



## DIVERSITY AT PLOT LEVEL
# Plot plot-level Shannon diversity
GSDI <- ggplot(Gplot_diversity, aes(x = interaction(Site, Plot), y = shannon_index, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs(x = "Site-Plot", y = "Diversity index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Saving as png
ggsave(GSDI,
       filename = "C:/workspace/gumbo_dev/Plots/ Grass Diversity Index Site-PLOT.png",
       width = 16, height = 14, units = "cm" )

#####################################################################################################

###DETERMINING BETA DIVERSITY

# Prepare species abundance data
GBspecies_matrix <- Grass_H %>%
  group_by(Site, Spp_name) %>%
  summarise(abundance = n(), .groups = 'drop') %>%
  spread(key = Spp_name, value = abundance, fill = 0)



# Calculate Bray-Curtis dissimilarity
bc_dist <- vegdist(Bspecies_matrix[,-1], method = "bray")  # Remove site column

# View the dissimilarity matrix
bc_dist


####################### CALCULATING SIMPSONS DIVERSITY INDEX
# Calculate species abundance per plot
Gplot_data <- Grass_H %>%
  group_by(Site, Plot, Spp_name) %>% 
  summarise(abundance = n(), .groups = 'drop')

# Calculate site-level Simpson diversity (mean across plots)
Gsite_diversity_simpson <- plot_diversity_simpson %>%
  group_by(Site) %>%
  summarise(site_simpson_index = mean(simpson_index), .groups = 'drop')

# SITE - level Simpson diversity index
GSIMP <- ggplot(Gsite_diversity_simpson, aes(x = Site, y = site_simpson_index)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs( y = "Simpson Diversity Index") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Saving as png
ggsave(GSIMP,
       filename = "C:/workspace/gumbo_dev/Plots/ GRASS Simp Diversity Index SITE.png",
       width = 16, height = 14, units = "cm" )

######################

### Calculating Simpson's Index for each plot
Gplot_diversity_simpson <- plot_data %>%
  group_by(Site, Plot) %>%
  summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')
View(Gplot_diversity_simpson)

#PLOT -level Simpson diversity index
GSimp <- ggplot(Gplot_diversity_simpson, aes(x = interaction(Site, Plot), y = simpson_index, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs(x = "Site - Plot", y = "Simpson Diversity Index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#  +theme(legend.position = "right")

# Saving as png
ggsave(GSimp,
       filename = "C:/workspace/gumbo_dev/Plots/ GRASS SIMP Diversity Index PLOT.png",
       width = 16, height = 14, units = "cm" )

############################################################################