library(tidyverse)
library(vegan)
library(patchwork)

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


GrassH <- read_csv("C:/workspace/gumbo_dev/DATA/Grasses_C.csv")

# Data 
Grass_H <- GrassH %>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot),
         Subplot = as.factor(Subplot),
         Height = as.numeric(DPM_Height))

# Summarize height statistics by Site, Plot, and Subplot
Gheight_summary <- Grass_H %>%
  group_by(Site, Plot) %>%
  summarize(
    Mean_Height = mean(DPM_Height, na.rm = TRUE),
    Median_Height = median(DPM_Height, na.rm = TRUE),
    Min_Height = min(DPM_Height, na.rm = TRUE),
    Max_Height = max(DPM_Height, na.rm = TRUE),
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

# Create the violin plot at Site and Plot level
Gheight_SP <- ggplot(Grass_H, aes(x = interaction(Site, Plot), y = DPM_Height)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site and Plot",
    y = "DPM height (cm)"
  ) +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8))

ggsave(Gheight_SP,
       filename = "C:/workspace/gumbo_dev/Plots/Violin GRASS height SP.png",
       width = 16, height = 14, units = "cm" )


# Gheight creates object that can be saved
Gheight <- ggplot(Grass_H, aes(x = Site, y = DPM_Height)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site",
    y = "DPM height (cm)") + 
   theme_beautiful() 
 
# Saving as png
ggsave(Gheight,
       filename = "C:/workspace/gumbo_dev/Plots/Violin - Grass Height Site.png",
       width = 16, height = 14, units = "cm" )


### Creating bar graph for SITE-PLOT interaction
# Create  summary data frame
Gheight_summary <- Grass_H %>%
  group_by(Site, Plot) %>%
  summarize(
    Mean_Height = mean(DPM_Height, na.rm = TRUE),
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


#############
###### USING tag annotations to create violin plots at SITE LEVEL DPM HEIGHT
Gsa <- ggplot(Grass_H, aes(x = Site, y = DPM_Height)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site",
    y = "DPM height (cm)",  tag = "(a)") + # optional to add tag = . Remove it when not necessary
  theme_beautiful() + 
  theme(                                    ## when adjusting tag 'a'
    legend.position = "right",
    plot.tag.position = c(0, 0.999)) 

ggsave(Gsa,
       filename = "C:/workspace/gumbo_dev/Plots/ DPM height- Site.png",
       width = 16, height = 14, units = "cm" )

#########
### USING tag annotations to create violin plots, as well as combining plots
Gsb <- ggplot(Gheight_summary, aes(x = interaction(Site, Plot), y = Mean_Height, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site-Plot", y = "DPM height (cm)", tag = "(b)" ) +
  theme_beautiful() +
  scale_x_discrete(labels = function(x) gsub("\\.", "", x)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(legend.position = "right")+
  theme(                                    ## when adjusting tag 'a'
    legend.position = "right",
    plot.tag.position = c(0, 0.999))

# Save as png
ggsave(Gsb,
       filename = "C:/workspace/gumbo_dev/Plots/ DPM height SitePLOT.png",
       width = 16, height = 14, units = "cm" )

##Combining plots

Ac <- (Gsa|Gsb)

ggsave(Ac,
       filename = "C:/workspace/gumbo_dev/Plots/DPM Height - COMBINED PLOTS.png",
       width = 16, height = 10, units = "cm" )

######################################################################################
#################################################################################################
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



############# USING TAG ANNOTATIONS FOR Shannon Weiner Diversity AT SITE, PLOT LEVEL
## AT SITE LEVEL
Gsw <- ggplot(Gsite_diversity, aes(x = Site, y = Site_shannon_index)) +
  geom_bar(stat = "identity") +
  theme_beautiful() +
  labs(x = "Site", y = "Shannon-Weiner diversity index", tag = ("(a)"))+
  theme(legend.position = "right")+
  theme(                                    ## when adjusting tag 'a'
    legend.position = "right",
    plot.tag.position = c(0, 0.999))

####at site-plot level
Gsp <- ggplot(Gplot_diversity, aes(x = interaction(Site, Plot), y = shannon_index, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("\\.", "", x))+
  theme_beautiful() +
  labs(x = "Site-Plot", y = "Shannon-Weiner diversity index", tag = ("(b)")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  theme(legend.position = "right")+
  theme(                                    ## when adjusting tag 'a'
    legend.position = "right",
    plot.tag.position = c(0, 0.999))

# Combining plots

SW <- (Gsw|Gsp)
ggsave(SW,
       filename = "C:/workspace/gumbo_dev/Plots/ Grass ShannonDIndex Combined Plot.png",
       width = 16, height = 10, units = "cm" )



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
  group_by(Site,Spp_name) %>% 
  summarise(abundance = n(), .groups = 'drop')

# Calculate site-level Simpson diversity (mean across plots)
Gsite_diversity_simpson <- Gplot_data %>%
  group_by(Site) %>%
 summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')

# Calculate site-level Simpson diversity (mean across plots)
Site_diversity_simpson <- Gsite_diversity_simpson %>%
  group_by(Site) %>%
  summarise(site_simpson_index = mean(simpson_index), .groups = 'drop')

# SITE - level Simpson diversity index
GSIMP <- ggplot(Site_diversity_simpson, aes(x = Site, y = site_simpson_index)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs( y = "Simpson Diversity Index", tag = "(a)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(                                    ## when adjusting tag 'a'
    legend.position = "right",
    plot.tag.position = c(0, 0.999))

# Saving as png
ggsave(GSIMP,
       filename = "C:/workspace/gumbo_dev/Plots/ GRASS Simp Diversity Index SITE.png",
       width = 16, height = 14, units = "cm" )

######################

# Calculate species abundance per plot
Gplot_data <- Grass_H %>%
  group_by(Site,Plot, Spp_name) %>% 
  summarise(abundance = n(), .groups = 'drop')

### Calculating Simpson's Index for each plot
Gplot_diversity_simpson <- Gplot_data %>%
  group_by(Site, Plot) %>%
  summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')


#PLOT -level Simpson diversity index
GSimp <- ggplot(Gplot_diversity_simpson, aes(x = interaction(Site, Plot), y = simpson_index, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("\\.", "", x))+
  theme_beautiful() +
  labs(x = "Site - Plot", y = "Simpson Diversity Index", tag = "(b)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(                                    ## when adjusting tag 'a'
    legend.position = "right",
    plot.tag.position = c(0, 0.999)) # Adjust tag position (x, y)  

# Saving as png
ggsave(GSimp,
       filename = "C:/workspace/gumbo_dev/Plots/ GRASS SIMP Diversity Index PLOT.png",
       width = 16, height = 14, units = "cm" )

######## COMBINING PLOTS 

sd <- (GSIMP|GSimp) 

##Saving combined plots 
ggsave(sd,
       filename = "C:/workspace/gumbo_dev/Plots/ GRASS SIMP Diversity Combined.png",
       width = 16, height = 14, units = "cm" )

#################################################################################################
##################################################################################################

## DETERMINE GRASS SPECIES COMPOSITION

GrassH <- read_csv("C:/workspace/gumbo_dev/DATA/Grasses_C.csv")

# Filter for Grasses 

Grass_comp <- Grass_H %>%
  filter(!is.na(Spp_name)) %>%  # Keep only rows where grass spp are recorded
  group_by(Site, Plot, Spp_name) %>%
  summarise(Count = n(), 
            .groups = "drop"  )

#Number of grass species per site
GrassC <- Grass_comp %>%
  filter(!is.na(Spp_name)) %>% 
  group_by(Site) %>%
  summarise(Grass_count = length(unique(Spp_name)))

# 
Gca <- ggplot(GrassC) + 
  geom_col(aes(x = Site, y = Grass_count), fill = "grey", colour = "grey", width = 0.75) +
  labs(x = "Site", y = "No. of grass species", tag = "(a)") + theme_beautiful() +
  theme(
    legend.position = "right",
    plot.tag.position = c(0, 0.99)) # Adjust tag position (x, y)


# Saving as png
ggsave(Gca,
       filename = "C:/workspace/gumbo_dev/Plots/ GRASSSpecies Site.png",
       width = 16, height = 14, units = "cm" )


### Count unique grass species at each plot within each site
Gplot_summary <- Grass_H %>%
  filter(!is.na(Spp_name)) %>% 
  group_by(Site, Plot) %>%
  summarise(
    species_count = n_distinct(Spp_name),
    total_individuals = n (),  # Assuming 'Count' column exists
    .groups = "drop" )

# Plot species richness by plot within each site
Gpp <- ggplot(Gplot_summary, aes(x = Plot, y = species_count, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Plot", y = "No. of grass species", tag = ("(b)")) +
  theme_beautiful() + 
  theme(legend.position = "right") +
  theme(
    legend.position = "right",
    plot.tag.position = c(0, 0.99)) # Adjust tag position (x, y)
  
## Save file as png
ggsave(Gpp,
       filename = "C:/workspace/gumbo_dev/Plots/ GRASSSpecies SP.png",
       width = 16, height = 14, units = "cm" )


#### COMBINING PLOTS

Gsc <- (Gca|Gpp)

##Save combined plots as png
ggsave(Gsc, 
       filename = "C:/workspace/gumbo_dev/Plots/Grass Spp Combined SP.png",
       width = 16, height = 10, units = "cm" )


## Combine all six plots in a 3-row × 2-column layout. MULTIPANEL PLOT
multi_panel_plot <- (Gca + Gpp) / (Gsw + Gsp) / (GSIMP + GSimp) +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = "(",       # Add opening bracket
                  tag_suffix = ")" )&     # Add closing bracket)  
   theme(plot.tag.position = c(-0.05, 1)) 

multi_panel_plot

ggsave(multi_panel_plot, 
       filename = "C:/workspace/gumbo_dev/Plots/Grass 2CCCombined SP.png",
       width = 16, height = 14, units = "cm" )
