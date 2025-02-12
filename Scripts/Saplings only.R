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


################  DETERMINING METRICS FOR SAPLINGS
###### SAPLINGS HEIGHT

data <- read_csv ("C:/workspace/gumbo_dev/DATA/Woody_sizeclasses.csv")

# Filter for SAPLINGS and calculate height statistics at the site level
saplings_height_summary <- data %>%
  filter(!is.na(Saplings)) %>%  # Keep only rows where Trees are recorded
  group_by(Site, Plot) %>%
  summarise(
    Mean_Height = mean(Max_Height, na.rm = TRUE),   
    Median_Height = median(Max_Height, na.rm = TRUE),
    Max_Height = max(Max_Height, na.rm = TRUE),     
    Min_Height = min(Max_Height, na.rm = TRUE),     
    Count = n(), 
    .groups = "drop"  )

# Filter for SAPLINGS 
saplings_height_summary <- data %>%
  filter(!is.na(Saplings))

SHgt <- ggplot(saplings_height_summary, aes(x = interaction(Site,Plot), y = Max_Height)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site and Plot",
    y = "Tree height (m)"
  ) +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8))

# Saving as png
ggsave(SHgt,
       filename = "C:/workspace/gumbo_dev/Plots/ SAPLINGS HEIGHT Site&Plot.png",
       width = 16, height = 14, units = "cm" )

########################################################################################

## DETERMINE SAPLINGS COMPOSITION

# Filter for SAPLINGS 

saplings_comp <- data %>%
  filter(!is.na(Saplings)) %>%  # Keep only rows where Trees are recorded
  group_by(Site, Plot, Spp_name) %>%
  summarise(Count = n(), 
            .groups = "drop"  )

#Number of tree species per site
saplings_comp2 <- data %>%
  filter(!is.na(Saplings)) %>% 
  group_by(Site) %>%
  summarise(Saplings_count = length(unique(Spp_name)))

# 
Scompa <- ggplot(saplings_comp2) + 
  geom_col(aes(x = Site, y = Saplings_count), fill = "grey", colour = "grey", width = 0.75) +
  labs(x = "Site", y = "No. of Sapling species", tag = "(a)") + theme_beautiful() +
  #plot_annotation("(a)")
  theme(
    legend.position = "right",
    plot.tag.position = c(0, 0.95)) # Adjust tag position (x, y)


# Saving as png
ggsave(Scompa,
       filename = "C:/workspace/gumbo_dev/Plots/ SAPLINGS Species Site.png",
       width = 16, height = 14, units = "cm" )


### Count unique saplings species at each plot within each site
Splot_summary <- data %>%
  filter(!is.na(Saplings)) %>% 
  group_by(Site, Plot) %>%
  summarise(
    species_count = n_distinct(Spp_name),
    total_individuals = n (),  # Assuming 'Count' column exists
    .groups = "drop" )

# Plot species richness by plot within each site
#Spp2b <- ggplot(Splot_summary, aes(x = Plot, y = species_count, fill = Site)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  labs(x = "Plot", y = "No. of saplings species") +
#  theme_beautiful() + 
#  theme(legend.position = "right") +
#  plot_annotation("(b)") 


######USING TAG ANNOTATIONS
Sppb <- ggplot(Splot_summary, aes(x = Plot, y = species_count, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Plot", y = "No. of Sapling species", tag = "(b)") + # optional to add tag = . Remove it when not necessary
  theme_beautiful() + 
   theme(                                  ## when adjusting tag 'a'
    legend.position = "right",
    plot.tag.position = c(0, 0.95)) # Adjust tag position (x, y)  


ggsave(Sppb,
       filename = "C:/workspace/gumbo_dev/Plots/ SAPLINGpecies SiteandPlot.png",
       width = 16, height = 14, units = "cm" )

## COMBINING SPECIES COMPOSITION PLOTS INTO A SINGLE MULTI PANEL FOR SITE AND SP

# Combining plots
Aab <- (Scompa|Sppb)

ggsave(Aab,
       filename = "C:/workspace/gumbo_dev/Plots/SPP COMPO Combinedplots -Saplings.png",
       width = 16, height = 12, units = "cm" )



##########################################################################################

### REDOING SAPLINGS DIVERSITY CALCULATIONS SO THAT THEY SHOW PER SITE

# Filter data for trees and group by Site, Plot, and Spp_name
# Filter data for trees and calculate species abundance
saplings_data2 <- data %>%
  filter(!is.na(Saplings)) %>%                  # Filter out rows with no Trees
  group_by(Site, Plot, Spp_name) %>%         # Group by Site, Plot, and Species
  summarise(Saplings_Count = n(), .groups = "drop")  # Count the number of trees per species

# Calculate Shannon-Wiener Diversity Index at Site and Plot level
diversity_data <- saplings_data2 %>%
  group_by(Site, Plot) %>%                   # Group by Site and Plot
  summarise(
    Shannon_Diversity = -sum((Saplings_Count / sum(Saplings_Count)) * log(Saplings_Count / sum(Saplings_Count))),
    .groups = "drop"
  )

## Creating plot for shannon diversity at site level
#Ssdv <- ggplot(diversity_data, aes(x = Site, y = Shannon_Diversity)) +
#  geom_bar(stat = "identity", position = "dodge", fill = "grey", colour = "grey") +
#  labs(
#    x = "Site",
#    y = "Shannon-Weiner Diversity Index"
#  ) +
#  theme_beautiful() 

#### USING ANNOTATIONS
Ssdv <- ggplot(diversity_data, aes(x = Site, y = Shannon_Diversity)) +
  geom_bar(stat = "identity", position = "dodge", fill = "grey", colour = "grey") +
  labs(
    x = "Site",
    y = "Saplings Shannon-Weiner Index", tag = "(a)") + # optional to add tag = . Remove it when not necessary
   theme_beautiful() + 
   theme(                                    ## when adjusting tag 'a'
    legend.position = "right",
    plot.tag.position = c(0, 0.999)) # Adjust tag position (x, y)  

#Saving data
ggsave(Ssdv,
       filename = "C:/workspace/gumbo_dev/Plots/ ShannonD- Saplings SITE.png",
       width = 16, height = 14, units = "cm" )


### Creating plot for Shannon diversity at Site and Plot level
#Sp <- ggplot(diversity_data, aes(x = interaction(Site, Plot), y = Shannon_Diversity, fill = Site)) +
#  geom_bar(stat = "identity", show.legend = FALSE) +
#  theme_beautiful() +
#  labs(x = "Site-Plot", y = "Saplings Shannon-Weiner Index") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

## using PLOT ANNOTATIONS
Sp <- ggplot(diversity_data, aes(x = interaction(Site, Plot), y = Shannon_Diversity, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
 labs(
  x = "Site-Plot",
  y = "Saplings Shannon-Weiner Index", tag = "(b)") + # optional to add tag = . Remove it when not necessary
  theme_beautiful() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(                                    ## when adjusting tag 'a'
    legend.position = "right",
    plot.tag.position = c(0, 0.999)) # Adjust tag position (x, y)  

##Saving as png
ggsave(Sp,
       filename = "C:/workspace/gumbo_dev/Plots/ Saplings Shannon Diversity Index SP.png",
       width = 16, height = 10, units = "cm" )

## COMBINING SHANNON WEINER DIVERSITY PLOTS INTO A SINGLE MULTI PANEL FOR SITE AND SP
# Combining plots
Ac <- (Ssdv|Sp)

ggsave(Ac,
       filename = "C:/workspace/gumbo_dev/Plots/SHANNON Combinedplots -Saplings .png",
       width = 16, height = 10, units = "cm" )

############################################################################################
##########################################################################################
###########         CALCULATING SIMPSONS DIVERSITY INDEX FOR SAPLINGS

# Calculate species abundance per plot
Splot_data <- data %>%
  filter(!is.na(Saplings)) %>%  
  group_by(Site, Plot, Spp_name) %>% 
  summarise(abundance = n(), .groups = 'drop')

# Calculate Simpson's Index for each plot
Splot_diversity_simpson <- Splot_data %>%
  group_by(Site, Plot) %>%
  summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')


#write.csv(Tplot_diversity_simpson, "Trees Simpsons Diversity.csv")

# Calculate site-level Simpson diversity (mean across plots)
Ssite_diversity_simpson <- Splot_diversity_simpson %>%
  group_by(Site) %>%
  summarise(site_simpson_index = mean(simpson_index), .groups = 'drop')

# SITE - level Simpson diversity index
#SSIMP <- ggplot(Ssite_diversity_simpson, aes(x = Site, y = site_simpson_index)) +
#  geom_bar(stat = "identity", show.legend = FALSE) +
#  theme_beautiful() +
#  labs( y = "Simpson Diversity Index") +
#  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
#  plot_annotation("(a)")

## USING pLOT TAG ANNOTATION
SSIMP <- ggplot(Ssite_diversity_simpson, aes(x = Site, y = site_simpson_index)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs( y = "Saplings Simpson Diversity Index", tag = "(a)") + # optional to add tag = . Remove it when not necessary
    theme_beautiful() + 
     theme(                                    ## when adjusting tag 'a'
      legend.position = "right",
      plot.tag.position = c(0, 0.999)) # Adjust tag position (x, y)  

# Saving as png
ggsave(SSIMP,
       filename = "C:/workspace/gumbo_dev/Plots/ SAPLINGSIMP Diversity Index SITE.png",
       width = 16, height = 14, units = "cm" )


######  PLOT -level Simpson diversity index
#SPSimp <- ggplot(Splot_diversity_simpson, aes(x = interaction(Site, Plot), y = simpson_index, fill = Site)) +
#  geom_bar(stat = "identity", show.legend = FALSE) +
#  theme_beautiful() +
#  labs(x = "Site - Plot", y = "Saplings Simpson Diversity Index") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


###using plot tag annotations
SPSimp <- ggplot(Splot_diversity_simpson, aes(x = interaction(Site, Plot), y = simpson_index, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs(x = "Site - Plot", y = "Saplings Simpson Diversity Index", tag = "(b)") + # optional to add tag = . Remove it when not necessary
  theme_beautiful() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +     
  theme(                                    ## when adjusting tag 'a'
    legend.position = "right",
    plot.tag.position = c(0, 0.999)) # Adjust tag position (x, y)  

# Saving as png
ggsave(TPSimp,
       filename = "C:/workspace/gumbo_dev/Plots/ TreeSIMPSON Diversity Index PLOT.png",
       width = 16, height = 14, units = "cm" )


#### COMBINING SIMPSON PLOTS INTO A SINGLE MULTI PANEL FOR SITE AND SP

# Combining plots
Ab <- (TSIMP|TPSimp)

ggsave(Ab,
       filename = "C:/workspace/gumbo_dev/Plots/SIMPCombinedplots -Trees .png",
       width = 16, height = 10, units = "cm" )




##CREATING MULTI PANEL PLOT
## Combine all six plots in a 3-row × 2-column layout. MULTIPANEL PLOT
Tmulti_panel_plot <- (Tcompa + Tpp2b) / (Tsdv + Treep) / (TSIMP + TPSimp) +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = "(",       # Add opening bracket
                  tag_suffix = ")" )&     # Add closing bracket)  
  theme(plot.tag.position = c(-0.05, 1)) 

Tmulti_panel_plot

ggsave(Tmulti_panel_plot, 
       filename = "C:/workspace/gumbo_dev/Plots/TREE CCCombined SP.png",
       width = 16, height = 14, units = "cm" )

