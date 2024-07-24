#### CREATING MULTIPANELS FOR GRASSES 
library(tidyverse)
library(patchwork)
library(ggplot2)


###(1) Grass height

Grassheight <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grasses height.csv")

# Calculate average height per site
average_height <- Grassheight %>%
  group_by(Site) %>%
  summarise(average_height = mean(DPM.Height))

# Print the average height per site
print(average_height)

# Plot average height per site
ggplot(average_height, aes(x = Site, y = average_height)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Site",
    y = "Mean height (cm)"
  ) +
  theme_minimal() #+ theme(axis.title.x = element_text(size = 16), 
                   #    axis.title.y = element_text(size = 16),
                    #   axis.text.x = element_text(size = 14),  
                     #  axis.text.y = element_text(size = 14)) 

###(2)Grass number of species

Grass_species <- Grasses %>%
  group_by(Site) %>%
  count(Spp_name)

######### Number of grass spp per site
Grass_species_unique <- Grass_species %>%
  group_by(Site) %>%
  summarise(n_species = length(unique(Spp_name)))

##### Plotting graphs for grass species per site
#ggplot(Grass_species_unique) +
 # geom_col(aes(x = Site, y = n_species), fill = "lightgreen", colour = "darkgreen", width = 0.75) +
  #labs(y = "No. of species") + theme_classic()+
  #theme(axis.title.x = element_text(size = 16), 
   #     axis.title.y = element_text(size = 16),
    #    axis.text.x = element_text(size = 14),  
     #   axis.text.y = element_text(size = 14)) 

###Geombar for NO. of species 

ggplot(Grass_species_unique, aes(x = Site, y = n_species)) +
  geom_bar(stat = "identity")+ #fill = "green") +
  theme_minimal() +
  labs(,
       x = "Site",
       y = "No.of species")#+
  #theme(axis.title.x = element_text(size = 16), 
   #     axis.title.y = element_text(size = 16),
    #    axis.text.x = element_text(size = 14),  
     #   axis.text.y = element_text(size = 14)) 



####(3)GRASS SPECIES DIVERSITY

#data <- read.csv("path/to/your/data.csv", fileEncoding = "UTF-8")
DataGspp <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grasses spp only.csv", stringsAsFactors = FALSE)
# Print the column names and the first few rows to verify the data structure
print(colnames(DataGspp))
print(head(DataGspp))
tail(DataGspp)
view(DataGspp)
# Group by Site and Spp_name to count occurrences
species_counts <- DataGspp %>%
  group_by(Site, Spp_name) %>%
  summarize(Count = n(), .groups = 'drop')

# Print the species counts to verify
head(species_counts)
tail(species_counts)


# Function to calculate Shannon-Wiener Diversity Index
shannon_wiener <- function(counts) {
  prop <- counts / sum(counts)
  prop <- prop[prop > 0] # Remove zero proportions to avoid log(0)
  -sum(prop * log(prop))
}

# Calculate diversity index per site
diversity_index <- species_counts %>%
  group_by(Site) %>%
  summarize(DiversityIndex = shannon_wiener(Count), .groups = 'drop')

# Print the diversity index for each site
view(diversity_index)
### bar graph for the diversity index
ggplot(diversity_index, aes(x = Site, y = DiversityIndex)) +
  geom_bar(stat = "identity")+ #fill = "green") +
  theme_minimal() +
  labs(,
       x = "Site",
       y = "Diversity index")#+
  #theme(axis.title.x = element_text(size = 16), 
   #     axis.title.y = element_text(size = 16),
    #    axis.text.x = element_text(size = 14),  
     #   axis.text.y = element_text(size = 14)) 

#####Creating multi panel figure
library(patchwork)

#######name your panels e.g panel1 = height, panel2 = number of spp, panel3= diversity index
panel1 <- ggplot(average_height, aes(x = Site, y = average_height)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Site",
    y = "Mean height (cm)"
  ) +
  theme_classic() + 
  #theme(axis.title.x = element_text(size = 12), 
   #                       axis.title.y = element_text(size = 12),
    #                      axis.text.x = element_text(size = 10),  
     #                     axis.text.y = element_text(size = 10)) 
  theme(plot.title = element_text(hjust = -0.1),axis.title.y = element_text(size = 10), 
        axis.title.x = element_text(size = 10))



panel2 <-ggplot(Grass_species_unique, aes(x = Site, y = n_species)) +
  geom_bar(stat = "identity")+ #fill = "green") +
  theme_classic() +
  labs(,
       x = "Site",
       y = "No.of species")+
 # theme(axis.title.x = element_text(size = 16), 
  #      axis.title.y = element_text(size = 16),
   #     axis.text.x = element_text(size = 14),  
    #    axis.text.y = element_text(size = 14))+
  theme(plot.title = element_text(hjust = -0.1),axis.title.y = element_text(size = 10), 
        axis.title.x = element_text(size = 10))  
  
  


panel3 <- ggplot(diversity_index, aes(x = Site, y = DiversityIndex)) +
  geom_bar(stat = "identity")+ #fill = "green") +
  theme_classic() +
  labs(,
       x = "Site",
       y = "Diversity index")+
  #theme(axis.title.x = element_text(size = 12), 
   #     axis.title.y = element_text(size = 12),
    #    axis.text.x = element_text(size = 10),  
     #   axis.text.y = element_text(size = 10)) +
  theme(plot.title = element_text(hjust = -0.1),axis.title.y = element_text(size = 10), 
        axis.title.x = element_text(size = 10))
   

# Create a multi-panel layout using Patchwork
multi_panel_plot <- (panel1)/(panel3+panel2)+
  plot_annotation(tag_levels = 'a') +
  theme(plot.tag.position = c(0, 1), plot.tag = element_text(size = 0.5, hjust = -0.5))


#####combined_plot <- (plot_height + plot_diversity) / (plot_canopy + plot_density) +
  ###plot_annotation(tag_levels = 'a')

#print(multi_panel_plot)

ggsave("multi_panel_plot.png", multi_panel_plot, width = 16, height = 12, units = "cm")





###############################################################################################
###############################################################################################









#############
############ WOODY PLANT SPP DIVERSITY AFTER MANIPULATION################################
Treespp <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Woody spp only.csv")
view(Treesspp)
###### Group species counts by Site and Spp_name


###(1) No. of Tree spp
library(tidyverse)
library(dplyr)
library(ggplot2)

#### Counting woody species
Woody_species <- Woody_P %>%
  group_by(Site) %>%
  count(Spp_name)

###### Number of woody plant spp per site
Woody_species_unique <- Woody_P %>%
  group_by(Site) %>%
  summarise(n_species = length(unique(Spp_name)))


ggplot(Woody_species_unique) + 
  geom_col(aes(x = Site, y = n_species), fill = "lightgreen", colour = "darkgreen", width = 0.75) +
  labs(y = "Species composition") + theme_classic()


####### With no colours 1
ggplot(Woody_species_unique) + 
  geom_col(aes(x = Site, y = n_species), fill = "lightgreen", colour = "darkgreen", width = 0.75) +
  labs(y = "No. of species") + theme_classic() +
  theme(axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        axis.text.x = element_text(size = 18),  
        axis.text.y = element_text(size = 18)) 

##(2) Mean Tree height
ggplot(Results) +
  geom_col(aes(x = SITE, y = Mean_height_of_trees), fill = "darkgrey", colour = "black", width = 0.75) +
  labs(y = "Mean tree height") + theme_classic() + 
  theme(axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12))


##  (3) STEM COUNT PER HECTARE
# Reshape the data for plotting
plant_height2_long <- plant_height2 %>%
  select(Site, Stem_count)%>%     ###Trees, Seedlings, Saplings) %>%
  pivot_longer(cols = -Site, names_to = "Category", values_to = "Density")

# Plot the density for each category per site
ggplot(plant_height2_long, aes(x = Site, y = Density)) +   #, ##fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site",
       y = "Stem count (per hectare)"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold")
  )

## (4) PLANT DENSITY
# Given data
plant_height2 <- data.frame(
  Site = c("A", "B", "C", "D", "E", "F"),
  Trees = c(1853, 1348, 2749, 2488, 2606, 1703),
  ###Stem_count = c(13512, 12008, 18337, 16543, 15108, 11432)
  Seedlings = c(1437, 1658, 1793, 1393, 2990, 825),
  Saplings = c(2413, 1074, 810, 1826, 1288, 1134)
)

# Define the plot area in square meters
plot_area_sqm <- 12000

# Convert the plot area to hectares
plot_area_hectares <- plot_area_sqm / 10000



# Pivot the data
plant_height2_long <- plant_height2 %>%
  pivot_longer(cols = c("Trees", "Seedlings", "Saplings"), 
               names_to = "Category", 
               values_to = "Density")

# Print the pivoted data to check
print(plant_height2_long)

# Create a bar graph
plantD <- ggplot(plant_height2_long, aes(x = Site, y = Density, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "Site", y = "Density", fill = "Category")

# Print the bar graph
print(plantD)

# Save the bar graph
ggsave("plant_density_bar_graph.png", p, width = 8, height = 6, units = "in")


#######name your panels e.g W_panel1 = No of spp, W_panel2 = Tree height, w_panel3= Stem count panel4 = Plant density

W_panel1 <- ggplot(Woody_species_unique) + 
  geom_col(aes(x = Site, y = n_species), fill = "lightgreen", colour = "darkgreen", width = 0.75) +
  labs(y = "No. of species") + theme_classic() +
  #theme(axis.title.x = element_text(size = 22), 
   #     axis.title.y = element_text(size = 22),
    #    axis.text.x = element_text(size = 18),  
     #   axis.text.y = element_text(size = 18)) +
  theme(plot.title = element_text(hjust = -0.1),axis.title.y = element_text(size = 10), 
        axis.title.x = element_text(size = 10))


W_panel2 <- ggplot(Results) +
  geom_col(aes(x = SITE, y = Mean_height_of_trees), fill = "darkgrey", colour = "black", width = 0.75) +
  labs(y = "Mean tree height") + theme_classic() + 
  #theme(axis.title.x = element_text(size = 14), 
   #     axis.title.y = element_text(size = 14), 
    #    axis.text.x = element_text(size = 12),  
     #   axis.text.y = element_text(size = 12))+
  theme(plot.title = element_text(hjust = -0.1),axis.title.y = element_text(size = 10), 
        axis.title.x = element_text(size = 10))


W_panel3 <- ggplot(plant_height2_long, aes(x = Site, y = Density)) +   #, ##fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site",
       y = "Stem count (per hectare)"
  ) +
  theme_classic() +
  #theme(
   # axis.title.x = element_text(size = 14),
    #axis.title.y = element_text(size = 14),
    #axis.text.x = element_text(size = 12),
    #axis.text.y = element_text(size = 12),
  theme(plot.title = element_text(hjust = -0.1),axis.title.y = element_text(size = 10), 
        axis.title.x = element_text(size = 10))


W_panel4 <- plantD <- ggplot(plant_height2_long, aes(x = Site, y = Density, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "Site", y = "Density per hectare", fill = "Category")+
  theme_classic()+
  #theme(
   # axis.title.x = element_text(size = 14),
    #axis.title.y = element_text(size = 14),
    #axis.text.x = element_text(size = 12),
    #axis.text.y = element_text(size = 12),
    #plot.title = element_text(size = 16, face = "bold")
  theme(plot.title = element_text(hjust = -0.1),axis.title.y = element_text(size = 10), 
  axis.title.x = element_text(size = 10))
  

# Create a multi-panel layout using Patchwork
multi_panel_plot <- (W_panel1+W_panel2)/(W_panel3+W_panel4)+
  plot_annotation(tag_levels = 'a') +
  theme(plot.tag.position = c(0, 1), plot.tag = element_text(size = 0.5, hjust = -0.5))


#####combined_plot <- (plot_height + plot_diversity) / (plot_canopy + plot_density) +
###plot_annotation(tag_levels = 'a')

print(multi_panel_plot)

ggsave("multi_panel_plot.png", multi_panel_plot, width = 16, height = 13, units = "cm")









