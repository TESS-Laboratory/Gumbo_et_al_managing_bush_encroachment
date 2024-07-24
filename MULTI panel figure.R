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



#### include tag labels

# Display the multi-panel plot
# print(multi_panel_plot)

##ggsave("multi_panel_plot", width=16, height=16, units=cm)


###############################################################################################
###############################################################################################

## USING GEOM POINT


panel1 <- ggplot(average_height, aes(x = Site, y = average_height)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Site",
    y = "Mean grass height (cm)"
  ) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 18), 
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 16),  
        axis.text.y = element_text(size = 16)) 



panel2 <- ggplot(Grass_species_unique) +
  geom_col(aes(x = Site, y = n_species), fill = "lightgreen", colour = "darkgreen", width = 0.75) +
  labs(y = "No. of species") + theme_classic()+
  theme(axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),  
        axis.text.y = element_text(size = 18))


panel3 <- ggplot(diversity_index, aes(x = Site, y = DiversityIndex)) +
  geom_bar(stat = "identity")+ #fill = "green") +
  theme_classic() +
  labs(,
       x = "Site",
       y = "Diversity index")+
  theme(axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),  
        axis.text.y = element_text(size = 18)) 



plot_canopy <- ggplot(data_summary, aes(x = Site, y = CanopyCover)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Canopy Cover", x = "Site", y = "Canopy Cover")


















#############
############ WOODY PLANT SPP DIVERSITY AFTER MANIPULATION################################
Treespp <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Woody spp only.csv")
view(Treesspp)
###### Group species counts by Site and Spp_name
Treespp <- Treespp %>%
  group_by(Site, Spp_name) %>%
  summarise(Count = n()) %>%
  ungroup()
view(Treespp)

##### Group by Site and Species, then sum the counts
Treespp <- Treespp %>%
  group_by(Site, Spp_name) %>%
  summarise(Count = sum(Count)) %>%
  spread(Spp_name, Count, fill = 0)
# View the grouped and summarized dataset
view(Treespp)

# Convert data to a matrix for vegan package
species_matrix <- as.data.frame(Treespp %>% column_to_rownames(var = "Site"))


# Ensure all columns are numeric
species_matrix <- as.data.frame(lapply(species_matrix, as.numeric))

# Calculate Shannon-Wiener diversity index for each site
shannon_index <- diversity(species_matrix, index = "shannon")

##Combine the Shannon index with site data
diversity_data <- data.frame(Site = rownames(species_matrix), Shannon_Index = shannon_index)


ggplot(diversity_data, aes(x = Site, y = Shannon_Index)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Site",
    y = "Diversity indices"
  ) +
  theme_classic()
