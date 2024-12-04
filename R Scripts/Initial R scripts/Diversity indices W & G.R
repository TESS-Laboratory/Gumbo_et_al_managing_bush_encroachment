#############GRASS SPECIES DIVERSITY
library(tidyverse)
# Install necessary packages
# Load the data from the Excel file



###############################################################
###GRASS DIVERSITY DATA MANIPULATED TO REMOVE DOUBLE COUNTING OF SOME SPECIES
Grassspp<- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grasses spp only.csv")
view(Grassspp)
###### Group species counts by Site and Spp_name
Grassspp <- Grassspp %>%
  group_by(Site, Spp_name) %>%
  summarise(Count = n()) %>%
  ungroup()
view(Grassspp)


##### Group by Site and Species, then sum the counts
Grassspp <- Grassspp %>%
  group_by(Site, Spp_name) %>%
  summarise(Count = sum(Count)) %>%
  spread(Spp_name, Count, fill = 0)
# View the grouped and summarized dataset
view(Grassspp)

# Convert data to a matrix for vegan package
species_matrix <- as.data.frame(Grassspp %>% column_to_rownames(var = "Site"))

# Ensure all columns are numeric
species_matrix <- as.data.frame(lapply(species_matrix, as.numeric))

# Calculate Shannon-Wiener diversity index for each site
shannon_index <- diversity(species_matrix, index = "shannon")

# Combine the Shannon index with site data
diversity_data <- data.frame(Site = rownames(species_matrix), Shannon_Index = shannon_index)

# View the Shannon-Wiener diversity index data
print(diversity_data)

##### Plot the Shannon-Wiener diversity index
ggplot(diversity_data, aes(x = Site, y = Shannon_Index)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Site",
    y = "Diversity indices"
  ) +
  theme_classic()

#############
############ WOODY PLANT SPP DIVERSITY AFTER MANIPULATION################################
Treespp <- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Woody spp only.csv")
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

# View the Shannon-Wiener diversity index data
print(diversity_data)

##### Plot the Shannon-Wiener diversity index
ggplot(diversity_data, aes(x = Site, y = Shannon_Index)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Site",
    y = "Diversity indices"
  ) +
  theme_classic()


############## GRASS HEIGHT 
Grassheight <- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grasses height.csv")

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
    y = "Mean grass height (cm)"
  ) +
  theme_classic()
  
  ###############plotting a boxplot for grass height

ggplot(average_height, aes(x = Site, y = average_height)) +
  geom_col() +
  labs(
    x = "Site",
    y = "Mean grass height (cm)"
  ) +
  theme_classic()
