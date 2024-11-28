#############GRASS SPECIES DIVERSITY
# Load necessary libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
# Read the data from CSV file with correct encoding
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
  geom_bar(stat = "identity", fill = "grey") +
  theme_classic() +
  labs(,
       x = "Site",
       y = "Diversity Index") +
  theme(plot.title = element_text(hjust = 0.5))















###############################################################
###GRASS DIVERSITY DATA MANIPULATED TO REMOVE DOUBLE COUNTING OF SOME SPECIES
##Grassspp<- read_csv("DATA/Grasses2.csv") #####RELATIVE PATH WHICH CAN BE EASILY ACCESSED ON GITHUB
#view(Grass)
Grassspp<- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grasses spp only.csv")

summary(Grassspp)
Grassspp %>% ggplot(aes(x=Spp_name, y=))
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

##### something deleted by mistakeclean_column_names <- function(names) {
 names <- make.names(names, unique = TRUE) # Ensure unique and valid column names
  names <- gsub("[^[:alnum:]_]", "", names) # Remove non-alphanumeric characters
  return(names)}

# Select species data (replace with the actual column selection logic)
#species_matrix <- data %>%
 # select(starts_with("Site")) # Adjust the selection based on your actual data structure
# Ensure all columns are numeric

species_matrix <- as.data.frame(lapply(species_matrix, as.numeric))

# Convert columns to numeric
species_matrix <- as.data.frame(lapply(species_matrix, as.numeric))

# Print the first few rows of the species matrix to verify
print(head(species_matrix))


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


library(tidyverse)
library(ggplot2)
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
