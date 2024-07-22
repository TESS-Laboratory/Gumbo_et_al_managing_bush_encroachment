library(tidyverse)
# Install necessary packages
install.packages("readxl")
install.packages("vegan")
install.packages("ggplot2")

# Load the packages
library(readxl)
library(vegan)
library(ggpplot)


Woody <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Woody2.csv")
head(Woody)
attach(Woody)


#### Counting woody species
Woody<- Woody %>% 
  group_by(Site) %>%
  count(Spp_name)
view(Woody)

###### Number of woody plant spp per site
Woody_unique_species <- Woody_P %>%
  group_by(Site) %>%
  summarise(n_species = length(unique(Spp_name)))
view(Woody_unique_species)


##### plotting graphs for species count per site
#ggplot(Woody_species_unique) +
#geom_col(aes(x = Site, y = n_species), fill = "lightgreen", colour = "darkgreen", width = 1) +
# labs(y = "Number of woody species") +
#theme_classic()
ggplot(Woody_unique_species) + 
  geom_col(aes(x = Site, y = n_species), fill = "lightgreen", colour = "darkgreen", width = 0.5) +
  labs(y = "No. of woody species") + theme_classic()



################## New method plus data cleaning 
head(Woody3)
# Function to clean species names
clean_names <- function(name) {
  # Convert to ASCII, replacing non-ASCII characters with a placeholder
  name <- iconv(name, to = "ASCII//TRANSLIT", sub = "")
  # Remove any remaining non-alphanumeric characters
  name <- gsub("[^[:alnum:] ]", "", name)
  return(name)
}

# Clean the species names
Woody3$Spp_name <- sapply(Woody3$Spp_name, clean_names)

# Ensure column names are clean
names(Woody3) <- make.names(names(Woody3), unique = TRUE)

# Aggregate species counts by Site and Spp_name
species_cc <- Woody3 %>%
  group_by(Site, Spp_name) %>%
  summarise(Count = n()) %>%
  ungroup()
view(species_cc)

# Spread the data to have species as columns and sites as rows
species_matrix <- species_cc %>%
  spread(Spp_name, Count, fill = 0)

# View the prepared data
head(species_matrix)


# Remove the Site column for diversity calculation
species_matrix_for_diversity <- species_matrix %>%
  select(-Site)

# Ensure the data is numeric
species_matrix_for_diversity <- as.data.frame(lapply(species_matrix_for_diversity, as.numeric))

# Check for any remaining NAs
sum(is.na(species_matrix_for_diversity))

# Calculate Shannon-Wiener diversity index for each site
shannon_index <- diversity(species_matrix_for_diversity, index = "shannon")
view(shannon_index)

# Combine the Shannon index with site data
diversity_data <- data.frame(Site = species_matrix$Site, Shannon_Index = shannon_index)

# View the Shannon-Wiener diversity index data
print(diversity_data)

# Plot the Shannon-Wiener diversity index
ggplot(diversity_data, aes(x = Site, y = Shannon_Index, colour =)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Site",
    y = "Diversity indices"
  ) +
  theme_classic()
