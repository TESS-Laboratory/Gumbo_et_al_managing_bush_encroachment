library(tidyverse)

#### Counting grass species
#Woody_species <- Woody_P %>%
 # group_by(Site) %>%
  #count(Spp_name)

Grass_species <- Grasses %>%
  group_by(Site) %>%
  count(Spp_name)

######### Number of grass spp per site
Grass_species_unique <- Grass_species %>%
  group_by(Site) %>%
  summarise(n_species = length(unique(Spp_name)))

##### Plotting graphs for grass species per site
ggplot(Grass_species_unique) +
  geom_col(aes(x = Site, y = n_species), fill = "lightgreen", colour = "darkgreen", width = 0.75) +
  labs(y = "No. of grass species") + theme_classic()


##### creating tables
data(Grass_species_unique)

#table <- data.frame(
 # Site = c("Site1", "Site2", "Site3", "Site4"),
  #Plot = c("Plot1", "Plot2", "Plot3", "Plot4"),
  #Species = c(25, 30, 20, 28)
#)
table <- data.frame(Site = c("A","B","C","D","E","F"), Species = c(27,25,22,25,19,25))
