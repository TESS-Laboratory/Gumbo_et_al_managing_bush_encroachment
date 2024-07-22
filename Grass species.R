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
  labs(y = "No. of species") + theme_classic()+
  theme(axis.title.x = element_text(size = 30), 
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 28),  
        axis.text.y = element_text(size = 28)) 

