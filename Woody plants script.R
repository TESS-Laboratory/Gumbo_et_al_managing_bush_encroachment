library(tidyverse)


Woody <- Woody[2:nrow(Woody), ]

Woody_species_count <- Woody %>%
  group_by(Site, Plot, Subplot) %>%
  count(Spp_name)

  
  
Woody_species_count <- Woody %>%
  group_by(Site) %>%
  count(Spp_name)
 
Woody_species_unique <- Woody %>%
  group_by(Site) %>%
  summarise(n_species = length(unique(Spp_name)))

ggplot(Woody_species_unique) +
  geom_col(aes(x = Site, y = n_species), fill = "lightgreen", colour = "darkgreen", width = 1) +
  labs(y = "Number of woody species") +
  theme_classic()

