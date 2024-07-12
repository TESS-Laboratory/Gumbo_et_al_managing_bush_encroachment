library(tidyverse)
library(dplyr)
library(ggplot2)
####group data by site and subplot
##df_grouped <- df %>%
  #group_by(Site, Plot) %>%
  #summarise(
   # Mean_Variable1 = mean(Variable1),
    #Median_Variable2 = median(Variable2),
    # Add more summary statistics as needed
  #)




#### Counting woody species
Woody_species <- Woody_P %>%
  group_by(Site) %>%
  count(Spp_name)

###### Number of woody plant spp per site
Woody_species_unique <- Woody_P %>%
  group_by(Site) %>%
  summarise(n_species = length(unique(Spp_name)))

##### plotting graphs for species count per site
#ggplot(Woody_species_unique) +
#geom_col(aes(x = Site, y = n_species), fill = "lightgreen", colour = "darkgreen", width = 1) +
 # labs(y = "Number of woody species") +
  #theme_classic()
ggplot(Woody_species_unique) + 
  geom_col(aes(x = Site, y = n_species), fill = "lightgreen", colour = "darkgreen", width = 0.75) +
  labs(y = "No. of woody species") + theme_classic()



