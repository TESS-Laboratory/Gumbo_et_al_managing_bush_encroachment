library(tidyverse)
library(ggplot2)
# Install the readr package if not already installed
# install.packages("readr")

WoodyP <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/WoodyPlants2.csv")
head(WoodyP)

# Group by Site, Plot, Subplot, and calculate the mean Max_Height and sum of Stem_count
aggregated_data <- WoodyP %>%
  group_by(Site, Plot, Subplot) %>%
  summarise(
    mean_height = mean(Max_Height, na.rm = TRUE),
    total_stems = sum(Stem_count, na.rm = TRUE)
  )

##### Grouping according species 
species_grouped <- WoodyP %>%
  group_by(Site, Plot, Subplot, Spp_name) %>%
  summarise(
    mean_height = mean(Max_Height, na.rm = TRUE),
    total_stems = sum(Stem_count, na.rm = TRUE)
  )


# Remove duplicated rows based on Site, Plot, Subplot, and Spp_name
data_unique <- WoodyP %>%
  distinct(Site, Plot, Subplot, Spp_name, .keep_all = TRUE)

# Group again by Site, Plot, Subplot, and Spp_name after removing duplicates
species_grouped_unique <- data_unique %>%
  group_by(Site, Plot, Subplot, Spp_name) %>%
  summarise(
    mean_height = mean(Max_Height, na.rm = TRUE),
    total_stems = sum(Stem_count, na.rm = TRUE)
  )



####### GROUPING BY SPECIES IN Each subplot
# Group by Site, Plot, Subplot, and Spp_name to get the count of each species
species_summary <- WoodyP %>%
  group_by(Site, Plot, Subplot, Spp_name) %>%
  summarise(species_count = n()) %>%
  arrange(Site, Plot, Subplot)

# View the grouped data without duplicates
print(species_grouped_unique)
