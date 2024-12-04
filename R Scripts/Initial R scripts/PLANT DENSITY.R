#################CALCULATING WOODY DENSITY CLEANED DATA AND OLD 
# Load necessary libraries
library(tidyverse)
library(reshape2)

# Read the data (assuming it's a CSV file, you need to convert the image data to CSV first)
Plantheight <- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Cleaned folders/Cleaned - Woody2.csv", stringsAsFactors = FALSE)

# Define height classes
Plantheight <- Plantheight %>%
  mutate(Height_Class = cut(Max_Height,
                            breaks = c(0, 0.5, 1.49, 30),
                            labels = c("0 - 0.5", "0.51 - 1.49", "1.5 - 30"),
                            right = FALSE))

# Group by Site and Height_Class, and count the number of occurrences
groupeddata <- Plantheight %>%
  group_by(Site, Height_Class) %>%
  summarise(Count = n())

###############################################################

##### OLD DATA FOR WOODY PARAMETER
# Create the data frame
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Given data
plant_height2 <- data.frame(
  Site = c("A", "B", "C", "D", "E", "F"),
  ##Trees = c(1853, 1348, 2749, 2488, 2606, 1703),
  Stem_count = c(13512, 12008, 18337, 16543, 15108, 11432)
  ##Seedlings = c(1437, 1658, 1793, 1393, 2990, 825),
  ##Saplings = c(2413, 1074, 810, 1826, 1288, 1134)
)

# Define the plot area in square meters
plot_area_sqm <- 12000

# Convert the plot area to hectares
plot_area_hectares <- plot_area_sqm / 10000

# Calculate the density per hectare for each category
plant_height2 <- plant_height2 %>%
  mutate(
    #####Trees_Density = Trees / plot_area_hectares,#####REMOVING TREES
    Stem_count_Density = Stem_count / plot_area_hectares,
    ###Seedlings_Density = Seedlings / plot_area_hectares##,
    ##Saplings_Density = Saplings / plot_area_hectares
  )

# Print the data to verify calculations
print(plant_height2)

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
