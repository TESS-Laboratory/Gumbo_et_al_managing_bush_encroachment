############## CREATING MULTI-PANELS


## Load Packages
 install.packages("ggplot2")
install.packages("patchwork")
# install.packages("jpeg")
# install.packages("grid")
# install.packages("cowplot")

library(tidyverse)
library(patchwork)
# library(jpeg)
# library(grid)
# library(cowplot)


##### add theme fancy######################## fancy !!!!!!


############## include in this script your analytical code to read in data 

########### generate the plot objects!



#### Function to read and convert JPEG to raster
# # Function to create a ggdraw object with a raster image and label
# raster_to_ggdraw <- function(file_path, label) {
#   img <- readJPEG(file_path)
#   ggdraw() +
#     draw_image(img) +
#     draw_label(label, x = 0.05, y = 0.95, hjust = 0, vjust = 1, size = 14, fontface = "bold", color = "black")
# }
# 
# # Convert to ggdraw objects with labels
# plot1 <- raster_to_ggdraw("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/Graphs and table RESULTS/WOODY species composition.jpeg", "(a)")
# plot2 <- raster_to_ggdraw("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/Graphs and table RESULTS/Results 2/Mean tree height.jpeg", "(b)")
# plot3 <- raster_to_ggdraw("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/Graphs and table RESULTS/Results 2/Stem count Density per ha.jpeg", "(c)")
# plot4 <- raster_to_ggdraw("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/Graphs and table RESULTS/Results 2/Plant DENSITY 1.jpeg", "(d)")


# Create a multi-panel layout using Patchwork
multi_panel_plot <- (plot1 + plot2) / (plot3 + plot4)

############################# include tag labels!##################

# Display the multi-panel plot
# print(multi_panel_plot)

ggsave("multi_panel_plot.jpg", multi_panel_plot, width=16, height=16, units=cm)

########################### TRYING NEW WAY

library(tidyverse)
library(patchwork)
library(ggplot2)

# Read the data from CSV files
# Assuming each script reads from a different CSV
data_species <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Woody spp only.csv")
data_height <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Cleaned folders/Cleaned - Woody2.csv")
data_diversity <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Woody spp only.csv")


### manually entered
plant_density <- data.frame(
  Site = c("A", "B", "C", "D", "E", "F"),
  Trees = c(1853, 1348, 2749, 2488, 2606, 1703),
  ##Stem_count = c(13512, 12008, 18337, 16543, 15108, 11432)
  Seedlings = c(1437, 1658, 1793, 1393, 2990, 825),
  Saplings = c(2413, 1074, 810, 1826, 1288, 1134)
)


# Analysis and Plot for Plant Height
# Replace with your script logic for determining plant height
plot_species <- ggplot(data_species, aes(x = Site, y = Spp_name)) +
geom_point() +
theme_minimal() +
labs(x = "Site", y = "Spp_Name")


 #Analysis and Plot for Diversity
 #Replace with your script logic for determining diversity
plot_diversity <- ggplot(data_diversity, aes(x = Site, y = diversity)) +
 geom_point() +
  theme_minimal() +
  labs(x = "Site", y = "Diversity indices")

# Analysis and Plot for Canopy Cover
# Replace with your script logic for determining canopy cover
plot_height <- ggplot(data_height, aes(x = Site, y = Max_Height..m.)) +
  geom_point() 

  
