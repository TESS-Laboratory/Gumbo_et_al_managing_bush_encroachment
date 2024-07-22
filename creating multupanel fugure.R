############## CREATING MULTI-PANELS


## Load Packages
# install.packages("ggplot2")
# install.packages("patchwork")
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


