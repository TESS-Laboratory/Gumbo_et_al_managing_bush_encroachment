### Box and whisker woody plants
library(tidyverse)
# Load the ggplot2 package
library(ggplot2)
library(patchwork)

# Load the data
#data <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Cleaned folders/Cleaned - Woody2.csv")

data <- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Cleaned folders/Cleaned - Woody2.csv")

# Create the box-and-whisker plot for Mean tree height
b <- ggplot(data, aes(x = Site, y = Max_Height)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
 labs(
       x = "Site",
       y = "Mean tree height (m)") + theme_classic()+
  plot_annotation("(b)")


##################
#### Counting woody species
Woody_species <- data %>%
  group_by(Site) %>%
  count(Spp_name)

###### Number of woody plant spp per site
Woody_species_unique <- data %>%
  group_by(Site) %>%
  summarise(n_species = length(unique(Spp_name)))

a <- ggplot(Woody_species_unique) + 
  geom_col(aes(x = Site, y = n_species), fill = "grey", colour = "grey", width = 0.75) +
  labs(y = "Woody species") + theme_classic()+
  plot_annotation("(a)")
a
####### Woody plant parameters
# Create the data frame
woodyparameters <- data.frame(
  Site = c("A", "B", "C", "D", "E", "F"),
  Seedlings = c(1724, 1990, 2151, 1671, 3588, 990),
  Saplings = c(2896, 1289, 972, 2191, 1545, 1361),
  Trees = c(2224, 1618, 3299, 2985, 3127, 2044))

# Convert the data to long format for ggplot
data_long <- reshape2::melt(woodyparameters, id.vars = "Site")

# Plot the data
c <- ggplot(data_long, aes(x = Site, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Site",
    y = "Density per hectare",
    fill = "Legend"
  ) +
  theme_classic()+
  plot_annotation("(c)")
#################################
combinedplot <- (b|a|c)+
  plot_annotation(tag_levels = "a")
  
combinedplot

#################
multi_panel_plot <- (a|b|c)+
  plot_annotation(tag_levels = 'a') +
  theme(plot.tag.position = c(0, 1), plot.tag = element_text(size = 0.5, hjust = -0.5))
multi_panel_plot


########################################################################################################
#######################################################################################################

GRASSSES 

Grassspp<- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grasses spp only.csv")


#Grassspp<- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grasses spp only.csv")

summary(Grassspp)

#### Counting woody species
Grass_species <- Grassspp %>%
  group_by(Site) %>%
  count(Spp_name)

###### Number of woody plant spp per site
Grass_species_unique <- Grassspp %>%
  group_by(Site) %>%
  summarise(n_species = length(unique(Spp_name)))

a <- ggplot(Grass_species_unique) + 
  geom_col(aes(x = Site, y = n_species), fill = "grey", colour = "grey", width = 0.75) +
  labs(y = "No. of grass species") + theme_classic()+
  plot_annotation("(a)")


Grasssheight <- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grasses height.csv")
ggplot(Grasssheight, aes(x = Site, y = DPM.Height )) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site",
    y = "Mean grass height (cm)") + theme_classic()+
  plot_annotation("(b)")


DataGspp <- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grasses spp only.csv", stringsAsFactors = FALSE)
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
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site",
    y = "Diversity index") + theme_classic()+
  plot_annotation("(c)")
  
   
ggplot(diversity_index, aes(x = Site, y = DiversityIndex)) +
geom_bar(stat = "identity", fill = "grey") +
  theme_classic() +
  labs(
       x = "Site",
       y = "Diversity Index") +
  theme(plot.title = element_text(hjust = 0.5))+
  plot_annotation("(c)")


#geom_boxplot(fill = "lightblue", color = "darkblue") +
 # labs(
  #  x = "Site",
   # y = "Diversity index") + theme_classic()+
  #plot_annotation("(c)")



CREATING MULTI-PANEL FOR GRASSES
# Example data for plotting
#data <- read.csv("path/to/your/file.csv")

# Plot 1: Boxplot of Max Height by Site

Grass_species_unique <- Grassspp %>%
  group_by(Site) %>% summarise(n_species = length(unique(Spp_name)))
plot1 <- ggplot(Grass_species_unique) + 
geom_col(aes(x = Site, y = n_species), fill = "grey", colour = "grey", width = 0.75) +
  labs(title = "(a)") + theme_classic()


  #plot_annotation("(a)")
  
# Plot 2: Bar plot of Species Count by Site
species_count <- data %>%
  group_by(Site) %>%
  summarise(Count = n_distinct(Spp_name))

plot2 <- ggplot(species_count, aes(x = Site, y = Count)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(title = "(b) Species Count by Site") +
  theme_minimal()

# Plot 3: Scatter plot of Max Height vs Stem Count
plot3 <- ggplot(data, aes(x = Stem_count, y = Max_Height)) +
  geom_point(color = "purple") +
  labs(title = "(c) Max Height vs Stem Count") +
  theme_minimal()






############################### Creating multi-panel using images

# Install packages if necessary
install.packages("cowplot")
install.packages("magick")  # For reading images

# Load necessary libraries
library(cowplot)
library(magick)

# Load images
img1 <- image_read("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/Nov-Woody & Grass Results/Grasses/Grass spp.jpeg")
img2 <- image_read("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/Nov-Woody & Grass Results/Grasses/grass height.jpeg")
img3 <- image_read("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/Nov-Woody & Grass Results/Grasses/grass diversity index.jpeg")

print(img1)
# Convert magick images to ggplot objects
plot1 <- ggdraw() + draw_image(img1)
plot2 <- ggdraw() + draw_image(img2)
plot3 <- ggdraw() + draw_image(img3)

# Arrange the images into a multi-panel layout with labels
multi_panel <- plot_grid(
  plot1 + ggtitle("(a)"),
  plot2 + ggtitle("(b)"),
  plot3 + ggtitle("(c)"),
  ncol = 1,
  align = 'v'
)

print(multi_panel)
# Save the output
ggsave("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/Nov-Woody & Grass Results"), multi_panel, width = 10, height = 15)

#Solving blurry images
# Convert images to ggplot-compatible objects with slight scaling adjustments

# Load images
img1 <- image_read("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/Nov-Woody & Grass Results/Grasses/Grass spp.jpeg")
img2 <- image_read("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/Nov-Woody & Grass Results/Grasses/grass height.jpeg")
img3 <- image_read("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/Nov-Woody & Grass Results/Grasses/grass diversity index.jpeg")

plot1 <- ggdraw() + draw_image(img1, scale = 1.1) #+ ggtitle("(a)")
plot2 <- ggdraw() + draw_image(img2, scale = 1.1) #+ ggtitle("(b)")
plot3 <- ggdraw() + draw_image(img3, scale = 1.1) #+ ggtitle("(c)")

# Arrange the images in a multi-panel layout
multi_panel <- plot_grid(
  plot1,
  plot2,
  plot3,
  ncol = 1,
  align = 'v'
)

# Save the high-resolution multi-panel plot
ggsave("/mnt/data/multi_panel_figure_high_res.png", multi_panel, width = 10, height = 15, dpi = 300)

# Optional: Print to plot viewer in RStudio
print(multi_panel)

#######################################################################################################################
##################################################### MULTI-PANEL ATTEMPT 2

library(ggplot2)
library(dplyr)
library(patchwork)

Grass_species_unique <- Grassspp %>%
  group_by(Site) %>%
  summarise(n_species = length(unique(Spp_name)))

# Create the bar plot (plot a)
plot_a <- ggplot(Grass_species_unique) + 
  geom_col(aes(x = Site, y = n_species), fill = "grey", colour = "grey", width = 0.75) +
  labs(y = "No. of grass species") +
  theme_classic() + # Increase the base font size here
  theme(plot.title = element_text(size = 16),  # Adjust title font size
        axis.title = element_text(size = 14),  # Axis title font size
        axis.text = element_text(size = 14))+
  ggtitle("(a)") #+theme(plot.title = element_text(hjust = -0.1),axis.title.y = element_text(size = 10), 
                          #axis.title.x = element_text(size = 10))


plot_b <- ggplot(Grasssheight, aes(x = Site, y = DPM.Height)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(x = "Site", y = "Mean grass height (cm)") + 
  theme_classic() + # Increase the base font size here
  theme(plot.title = element_text(size = 16),  # Adjust title font size
        axis.title = element_text(size = 14),  # Axis title font size
        axis.text = element_text(size = 14))+
  ggtitle("(b)") #theme(plot.title = element_text(hjust = -0.1),axis.title.y = element_text(size = 10), 
                         #axis.title.x = element_text(size = 10))


plot_c <- ggplot(diversity_index, aes(x = Site, y = DiversityIndex)) +
  geom_point(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site",
    y = "Diversity index")+theme_classic()+ # Increase the base font size here
  theme(plot.title = element_text(size = 16),  # Adjust title font size
        axis.title = element_text(size = 14),  # Axis title font size
        axis.text = element_text(size = 14))+
  ggtitle("(c)") 


# Combine the plots in a single layout
multi_panel <- plot_a/ (plot_b|plot_c)  # "/" for stacking vertically, or "|" for side-by-side

# Display the multi-panel plot
print(multi_panel)

# Save the multi-panel plot with high resolution
ggsave("multi_panel_figure5.png", multi_panel, width = 10, height = 12, dpi = 300)



################################################################################## MULTI-PANEL TREES

data <- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Cleaned folders/Cleaned - Woody2.csv")

plot1 <- ggplot(Woody_species_unique) + 
  geom_col(aes(x = Site, y = n_species), fill = "grey", colour = "grey", width = 0.75) +
  labs(y = "No. of woody species") + theme_classic()+ # Increase the base font size here
  theme(plot.title = element_text(size = 16),  # Adjust title font size
        axis.title = element_text(size = 14),  # Axis title font size
        axis.text = element_text(size = 14))+
  ggtitle("(a)")


# Plot 3: Scatter plot of Max Height vs Stem Count
#plot2 <- ggplot(data, aes(x = Stem_count, y = Max_Height)) +
 # geom_point(color = "purple") +
  #labs(title = "(c) Max Height vs Stem Count") +
  #theme_minimal+ 
  #ggtitle("(b)")
  
  plot2 <- data <- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Cleaned folders/Cleaned - Woody2.csv")
  
  # Create the box-and-whisker plot for Mean tree height
  plot2 <- ggplot(data, aes(x = Site, y = Max_Height)) +
    geom_boxplot(fill = "lightblue", color = "darkblue") +
    labs(
      x = "Site",
      y = "Mean tree height (m)") + theme_classic()+ # Increase the base font size here
    theme(plot.title = element_text(size = 16),  # Adjust title font size
          axis.title = element_text(size = 14),  # Axis title font size
          axis.text = element_text(size = 14))+
  ggtitle("(b)")
  
  plot3 <- ggplot(data_long, aes(x = Site, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      x = "Site",
      y = "Density per hectare",
      fill = "Legend"
    ) +
    theme_classic()+ # Increase the base font size here
    theme(plot.title = element_text(size = 16),  # Adjust title font size
          axis.title = element_text(size = 14),  # Axis title font size
          axis.text = element_text(size = 14))+
    ggtitle("(c)")
  
## Creating a legend to show the encroachment levels
  legend_data <- data.frame(
    Site = c("C", "D", "E"),
    Encroachment = factor("Heavily encroached", levels = c("Heavily encroached"))
  )
  
  plot_legend <- ggplot() +
    theme_void() +
    #ggtitle("(c)") +
    annotate("text", x = 1, y = 1, label = "Site C,D,E: Heavily encroached", color = "black", hjust = 0, size = 5)
  
  
  # Combine the plots in a single layout
  multi_panel <- plot1/ (plot2|plot3)/plot_legend + plot_layout(ncol = 1, heights = c(2, 2, 1))  # "/" for stacking vertically, or "|" for side-by-side
  
  # Display the multi-panel plot
  print(multi_panel)
  
  # Save the multi-panel plot with high resolution
  ggsave("multi_panel_WOODY plants updated2.png", multi_panel, width = 10, height = 12, dpi = 300)
  
  
  ################################################################################################################
  
####CATTLE POPULATION AT SHR
  
  
  library(ggplot2)
  
  # Create the data frame
  cattle_data <- data.frame(
    year = as.factor(c(1930, 1941, 1944, 1980)),  # Convert years to factor for categorical x-axis
    cattle_population = c(22000, 14600, 16400, 30000)
  )
  
  # Create a box plot
  ggplot(cattle_data, aes(x = year, y = cattle_population)) +
    geom_point(fill = "lightblue", color = "darkblue") +
    labs(
      x = "Year",
      y = "Cattle numbers"
    ) +
    theme_classic() +theme(plot.title = element_text(size = 16),  # Adjust title font size
                          axis.title = element_text(size = 14),  # Axis title font size
                          axis.text = element_text(size = 14))
