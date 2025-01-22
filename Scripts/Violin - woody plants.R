
library(tidyverse)
library(vegan)
library(multcompView)

woody_species <- read.csv("C:/workspace/gumbo_dev/DATA/WoodyPlants_C.csv")

theme_beautiful <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 8, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 8,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 8, color = "black"),
      legend.title = element_text(size = 8, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
}


# Data types
woody_species <- woody_species%>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot),
         Subplot = as.factor(Subplot),
         Height = as.numeric(Max_Height))

# Summarize height statistics by Site, Plot, and Subplot
height_summary <- woody_species %>%
  group_by(Site, Plot) %>%
  summarize(
    Mean_Height = mean(Max_Height, na.rm = TRUE),
    Median_Height = median(Max_Height, na.rm = TRUE),
    Min_Height = min(Max_Height, na.rm = TRUE),
    Max_Height = max(Max_Height, na.rm = TRUE),
    .groups = "drop"
  )

# View summary
head(height_summary)
tail (height_summary)


# Create the boxplot at Site and Plot level
ggplot(woody_species, aes(x = interaction(Site, Plot), y = Max_Height, fill = Site)) +
  geom_boxplot() +
  labs(
    x = "Site and Plot",
    y = "Woody plants height"
  ) +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8))


# Create the violin plot at site level
ggplot(woody_species, aes(x = Site, y = Max_Height)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site",
    y = "Woody plants height"
  ) +
  theme_beautiful()
  

##Saving plot in folder Plots
output_folder <- "C:/workspace/gumbo_dev/Plots"  

# Define the file path
output_file <- file.path(output_folder, "Violin plot woody plants height.jpeg")

# Save the last plot
ggsave(output_file)



# Create the violin plot at Site and Plot level
ggplot(woody_species, aes(x = interaction(Site, Plot), y = Max_Height)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site and Plot",
    y = "Max_Height"
  ) +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) 
