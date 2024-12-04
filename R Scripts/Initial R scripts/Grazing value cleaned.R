############################ GRASS GRAZING VALUE CLEANED DATA
library(tidyverse)

GrazingVC <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Cleaned folders/Grazing value only.csv")
head(GrazingVC)

   
 # Trim whitespace from all character columns
 GrazingVC <- GrazingVC %>%
   mutate(across(where(is.character), trimws))
 
 # Remove rows with any NA values
 GrazingVC <- na.omit(GrazingVC)
 
 # Print the first few rows after cleaning
 print(head(GrazingVC))
 
 # Convert columns to factors if needed
 GrazingVC$Site <- as.factor(GrazingVC$Site)
 GrazingVC$Grazing_value <- as.factor(GrazingVC$Grazing_value)
 
 # Print the structure of the data to verify
 ##str(GrazingVC)
 
 # Define custom colors for the Grazing_value levels
 custom_colors <- c("High" = "darkgreen", "Moderate" = "lightgreen", "Low" = "orange")
 
 # Create a bar plot with custom colors
 plot <- ggplot(GrazingVC, aes(x = Site, fill = Grazing_value)) +
   geom_bar(position = "dodge") +
   labs(x = "Site",
        y = "Count",
        fill = "Grazing Value") +
   scale_fill_manual(values = custom_colors) +
   theme_classic() + theme(axis.title.x = element_text(size = 14), 
                        axis.title.y = element_text(size = 14), 
                        axis.text.x = element_text(size = 12),  
                        axis.text.y = element_text(size = 12))
 
 # Print the plot
 print(plot)
 
       