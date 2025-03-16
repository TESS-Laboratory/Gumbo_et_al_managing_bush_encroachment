library(tidyverse)
library(vegan)
library(patchwork)
library(flextable)
library(officer)
library(openxlsx)
library(lme4)


data2 <- read_csv ("C:/workspace/gumbo_dev/DATA/Woody_sizeclasses.csv")
###arrange(Site, Plot, Subplot, desc(Count))
trees_comp <- data2 %>%
  filter(!is.na(Spp_name)) %>%  
  group_by(Site, Plot, Subplot, Spp_name) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Site, Plot, Subplot, desc(Count))  # Sort by count descending

top5_species <- trees_comp %>%
  group_by(Site, Plot, Subplot) %>%
  slice_max(Count, n = 5, with_ties = FALSE) %>%  # Get top 5 per subplot
  ungroup()

# Pivot the table to make species names columns
top5_pivot <- top5_species %>%
  pivot_wider(names_from = Spp_name, values_from = Count, values_fill = 0)

#############################################################
#############  WORKING ON DOMINANT TREE SPECIES ACROSS SUBPLOTS

TreeSubplot <- read_csv ("C:/workspace/gumbo_dev/DATA/Woody_sizeclasses.csv")


########################## SAPLINGS
 dataSP <- read_csv ("C:/workspace/gumbo_dev/DATA/Woody_sizeclasses.csv")

 saplings_comp <- dataSP %>%
   filter(!is.na(Saplings)) %>% 
   group_by(Site, Plot, Subplot, Spp_name) %>%
   summarise(Count = n(), .groups = "drop") %>%
   arrange(Site, Plot, Subplot, desc(Count))  # Sort by count descending
 
 top3_species <- saplings_comp %>%
   group_by(Site, Plot, Subplot) %>%
   slice_max(Count, n = 3, with_ties = FALSE) %>%  # Get top 5 per subplot
   ungroup()
 
 # Pivot the table to make species names columns
 top3_pivot <- top3_species %>%
   pivot_wider(names_from = Spp_name, values_from = Count, values_fill = 0)
 
 # Pivot the data to make species names as columns
 saplings_pivot <- saplings_comp %>%
   pivot_wider(names_from = Spp_name, values_from = Count, values_fill = 0)
 
### Save to Excel
 write.xlsx(saplings_pivot, file = "C:/workspace/gumbo_dev/Plots/Dominant T3SAplings_Species_SubplotPlot.xlsx")
 

 
 ################SEEDLINGS
 dataSD <- read_csv ("C:/workspace/gumbo_dev/DATA/Woody_sizeclasses.csv")
 
 seedlings_comp <- dataSD %>%
   filter(!is.na(Seedlings)) %>% 
   group_by(Site, Plot, Subplot, Spp_name) %>%
   summarise(Count = n(), .groups = "drop") %>%
   arrange(Site, Plot, Subplot, desc(Count))  # Sort by count descending
 
 top5_species <- seedlings_comp %>%
   group_by(Site, Plot, Subplot) %>%
   slice_max(Count, n = 5, with_ties = FALSE) %>%  # Get top 5 per subplot
   ungroup()
 
 # Pivot the table to make species names columns
 top5_pivot <- top5_species %>%
   pivot_wider(names_from = Spp_name, values_from = Count, values_fill = 0)
 
 # Pivot the data to make species names as columns
 seedlings_pivot <- seedlings_comp %>%
   pivot_wider(names_from = Spp_name, values_from = Count, values_fill = 0)
 
 ### Save to Excel
 write.xlsx(seedlings_pivot, file = "C:/workspace/gumbo_dev/Plots/Dominant SEEDlings_Species_SubplotPlot.xlsx")
 
 
 
 ##############################################################################################
 
 ### ANALYSIS AT SUBPLOT LEVEL ACROSS PLOTS
 
 # Read dataset from CSV file
 dataDC <- read.csv("C:/workspace/gumbo_dev/DATA/D.cinerea Subplot.csv")

 
 data2D <- dataDC %>%
   mutate(Subplot_ID = paste(Site, Plot, Subplot, sep = ""))
 
  # Summarize data at subplot level across sites and plots
 subplot_summary <- data2D %>%
   group_by(Subplot_ID) %>%
   summarise(
     Mean_Total = mean(Total, na.rm = TRUE),
     Median_Total = median(Total, na.rm = TRUE),
     Total_Count = sum(Total, na.rm = TRUE))
 
 #########################################
 
 data <- read_csv("C:/workspace/gumbo_dev/DATA/D.cinerea Subplot.csv")

 data$Site <- as.factor(data$Site)
 data$Plot <- as.factor(data$Plot)
 data$Subplot <- as.factor(data$Subplot)
 
  
 # Fit a mixed-effects model where Total count varies by Subplot, nested within Plot and Site
 
 model <- lmer(Total ~ (1 | Site/Plot/Subplot), data = data)
 summary(model)
 
 model <- lmer(Total ~ (1 | Site/Plot/Subplot), data = data)
 
 # View model summary
 summary(model)
 
 
 #################CREATING PLOTS FOR SPECIES ABSENT IN SUBPLOTS
 
 
 # Create dataset manually (if not reading from a CSV file)
 
 dataSP <- read_csv("C:/workspace/gumbo_dev/DATA/Spp & No. of subplots absent.csv")
 
 # Create the bar graph
 ggplot(dataSP, aes(x = Tree_species, y = Total_subplots_Absent)) +
   geom_bar(stat = "identity", fill = "steelblue") +
   #coord_flip() +  # Flips the graph for better readability
   theme_minimal() +
   labs(title = "Tree Species Absence Across Subplots",
        x = "Tree Species",
        y = "Number of Subplots Absent")
 
 