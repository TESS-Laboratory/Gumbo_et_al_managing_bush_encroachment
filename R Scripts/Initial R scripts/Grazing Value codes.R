####

library(dplyr)
library(tidyverse)
####
data <- read_excel("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/jUNK DATA/2.2-GRASSES sheet.xlsx")

####
data <- data %>%
  filter(!is.na(Grazing_value)) %>%
  mutate(Grazing_value = tolower(Grazing_value)) %>%
  mutate(Grazing_value = case_when(
    Grazing_value %in% c("high") ~ "High",
    Grazing_value %in% c("moderate", "modeerate") ~ "Moderate",
    TRUE ~ "Low"
  ))

###
table(data$Grazing_value)

###
ggplot(data, aes(x = Grazing_value, fill = Grazing_value)) +
  geom_bar(stat = "count") +
  labs(title = "Grazing Value Distribution", x = "Grazing Value", y = "Frequency") +
  scale_fill_manual(values = c("High" = "red", "Moderate" = "blue", "Low" = "green")) +
  theme_minimal()





#combining
# ##

library(dplyr)
library(ggplot2)
library(stringr)

####
data <- read_excel("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/jUNK DATA/2.2-GRASSES sheet.xlsx")

#####
data <- data %>%
  filter(!is.na(Grazing_value), !is.na(Site)) %>%
  mutate(Site = str_replace_all(Site, "\\.", "")) %>%
  mutate(Grazing_value = tolower(Grazing_value)) %>%
  mutate(Grazing_value = case_when(
    Grazing_value %in% c("high") ~ "High",
    Grazing_value %in% c("moderate", "modeerate") ~ "Moderate",
    TRUE ~ "Low"
  ))

###Site
sites <- unique(data$Site)

#####
print(paste("Number of unique sites:", length(sites)))
print("Sites names:")
print(sites)

###个Site
for (site in sites) {
  site_data <- filter(data, Site == site)
  
  ####
  print(paste("Data points for site", site, ":", nrow(site_data)))
  
  if (nrow(site_data) > 0) {
    p <- ggplot(site_data, aes(x = Grazing_value, fill = Grazing_value)) +
      geom_bar(stat = "count") +
      labs(title = paste("Grazing Value Distribution at Site", site),
           x = "Grazing Value", y = "Frequency") +
      scale_fill_manual(values = c("High" = "green", "Moderate" = "blue", "Low" = "red")) +
      theme_minimal()
    
    print(p)
  } else {
    print(paste("No data available for site", site))
  }
}

#######Combing grazing value across all sites
p_summary <- ggplot(data, aes(x = Site, fill = Grazing_value)) +
  geom_bar(position = "dodge") +
  labs(x = "Site", y = "Frequency") +
  scale_fill_manual(values = c("High" = "green", "Moderate" = "blue", "Low" = "red")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.5))

print(p_summary)











###dividing


library(tidyverse)
library(dplyr)


#
data <- read_excel("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/jUNK DATA/2.2-GRASSES sheet.xlsx")

# 
data <- data %>%
  filter(!is.na(Grazing_value), !is.na(Site)) %>%
  mutate(Grazing_value = tolower(Grazing_value)) %>%
  mutate(Grazing_value = case_when(
    Grazing_value %in% c("high") ~ "High",
    Grazing_value %in% c("moderate", "modeerate") ~ "Moderate",
    TRUE ~ "Low"
  ))

#Site
sites <- unique(data$Site)

###
print(paste("Number of unique sites:", length(sites)))
print("Sites names:")
print(sites)

####个Site
for (site in sites) {
  site_data <- filter(data, Site == site)
  
  ###
  print(paste("Data points for site", site, ":", nrow(site_data)))
  
  if (nrow(site_data) > 0) {
    p <- ggplot(site_data, aes(x = Grazing_value, fill = Grazing_value)) +
      geom_bar(stat = "count") +
      labs(title = paste("Grazing value across sites", site),
           x = "Grazing Value", y = "Frequency") +
      scale_fill_manual(values = c("High" = "red", "Moderate" = "blue", "Low" = "green")) +
      theme_minimal()
    
    print(p)
  } else {
    print(paste("No data available for site", site))
  }
}


###################GRASS HEIGHT
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the data
data <- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Desktop/Grass height.csv")

# Summarize DPM_Height by Site
summary_data <- data %>%
  group_by(Site) %>%
  summarize(
    mean_height = mean(DPM_Height, na.rm = TRUE),
    sd_height = sd(DPM_Height, na.rm = TRUE),
    count = n()
  )

# Print summary
print(summary_data)

########GRASS HEIGHT REVISED TO REMOVE NA OR EMPTY

data <- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Desktop/Grass height.csv")

# View the data to identify the correct column
head(data)

# Assuming the column to use for DPM_Height is the second one, correctly labeled here
# If it's labeled NaN, use the actual name of the column or index to extract it
# Assuming it's the third column from your example
colnames(data)[5] <- "DPM_Height"

# Remove rows with NA or empty values in Site
data <- data %>% filter(!is.na(Site) & Site != "")

# Convert DPM_Height from millimeters to centimeters if needed
# Uncomment if conversion is necessary
data$DPM_Height <- data$DPM_Height / 10

# Summarize DPM_Height by Site
summary_data <- data %>%
  group_by(Site) %>%
  summarize(
    mean_height = mean(DPM_Height, na.rm = TRUE),
  )

# Print summary
print(summary_data)

# Create a boxplot to visualize DPM_Height grouped by Site
ggplot(data, aes(x = Site, y = DPM_Height)) +
  geom_col() +
  labs(
    x = "Site",
    y = "DPM Height (cm)")+
  theme_classic()+
  theme(axis.title.x = element_text(size = 32), 
        axis.title.y = element_text(size = 32),
        axis.text.x = element_text(size = 30),  
        axis.text.y = element_text(size = 30)) 




#####Calculating species diversity (SHANON-WIENER INDICES)
# Load the data from the Excel file
# Replace "/path/to/your/excel/file.xlsx" with the actual path to your Excel file
data <- read_excel("/path/to/your/excel/file.xlsx")

# View the first few rows of the data
head(data)
# Remove rows with NA or empty values in Site
data <- data %>% filter(!is.na(Site) & Site != "")

# Remove the Site column for analysis
species_data <- data[,-1]

# View the prepared data
head(species_data)
# Calculate Shannon-Wiener diversity index for each site
shannon_index <- diversity(species_data, index = "shannon")

# Combine the Shannon index with site data
diversity_data <- data.frame(Site = data$Site, Shannon_Index = shannon_index)

# View the Shannon-Wiener diversity index data
print(diversity_data)
# Plot the Shannon-Wiener diversity index
ggplot(diversity_data, aes(x = Site, y = Shannon_Index, fill = Site)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Shannon-Wiener Diversity Index by Site",
    x = "Site",
    y = "Shannon-Wiener Diversity Index"
  ) +
  theme_minimal()

# Plot other indices if desired
# For example, plotting Simpson's index
ggplot(diversity_data, aes(x = Site, y = Simpson_Index, fill = Site)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Simpson's Diversity Index by Site",
    x = "Site",
    y = "Simpson's Diversity Index"
  ) +
  theme_minimal() + theme(axis.title.x = element_text(size = 32), 
                        axis.title.y = element_text(size = 32),
                        axis.text.x = element_text(size = 30),  
                        axis.text.y = element_text(size = 30)) 

