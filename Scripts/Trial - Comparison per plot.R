##### coparing species composition at site and subplot  level

library(tidyverse)
library(vegan)
library(multcompView)

woody_species <- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Project development - Taps/R DATA/Woody species.csv")
head(woody_species)

# Summarize species composition
species_composition <- woody_species %>%
  group_by(Site, Subplot, Spp_name) %>%
  summarize(Count = n(), .groups = "drop")

# View the summarized data
head(species_composition)

#Create a species-by-subplot matrix
species_matrix <- table(woody_species$Subplot, woody_species$Spp_name)

# Calculate Jaccard similarity
jaccard_similarity <- vegdist(species_matrix, method = "jaccard")

# View similarity results
print(as.matrix(jaccard_similarity))


# Aggregate species count per subplot
species_summary <- species_composition %>%
  group_by(Site, Subplot) %>%
  summarize(Species_Richness = n(), .groups = "drop")

# Plot species richness per subplot
library(ggplot2)
ggplot(species_summary, aes(x = Subplot, y = Species_Richness, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Species Richness per Subplot by Site", x = "Subplot", y = "Species Richness") +
  theme_minimal()




#######COMPARING SPECIES LEVEL AT SITE, PLOT AND SUBPLOT LEVEL

# View the structure of the data
str(woody_species)

# Ensure columns are in the correct data types
woody_species <- woody_species %>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot),
         Subplot = as.factor(Subplot),
         Species = as.factor(Spp_name))

# Species count by Site, Plot, and Subplot
species_summary <- woody_species %>%
  group_by(Site, Plot, Subplot) %>%
  summarize(Species_Richness = n_distinct(Spp_name), .groups = "drop")

# View summary
head(species_summary)
tail(species_summary)

# Create species-by-subplot matrix
species_matrix <- table(paste(woody_species$Site, woody_species$Plot, woody_species$Subplot, sep = "_"), 
                        woody_species$Spp_name)

# Check the matrix
species_matrix[1:5, 1:5]

# Calculate Bray-Curtis dissimilarity
bray_curtis <- vegdist(species_matrix, method = "bray")

# Convert to matrix and view results
bray_curtis_matrix <- as.matrix(bray_curtis)
bray_curtis_matrix[1:5, 1:5]
tail(bray_curtis_matrix)


# Calculate Jaccard similarity
jaccard <- vegdist(species_matrix, method = "jaccard")

# Convert to matrix and view results
jaccard_matrix <- as.matrix(jaccard)
jaccard_matrix[1:5, 1:5]

# Perform NMDS
nmds <- metaMDS(species_matrix, distance = "bray", k = 2)

# Plot NMDS results
plot(nmds, type = "t", main = "NMDS of Species Composition")




##### Aggregate species richness
species_richness <- woody_species %>%
  group_by(Site, Plot, Subplot) %>%
  summarize(Richness = n_distinct(Spp_name), .groups = "drop")

# Plot species richness
ggplot(species_richness, aes(x = interaction(Plot, Subplot), y = Richness, fill = Site)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Species Richness by Plot and Subplot", x = "Plot_Subplot", y = "Richness")

######Save the summarized data for reporting or further analysis.
write_csv(species_summary, "species_composition_summary.csv", row.names = FALSE)
write_csv(as.data.frame(bray_curtis_matrix), "bray_curtis_dissimilarity.csv", row.names = TRUE)




#################### DETERMINING NUMBER OF SPECIES PER SITE, PLOT, SUBPLOT

# 
woody_species <- woody_species %>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot),
         Subplot = as.factor(Subplot),
         Species = as.factor(Spp_name))

# Count unique species by Site, Plot, and Subplot
species_count <- woody_species %>%
  group_by(Site, Plot, Subplot) %>%
  summarize(Species_Count = n_distinct(Spp_name), .groups = "drop")

# View summary
head(species_count)
tail(species_count)

# Bar plot for species count
ggplot(species_count, aes(x = interaction(Plot, Subplot), y = Species_Count, fill = Site)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Number of Species by Plot and Subplot in Each Site",
    x = "Plot_Subplot",
    y = "Number of Species"
  )

# Boxplot for species count
ggplot(species_count, aes(x = Site, y = Species_Count, fill = Site)) +
  geom_boxplot() +
  labs(
    title = "Species Count Distribution by Site",
    x = "Site",
    y = "Number of Species"
  )


#Boxplot 2 # Boxplot to show distribution of species count by Site
ggplot(species_count, aes(x = Site, y = Species_Count, fill = Site)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Species Count Across Sites",
    x = "Site",
    y = "Number of Species"
  ) +
  theme_minimal()

# Perform ANOVA
anova_result <- aov(Species_Count ~ Site + Plot + Subplot, data = species_count)

# Display ANOVA results
summary(anova_result)

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)

# Display Tukey's results
print(tukey_result)


# Save species count summary
write.csv(species_count, "species_count_summary.csv", row.names = FALSE)

# Save Tukey's HSD results
tukey_df <- as.data.frame(tukey_result$Subplot)
write.csv(tukey_df, "tukey_subplots.csv", row.names = TRUE)
# Save Tukey results to a CSV
write.csv(as.data.frame(tukey_result$Site), "tukey_sites.csv", row.names = TRUE)
write.csv(as.data.frame(tukey_result$Plot), "tukey_plots.csv", row.names = TRUE)
write.csv(as.data.frame(tukey_result$Subplot), "tukey_subplots.csv", row.names = TRUE)




#### Convert Tukey's results to a compact letter display for Site
tukey_site_letters <- multcompLetters4(anova_result, tukey_result)

# Add compact letter display to the Site factor
species_summary <- species_summary %>%
  mutate(Site_Group = tukey_site_letters$Site$Letters[Site])

# Plot with compact letter display
ggplot(species_count, aes(x = Site, y = Species_Count, fill = Site)) +
  geom_boxplot() + 
  #geom_text(aes(label = Site_Group), vjust = -0.5, color = "black") +
  labs(
    title = "Tukey's HSD Results for Site",
    x = "Site",
    y = "Number of Species"
  ) +
  theme_minimal()


######################################### DETERMINING WOODY PLANT HEIGHT


# Data types
woody_species <- woody_species %>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot),
         Subplot = as.factor(Subplot),
         Height = as.numeric(Max_Height))

# Summarize height statistics by Site, Plot, and Subplot
height_summary <- woody_species %>%
  group_by(Site, Plot, Subplot) %>%
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

# Bar plot for mean height
ggplot(height_summary, aes(x = interaction(Plot, Subplot), y = Mean_Height, fill = Site)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Mean Height by Plot and Subplot in Each Site",
    x = "Plot_Subplot",
    y = "Mean Height"
  )

### Boxplot for height distribution
#ggplot(woody_species, aes(x = interaction(Site, Plot, Subplot), y = Max_Height, fill = Site)) +
  #geom_boxplot() +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #labs(
   # title = "Height Distribution by Plot and Subplot in Each Site",
    #x = "Site_Plot_Subplot",
    #y = "Height"
  #)

#### ATTEMPT 2 
height_summary <- woody_species %>%
  group_by(Site, Plot) %>%
  summarize(Max_Height = mean(Max_Height, na.rm = TRUE), .groups = "drop")

ggplot(height_summary, aes(x = Plot, y = Max_Height, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Plot",
    y = "Max Height"
  )


# Perform ANOVA to compare heights
anova_result <- aov(Height ~ Site + Plot + Subplot, data = woody_species)

# Display ANOVA results
summary(anova_result)

print(anova_result)

####Tukey's Post-Hoc Test for Pairwise Comparisons

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)

# Display Tukey's results
print(tukey_result)



############################# DETERMINE SIMILARITY OR DISIMILARITY 

# Subset species data (exclude non-species columns like Site, Plot)
species_matrix <- woody_species[, -c(1:3)]  # Adjust based on your column indices

# Calculate Bray-Curtis dissimilarity matrix
bray_curtis <- vegdist(species_matrix, method = "bray")

# Inspect the matrix
bray_curtis
