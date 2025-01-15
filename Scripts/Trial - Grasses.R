library(tidyverse)
library(patchwork)
library(vegan)
library(multcompView)

Grassspecies<- read_csv("C:/workspace/gumbo_dev/DATA/GrassesN.csv")

Grasssp <- read_csv("C:/workspace/gumbo_dev/DATA/GrassesN.csv")

# Summarize species composition
species_composition <- Grasssp %>%
  group_by(Site,Plot, Subplot, Quadrat, Spp_name) %>%
  summarize(Count = n(), .groups = "drop")

# View the summarized data
head(species_composition)

#Create a species-by-subplot matrix
species_matrix <- table(Grasssp$Site, Grasssp$Plot, Grasssp$Subplot,Grasssp$Quadrat, Grasssp$Spp_name)

# Calculate Jaccard similarity
jaccard_similarity <- vegdist(species_matrix, method = "jaccard")

# View similarity results
print(as.matrix(jaccard_similarity))



# Aggregate species count per subplot
species_summary <- species_composition %>%
  group_by(Site, Plot, Subplot, Quadrat) %>%
  summarize(Species_Richness = n(), .groups = "drop")


# Plot species richness per subplot
ggplot(species_summary, aes(x = Quadrat, y = Species_Richness, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Species Richness per Subplot by Site", x = "Subplot", y = "Species Richness") +
  theme_minimal()



