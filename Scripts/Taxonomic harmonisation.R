library(WorldFlora)
library(tidyverse)
library(dplyr)
library(tidyr)



#load and clean data

Woody_species <- read_csv("DATA/Woody2_plants_species.csv")

# 
africa_species <- Woody_species %>% 
  distinct(species)   # one row per species

#
africa_species <- africa_species %>%
  filter(
    !str_detect(
      species,
      regex("^\\s*[^\\s]+\\s+(sp\\.?|spp\\.?)\\b", ignore_case = TRUE)
    )
  )



# Extract species names from the dataframe
species_list <- africa_species$species


# Load WFO backbone into memory
WFO.remember()  # assumes 'classification.csv' is in working directory

# (optional) Clean names to remove authorship or punctuation
# Only if names include authorship like "(L.) Willd."
prepared <- WFO.prepare(spec.data = species_list)
cleaned_names <- prepared$spec.name  # this will be used for matching

# Match cleaned species names with WFO backbone
matches <- WFO.match(spec.data = cleaned_names, WFO.data = WFO.data)


# Pick the best single match per species
best_matches <- WFO.one(matches)

# Combine harmonized names with original data
selected_columns <- c(
  "spec.name", 
  "scientificName", 
  "taxonomicStatus", 
  "New.accepted", 
  "scientificNameAuthorship", 
  "genus",
  "taxonRank",
  "family", 
  "taxonID",
  "references",
  "source"
)


harmonized_info <- best_matches[, selected_columns]



species_Harmonized <- cbind(africa_species, harmonized_info)


species_Harmonized <- species_Harmonized %>%
  filter(!is.na(genus))


# Save to file
write.csv(species_Harmonized, "DATA/Harmonized_WPspp.csv", row.names = FALSE)


write.csv(best_matches, "DATA/best_matches_WPspp.csv", row.names = FALSE)


# result summary

table(best_matches$Matched)  # answer = 95


table(best_matches$taxonomicStatus) #answer = 95


table(best_matches$New.accepted)


synonyms <- best_matches %>% filter(New.accepted == TRUE)
nrow(synonyms)


unmatched <- best_matches %>% filter(Matched == FALSE)
nrow(unmatched)


summary_table <- best_matches %>%
  group_by(taxonomicStatus, New.accepted) %>%
  summarise(count = n(), .groups = "drop")

print(summary_table)




# optional visualization
# Summary data
summary_data <- data.frame(
  Category = c(
    "Matched - Accepted",
    "Matched - Synonym Replaced",
    "Matched - Unchecked",
    "Unmatched"
  ),
  Count = c(1795, 126, 7, 9)
)

# Add percentages
summary_data <- summary_data %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1),
         Label = paste0(Percentage, "%"))

# Plot with percentage labels
ggplot(summary_data, aes(x = Category, y = Count)) +
  geom_bar(stat = "identity", fill = "#0072B2", width = 0.7) +
  geom_text(aes(label = Label), vjust = -0.5, size = 6) +
  labs(
    #title = "Taxonomic Harmonization Summary (WorldFlora)",
    y = "Number of species",
    x = "Taxonomic status"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1, size = 22),
    axis.text.y = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
  )


ggsave("Taxonomic_Harmonization.png", width = 16, height = 10, dpi = 300, bg="white")




################################################################################

## TAXONOMIC HARMONISATION FOR GRASS SPECIES

# load data and clean to remain with unique species
 #Grass_species <- read_csv("DATA/Harmonised Taxonomisation/Grasses_species.csv")

Grass_species <-read_csv("DATA/March2025/2Grasses2426.csv")

unique_species <- unique(Grass_species$Species_name)
unique_species

write.csv(unique_species, "DATA/Harmonised Taxonomisation/Grasses_UNIQUEspecies26.csv", row.names = FALSE)




#load and clean data

Grass_species <- read_csv("DATA/Harmonised Taxonomisation/Grasses_UNIQUEspecies26.csv")

# 
africa_species <- Grass_species %>% 
  distinct(Species_name)   # one row per species

#
africa_species <- Grass_species %>%
  filter(
    !str_detect(
      Species_name,
      regex("^\\s*[^\\s]+\\s+(sp\\.?|spp\\.?)\\b", ignore_case = TRUE)
    )
  )



# Extract species names from the dataframe
species_list <- africa_species$Species_name


# Load WFO backbone into memory
WFO.remember()  # assumes 'classification.csv' is in working directory

# (optional) Clean names to remove authorship or punctuation
# Only if names include authorship like "(L.) Willd."
prepared <- WFO.prepare(spec.data = species_list)
cleaned_names <- prepared$spec.name  # this will be used for matching

# Match cleaned species names with WFO backbone
matches <- WFO.match(spec.data = cleaned_names, WFO.data = WFO.data)


# Pick the best single match per species
best_matches <- WFO.one(matches)

# Combine harmonized names with original data
selected_columns <- c(
  "spec.name", 
  "scientificName", 
  "taxonomicStatus", 
  "New.accepted", 
  "scientificNameAuthorship", 
  "genus",
  "taxonRank",
  "family", 
  "taxonID",
  "references",
  "source"
)


harmonized_info <- best_matches[, selected_columns]



species_Harmonized <- cbind(africa_species, harmonized_info)


species_Harmonized <- species_Harmonized %>%
  filter(!is.na(genus))


# Save to file
write.csv(species_Harmonized, "DATA/hHarmonized2026b_GRspp.csv", row.names = FALSE)


#write.csv(best_matches, "DATA/best_matches_GRspp.csv", row.names = FALSE)


# result summary

table(best_matches$Matched)


table(best_matches$taxonomicStatus)


table(best_matches$New.accepted)


synonyms <- best_matches %>% filter(New.accepted == TRUE)
nrow(synonyms)


unmatched <- best_matches %>% filter(Matched == FALSE)
nrow(unmatched)


summary_table <- best_matches %>%
  group_by(taxonomicStatus, New.accepted) %>%
  summarise(count = n(), .groups = "drop")

