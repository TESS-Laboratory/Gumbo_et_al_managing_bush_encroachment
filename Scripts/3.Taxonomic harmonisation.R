library(WorldFlora)
library(tidyverse)
library(dplyr)
library(tidyr)



#load and clean data
Woody <- read_csv("DATA/March2026/WOODYP2426OG.csv")

# unique species
unique_species <- unique(Woody$Species_name)
unique_species

# saving unique woody species
 write.csv(unique_species, "DATA/Harmonised Taxonomisation/Woody_UNIQUEspecies26.csv", row.names = FALSE)


 #loading data of unique species
 Woody_species <- unique_species
 
 # checking african species 
 africa_species <- unique(Woody_species)
 
 #
 africa_species <- africa_species[
   !str_detect(
     africa_species,
     regex("^\\s*[^\\s]+\\s+(sp\\.?|spp\\.?)\\b", ignore_case = TRUE)
   )
 ]
 
 # Extracting species names from the dataframe
 species_list <- africa_species


# Loading WFO backbone into memory
WFO.remember()  # assumes 'classification.csv' is in working directory

# (optional) Clean names to remove authorship or punctuation
# Only if names include authorship like "(L.) Willd."
prepared <- WFO.prepare(spec.data = species_list)
cleaned_names <- prepared$spec.name  # this will be used for matching

# Matching cleaned species names with WFO backbone
matches <- WFO.match(spec.data = cleaned_names, WFO.data = WFO.data)


# Picking the best single match per species
best_matches <- WFO.one(matches)

# Combining harmonised names with original data
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


harmonised_info <- best_matches[, selected_columns]



species_Harmonised <- cbind(africa_species, harmonised_info)


species_Harmonised <- species_Harmonised %>%
  filter(!is.na(genus))


# Save to file
#write.csv(species_Harmonised, "DATA/Harmonised_WPspp26.csv", row.names = FALSE)


#write.csv(best_matches, "DATA/best_matches_WPspp.csv", row.names = FALSE)


# Result summary

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

print(summary_table)


################################################################################

## TAXONOMIC HARMONISATION FOR GRASS SPECIES

# loading data and clean to remain with unique species

Grass_species <-read_csv("DATA/March2026/Grasses2426OG.csv")


# extracting unique species
unique_species <- unique(Grass_species$Species_name)
unique_species

#write.csv(unique_species, "DATA/Harmonised Taxonomisation/Grasses_UNIQUEspecies26.csv", row.names = FALSE)


#loading data

Grass_species <- Gunique_species

# checking african species 
africa_species <- unique(Grass_species)

#
africa_species <- africa_species[
  !str_detect(
    africa_species,
    regex("^\\s*[^\\s]+\\s+(sp\\.?|spp\\.?)\\b", ignore_case = TRUE)
  )
]

# Extracting species names from the dataframe
species_list <- africa_species
species_list
# Cleaning the species_list: removing NA 
species_list_clean <- species_list[!is.na(species_list) & species_list != ""]


# (optional) Clean names to remove authorship or punctuation
# Only if names include authorship like "(L.) Willd."
prepared <- WFO.prepare(spec.data = species_list_clean)
cleaned_names <- prepared$spec.name  # this will be used for matching

# Matching cleaned species names with WFO backbone
matches <- WFO.match(spec.data = cleaned_names, WFO.data = WFO.data)


# Picking the best single match per species
best_matches <- WFO.one(matches)

# Combining harmonised names with original data
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


Gharmonised_info <- best_matches[, selected_columns]

Gspecies_Harmonised <- tibble(Species_name = africa_species) %>%
  left_join(Gharmonised_info, by = c("Species_name" = "spec.name"))

Gspecies_Harmonised <- Gspecies_Harmonised %>%
  filter(!is.na(genus))


# Save to file
write.csv(Gspecies_Harmonised, "DATA/Harmonised Taxonomisation/Harmonised2026b_GRspp.csv", row.names = FALSE)


