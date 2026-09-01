library(WorldFlora)
library(tidyverse)
library(dplyr)
library(tidyr)



#load and clean data
Woody <- read_csv("DATA/March2026/WOODYP2426.csv")

# unique species
unique_species <- unique(Woody$Species_name)
unique_species

# saving unique woody species
 write.csv(unique_species, "DATA/Harmonised Taxonomisation/Woody_UNIQUEspecies26.csv", row.names = FALSE)


#loading data of unique species
Woody_species <- read_csv("DATA/Harmonised Taxonomisation/Woody_UNIQUEspecies26.csv")

# checking african species 
africa_species <- Woody_species %>% 
  distinct(Species_name)   # one row per species

#
africa_species <- africa_species %>%
  filter(
    !str_detect(
      Species_name,
      regex("^\\s*[^\\s]+\\s+(sp\\.?|spp\\.?)\\b", ignore_case = TRUE)
    )
  )


# Extracting species names from the dataframe
species_list <- africa_species$Species_name


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

# Combining harmonized names with original data
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
#write.csv(species_Harmonized, "DATA/Harmonized_WPspp26.csv", row.names = FALSE)


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

Grass_species <-read_csv("DATA/March2026/Grasses2426Updated1.csv")


# extracting unique species
unique_species <- unique(Grass_species$Species_name)
unique_species

#write.csv(unique_species, "DATA/Harmonised Taxonomisation/Grasses_UNIQUEspecies26.csv", row.names = FALSE)


#loading data

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



# Extracting species names from the dataframe
species_list <- africa_species$Species_name


# Loading WFO backbone into memory
WFO.remember()  # assumes 'classification.csv' is in working directory

# (optional) Cleaning names to remove authorship or punctuation
# Only if names include authorship like "(L.) Willd."
prepared <- WFO.prepare(spec.data = species_list)
cleaned_names <- prepared$spec.name  # this will be used for matching

# Matching cleaned species names with WFO backbone
matches <- WFO.match(spec.data = cleaned_names, WFO.data = WFO.data)


# Picking the best single match per species
best_matches <- WFO.one(matches)

# Combining harmonized names with original data
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
 #write.csv(species_Harmonized, "DATA/HarmonizedGrass_GRspp.csv", row.names = FALSE)


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

