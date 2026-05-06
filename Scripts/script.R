#WORLDFLORA PACKAGE TO STANDARDIZE SPECIES SCIENTIFIC NAMES.

#Useful links:

#link with help manual
#https://roelandkindt.r-universe.dev/WorldFlora


#https://roelandkindt.r-universe.dev/WorldFlora/doc/manual.html#WFO.prepare


######Download WFO Backbone######
# Step 1: Install the package

 # install.packages("WorldFlora")

 # install.packages("fuzzyjoin")


# Step 2: Load the package
library(WorldFlora)

library(fuzzyjoin)
library(tidyverse)

#Step 3: The WorldFlora package does not include the plant database by default — you need to download it separately

# Download the WFO backbone (only once)

 # WFO.download() # this one failed i downloaded manually


#WFO.download(WFO.url =
               #paste0("https://files.worldfloraonline.org/files/WFO_Backbone/",
              #        "_WFOCompleteBackbone/WFO_Backbone.zip"),
             #save.dir = getwd(), WFO.remember = TRUE,
             timeout = 500)


#download of a file named "classification", in csv


# Step 4: Register the WFO data so R knows where it is

 WFO.remember()

WFO.remember("DATA/WFO_Backbone.zip")


#####Example 1 w/ 98 sp#####

#Step 5: Prepare the data.frame with species name to be confirm


species <- read_csv("DATA/Woody2_plants_species.csv")

species

# Rename the column to match what WFO.match expects
colnames(species)[which(names(species) == "TaxonName")] <- "species"


# Step 6: Run WFO.match.fuzzyjoin() on your dataset
?WFO.match.fuzzyjoin

species_WFO <- WFO.match.fuzzyjoin(
  spec.data = species, #data.frame containing variables w/ sp name
  WFO.file = NULL,
  WFO.data = WFO.data, #copy of the WFO Backbone
  spec.name = "species", #column w/ sp name
  fuzzydist.max = 4 #maximum distance for joining
)


fuzzydist.max = How many letters were different between sp name and the matched name? Lower is better (0 = exact match)


#Important colunms:

#1 - species name original search

#9 - Fuzzy - if TRUE means the matching was done by the fuzzy method - look for similar letters to find a match

#10 - Fuzzy distance

#16 - scientific name correct

#41 - New name accepted

#coluns of family, genus, author...


#Clean table
species_WFO_c <- species_WFO[,c(1,9,10,16,41)]


table(species_WFO_c$New.accepted)
#name updated for 17 species, and still the same for 87


table(species_WFO_c$Fuzzy)
#only 1 species corrected using fuzzy methods. majority of them was updated.




######Example 2 Misspelling######

#Example with species with misspelling scientific names

species_mis <- read.csv("sp_ex_misspelling.csv")

species_mis


#Check with WFO

species_mis_WFO <- WFO.match.fuzzyjoin(
  spec.data = species_mis, #data.frame containing variables w/ sp name
  WFO.file = NULL,
  WFO.data = WFO.data, #copy of the WFO Backbone
  spec.name = "spec.name", #column w/ sp name
  fuzzydist.max = 3 #maximum distance for joining
)


#Clean table
species_mis_WFO_c <- species_mis_WFO[,c(1,8,9,15,40)]


#Table complete
species_mis_WFO_c <- species_mis_WFO[,c(1,8,9,15,18,19,23,40)]


table(species_mis_WFO_c$New.accepted)
#name updated for 10 species, and still the same for 29


table(species_mis_WFO_c$Fuzzy)
#27 species corrected using fuzzy methods. majority with problems of misspelling







######Obtaining data from WORLDFLORA######

# Show all species of a genus

#Step 1. create a dt with the genus name
test1 <- data.frame(Genus=c("Araucaria"))
#data.frame with the genus

#Step 2. indicate where is the WorldFlora backbone
WFO.file <- file.choose("C:/Users/rj485/Desktop/WorldFlora_package/test_R/WFO_Backbone/classification.csv")
#include the backbone again

#Step 3. Look for matches between the genus name and the WFO backbone
t1 <- WFO.match(test1, #genus oy family
                WFO.file = WFO.file,#backbone of WFO
                exclude.infraspecific = TRUE, 
                verbose = TRUE)



t1 #107 records


unique(t1$scientificName) #39 unique species name of Araucaria

#check only accepted species:
WFO.browse("Araucaria", 
           WFO.data = WFO.data, 
           accepted.only = TRUE) #only accepted scientific names




#Check for data of a specific Family

WFO.browse("Vochysiaceae", WFO.data = WFO.data)
#all genus that exist

WFO.browse("Vochysiaceae", WFO.data = WFO.data, accepted.only = TRUE)
#only genus accepted




# get a list of species
Vochysiaceae <- WFO.data[WFO.data$family == "Vochysiaceae",] #525 sp

Vochysiaceae <- Vochysiaceae[Vochysiaceae$taxonRank == "species", ] #419 sp

Vochysiaceae <- Vochysiaceae[Vochysiaceae$taxonomicStatus == "Accepted", ] #250 sp








######Comparing different sources (FloraBR + WorldFlora)######

#Standardize scientific names using Flora do Brasil project

install.packages("flora")

library(flora)

#Using the list of SPECIES (98 sp), to compare with the results obtained using WFO, available in species_WFO


?get.taxa

species_floraBR <- get.taxa(species[,1], 
                            replace.synonyms = TRUE, 
                            suggest.names = TRUE, 
                            #obtain information about ecology and distribution of species
                            life.form = TRUE, habitat = TRUE, vegetation.type = TRUE,
                            establishment = TRUE,
                            domain = TRUE, endemism = TRUE,
                            drop = c("genus", "specific.epiteth", "name.status"), 
                            #similar to fuzzy distance, here we need to inform the distance to be used
                            suggestion.distance = 0.9, parse = FALSE)


species_floraBR$search.str


#compare FloraBR with WorldFlora


#WorldFlora
species_WFO_c$scientificName # name corrected by WorldFlora
species_WFO_c$source <- "WorldFlora"

sp_WFO <- species_WFO_c[,c(1,4,6)] #original search, name and source


#FloraBrasil
species_floraBR$search.str #name corrected by FloraBR
species_floraBR$source <- "FloraBR"

sp_Flora <- species_floraBR[,c(9,12, 19)] #original search, scientific name corrected by FloraBR and source
names(sp_Flora)[1] <- "scientificName"


#Match columns names
names(sp_WFO)[1] <- "original_search"
names(sp_Flora)[2] <- "original_search"

# Rename for clarity before merging
names(sp_WFO)[2] <- "scientificName_WFO"
names(sp_Flora)[1] <- "scientificName_Flora"


#Merge both results for side-by-side comparison

library(dplyr)

comparison_sp <- merge(
  sp_WFO,
  sp_Flora,
  by = "original_search",
  all = TRUE,
  suffixes = c("_WFO", "_FloraBR")
)


#Identify species that have different resolutions between both databases
#Detect Conflicts

comparison_sp <- comparison_sp %>%
  mutate(
    agreement = case_when(
      is.na(scientificName_WFO) | is.na(scientificName_Flora) ~ "Incomplete", 
      #when there is a NA, consider comparison Incomplete
      scientificName_WFO == scientificName_Flora ~ "Match",
      TRUE ~ "Conflict" #when is different, consider a conflict
    )
  )

table(comparison_sp$agreement)

#WorldFlora and Flora do Brasil agreed in 92 species names, and have a conflict for 18 of them.
