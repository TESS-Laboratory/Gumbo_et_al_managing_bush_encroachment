############DATA QA & QC
library(tidyverse)

Treesdata <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Woody2.csv")
head(Treesdata)


##### Convert to UTF-8 encoding
Treesdata <- Treesdata %>%
  mutate(across(everything(), ~iconv(., from = "latin1", to = "UTF-8")))


# 1. Standardizing case
Treesdata <- Treesdata %>%
  mutate(Site = str_to_sentence(Site),
         Spp_name = str_to_title(Spp_name))

# 2. Trimming spaces
Treesdata <- Treesdata %>%
  mutate(Site = str_trim(Site),
         Spp_name = str_trim(Spp_name))

# Checking for consistency
unique_sites <- unique(Treesdata$Site)
unique_species <- unique(Treesdata$Spp_name)

print("Unique values in 'Site' after standardization:")
print(unique_sites)

print("Unique values in 'Spp_name' after standardization:")
print(unique_species)

# Summary of the cleaned data
print("Cleaned data:")
print(Treesdata)

# Checking for outliers in Max_Height
Q1 <- quantile(data$Max_Height, 0.25, na.rm = TRUE)
Q3 <- quantile(data$Max_Height, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Identify outliers
outliers <- Treesdata %>%
  filter(Max_Height < (Q1 - 1.5 * IQR) | Max_Height > (Q3 + 1.5 * IQR))

print("Outliers in the dataset:")
print(outliers)

print("Summary of cleaned data:")
print(summary(Treesdata))

###Saving the cleaned dataset 
#
write_csv(Treesdata,"C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Woody2.csv")

########################################################################
############################################################### GRASSES DATA CLEANING

Grassesdata <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grasses.csv")
head(Grassesdata)


##### Convert to UTF-8 encoding
Grassesdata <- Grassesdata %>%
  mutate(across(everything(), ~iconv(., from = "latin1", to = "UTF-8")))


# 1. Standardizing case
Grassesdata <- Grassesdata %>%
  mutate(Site = str_to_upper(Site),
         Spp_name = str_to_title(Spp_name))

# 2. Trimming spaces
Grassesdata <- Grassesdata %>%
  mutate(Site = str_trim(Site),
         Spp_name = str_trim(Spp_name),
         Graazing_value = str_trim(Grazing_value))

# Checking for consistency
unique_sites <- unique(Grassesdata$Site)
unique_species <- unique(Grassesdata$Spp_name)
unique_grazing_value <- unique(Grassesdata$Grazing_value)

print("Unique values in 'Site' after standardization:")
print(unique_sites)

print("Unique values in 'Spp_name' after standardization:")
print(unique_species)

##### Fixing inconsistency in words i.e removing capital or small letter
Grassesdata <- Grassesdata %>%
  mutate(Grazing_value = str_to_title(str_trim(Grazing_value)))

# Checking for unique values after standardization
unique_grazing_value <- unique(Grassesdata$Grazing_value)

print("Unique values in 'Grazing_value' after standardization:")
print(unique_grazing_value)

# Summary of the cleaned data
print("Cleaned data:")
print(Grassesdata)

# Checking for outliers in Max_Height
Q1 <- quantile(Grassesdata$DPM.Height, 0.25, na.rm = TRUE)
Q3 <- quantile(Grassesdata$DPM.Height, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Identify outliers
outliers <- Treesdata %>%
  filter(Max_Height < (Q1 - 1.5 * IQR) | Max_Height > (Q3 + 1.5 * IQR))

print("Outliers in the dataset:")
print(outliers)

print("Summary of cleaned data:")
print(summary(Grassesdata))

###Saving the cleaned dataset 
write_csv(Grassesdata, "C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grasses.csv")


###################################################################################################
######### CLEANING DATA INCLUDING PLOT, SUBPLOTS, 0 ETC

Grassesdata2 <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grasses2 - Copy.csv")
head(Grassesdata2)


##### Convert to UTF-8 encoding
Grassesdata2 <- Grassesdata2 %>%
  mutate(across(everything(), ~iconv(., from = "latin1", to = "UTF-8")))


# 1. Standardizing case
Grassesdata2 <- Grassesdata2 %>%
  mutate(Site = str_to_upper(Site),
         Spp_name = str_to_sentence(Spp_name),
         Subplot = str_to_upper(Subplot))

# 2. Trimming spaces
Grassesdata2 <- Grassesdata2 %>%
  mutate(Site = str_trim(Site),
         Spp_name = str_trim(Spp_name),
         Graazing_value = str_trim(Grazing_value))

# Checking for consistency
unique_sites <- unique(Grassesdata2$Site)
unique_species <- unique(Grassesdata2$Spp_name)
unique_grazing_value <- unique(Grassesdata2$Grazing_value)

print("Unique values in 'Site' after standardization:")
print(unique_sites)

print("Unique values in 'Spp_name' after standardization:")
print(unique_species)

##### Fixing inconsistency in words i.e removing capital or small letter
Grassesdata2 <- Grassesdata2 %>%
  mutate(Grazing_value = str_to_title(str_trim(Grazing_value)))

# Checking for unique values after standardization
unique_grazing_value <- unique(Grassesdata2$Grazing_value)

print("Unique values in 'Grazing_value' after standardization:")
print(unique_grazing_value)

# Summary of the cleaned data
print("Cleaned data:")
print(Grassesdata2)

###### saving cleaned data
write.csv("Grassesdata2, C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grasses2 - Copy.csv")

##########################################################################

GrassheigtN <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grass HEIGHT (new).csv")
head(GrassheigtN)

GrassheigtN <- GrassheigtN %>%
  mutate(Site = str_to_upper(Site),
         Subplot = str_to_upper(Subplot))

# 2. Trimming spaces
GrassheigtN <- GrassheigtN %>%
  mutate(Site = str_trim(Site),
         DPM.Height = str_trim(DPM.Height))

# Checking for consistency
unique_sites <- unique(GrassheigtN$Site)
unique_dpm_height <- unique(GrassheigtN$DPM.Height)

print("Unique values in 'Site' after standardization:")
print(unique_sites)

print("Unique values in 'Spp_name' after standardization:")
print(unique_dpm_height)

##### Fixing inconsistency in words i.e removing capital or small letter
Grassesdata2 <- Grassesdata2 %>%
  mutate(Grazing_value = str_to_title(str_trim(Grazing_value)))

# Checking for unique values after standardization
unique_grazing_value <- unique(Grassesdata2$Grazing_value)

print("Unique values in 'Grazing_value' after standardization:")
print(unique_grazing_value)

# Summary of the cleaned data
print("Cleaned data:")
print(GrassheigtN)

###### saving cleaned data
write_csv(GrassheigtN,"C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grass HEIGHT (new).csv")

#####################################################################################

####GRASS HEIGHT NEW CALCULATIONS AFTER DATA CLEANING, zeros included 
grassheightNN <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/CSV. files/Grass HEIGHT only.csv")
head(grassheightNN)


# Calculate average DPM per site
grassheightNN <- grassheightNN %>%
  group_by(Site) %>%
  summarise(grassheightNN = mean(DPM.Height))

# Print the average DPM per site
print(grassheightNN)

# Plot average DPM per site
ggplot(grassheightNN, aes(x = Site, y = grassheightNN,)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Site",
    y = "Mean height (cm)"
  ) +
  theme_classic()
