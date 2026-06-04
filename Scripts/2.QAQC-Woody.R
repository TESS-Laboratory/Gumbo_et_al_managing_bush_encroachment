library(tidyverse)
library(stringi)
library(dataMaid)

# Load required packages
#if (!require("dataMaid")) install.packages("dataMaid", dependencies = TRUE)
#library(dataMaid)

# Load your dataset
df <- read.csv("your_dataset.csv", stringsAsFactors = FALSE)
TC <- read_csv("DATA/March2025/Woodyplants25.csv")

WC <- read_csv("DATA/March2025/Woody2426b.csv")

# ----------------------------
# QUALITY CHECK (QC)
# ----------------------------

cat("---- BASIC STRUCTURE ----\n")
str(WC)
summary(WC)

cat("\n---- HEAD OF DATA ----\n")
print(head(df))

# Check for missing values
cat("\n---- MISSING VALUES ----\n")
print(colSums(is.na(WC)))

# Check for duplicate rows
cat("\n---- DUPLICATES ----\n")
duplicated_rows <- WC[duplicated(WC), ]
cat("Number of duplicated rows: ", nrow(duplicated_rows), "\n")

# Data types of each column
cat("\n---- DATA TYPES ----\n")
print(sapply(WC, class))

# Range and summary of numeric variables
cat("\n---- HEIGHT SUMMARY ----\n")
print(summary(WC$`Max_height(m)`))

cat("\n---- Fencing ----\n")
print(summary(WC$Fenced))

# Unique values for categorical variables
cat("\n---- UNIQUE TREATMENTS ----\n")
print(unique(WC$Treatment))

cat("\n---- UNIQUE SPECIES ----\n")
print(length(unique(WC$Species_name)))
print(head(unique(WC$Species_name), 50))  # show first 10 species

# Outlier detection (boxplot)
boxplot(WC$Height, main = "Boxplot of Height", ylab = "`Max_height(m)`", col = "lightblue")

####### ----------------------------########################################
# QUALITY ASSURANCE (QA)
# ----------------------------

# Cross-tabulations to check data consistency
cat("\n---- PLOT vs SUBPLOT COMBINATION ----\n")
print(table(WC$Plot, WC$Subplot))

cat("\n---- YEAR vs SUBPLOT COMBINATION ----\n")
print(table(WC$Year, WC$Subplot))

# Controlled vocabulary check
expected_treatments <- c("C", "F", "TF",
                         "TFB", "THF")
invalid_treatments <- setdiff(unique(WC$Treatment), expected_treatments)
cat("\n---- INVALID TREATMENT VALUES ----\n")
print(invalid_treatments)

# Logical check: Height = 0 but Value != 0
cat("\n---- LOGICAL CHECK:Max_Height(m) = 0 but Value != 0 ----\n")
inconsistent <- subset(WC$ Max_Height(m)== 0 & Value != 0)
print(nrow(inconsistent))
print(head(inconsistent)) 

# ----------------------------
# OPTIONAL: Automated QA/QC Report
# ----------------------------

# Generate HTML QA/QC report
makeDataReport(df, output = "html", replace = TRUE, file = "QAQC_Report")

cat("\nQA/QC script completed. Check QAQC_Report.html for detailed results.\n")


#################################################################### 

WCdata <- read_csv("DATA/March2025/Woody2426b.csv")
# Convert Spp_name to UTF-8 encoding
WCdata <- WCdata %>%
  mutate(Species_name = stri_enc_toutf8(Species_name))


# Check for Missing Values
missing_spp <- sum(is.na(WCdata$Species_name))
cat("Missing values in Species_name:", missing_spp, "\n")


# Trimming spaces
WCdata <- WCdata %>%
  mutate(Site = str_trim(Site),
         Species_name = str_trim(Species_name))

# Checking for consistency
unique_sites <- unique(WCdata$Site)
unique_species <- unique(WCdata$Species_name)

print("Unique values in 'Site' after standardization:")
print(unique_sites)

print("Unique values in 'Species_name' after standardization:")
print(unique_species)


# Summarize Unique Entries
unique_spp <- WCdata %>%
  count(Species_name) %>%
  arrange(desc(n))


#######QA AND QC ON SITE, PLOTS, SUBPLOTS

# 1. Check for Missing Values 
missing_values <- WCdata %>%
  summarize(
    Missing_Site = sum(is.na(Site)),
    Missing_Plot = sum(is.na(Plot)),
    Missing_Subplot = sum(is.na(Subplot))
  )
cat("Missing values in variables:\n")
print(missing_values)

# 2. Check for Duplicate Combinations
duplicates <- WCdata %>%
  group_by(Site, Plot, Subplot) %>%
  summarize(count = n()) %>%
  filter(count > 2)

cat("Duplicate combinations of Site, Plot, and Subplot:\n")
print(duplicates)

# 3. Validate Against Expected Values
# Example: Define expected values for Site, Plot, and Subplot
expected_sites <- c("A", "B", "C", "D", "E", "F")
expected_subplots <- c("Z1", "Z2", "Z3", "Z4")

invalid_entries <- WCdata %>%
  filter(
    !Site %in% expected_sites |
      Plot < 1 | Plot > 100 |  # Example range for Plot (adjust as needed)
      !Subplot %in% expected_subplots
  )

cat("Invalid entries:\n")
print(invalid_entries)

# 4. Standardize Formatting
WCdata <- WCdata %>%
  mutate(
    Site = str_to_upper(str_trim(Site)),        # Convert Site to uppercase
    Subplot = str_to_upper(str_trim(Subplot)), # Convert Subplot to uppercase
    Plot = as.integer(Plot)                    # Ensure Plot is an integer
  )

# 5. Logical Consistency Check
# Example: Ensure each Plot belongs to a unique Site
logical_issues <- WCdata %>%
  group_by(Plot) %>%
  summarize(unique_sites = n_distinct(Site)) %>%
  filter(unique_sites > 1)

cat("Logical consistency issues:\n")
print(logical_issues)

# View the cleaned data
head(WCdata)

#####################################################################
######################################  GRASSES GRASSES GRASSES GRASSES 

Grassesdata2 <- read_csv("DATA/March2025/Grasses25.csv")

##### Convert to UTF-8 encoding
Grassesdata2 <- Grassesdata2 %>%
  mutate(across(everything(), ~iconv(., from = "latin1", to = "UTF-8")))


# 1. Standardizing case
Grassesdata2 <- Grassesdata2 %>%
  mutate(Site = str_to_upper(Site),
         Species_name = str_to_sentence(Species_name),
         Subplot = str_to_upper(Subplot))

# 2. Trimming spaces
Grassesdata2 <- Grassesdata2 %>%
  mutate(Site = str_trim(Site),
         Species_name = str_trim(Species_name))

# Checking for consistency
unique_sites <- unique(Grassesdata2$Site)
unique_species <- unique(Grassesdata2$Species_name)

print("Unique values in 'Site' after standardization:")
print(unique_sites)

print("Unique values in 'Spp_name' after standardization:")
print(unique_species)

# OPTIONAL: Just in case, explicitly set empty strings in Species to NA
Grassesdata2$Species_name[Grassesdata2$Species_name == ""] <- NA

# Check how many NAs you have in the Species column now
sum(is.na(Grassesdata2$Species_name))

# This should return character(0) or an empty list
unique(Grassesdata2$Species_name[!is.na(Grassesdata2$Species_name) & Grassesdata2$Species_name == "NA"])


# Summarize Unique species
unique_spp <- Grassesdata2 %>%
  count(Species_name) %>%
  arrange(desc(n))



#########################################
# Load data and treat "" and "NA" as NA globally
df <- read.csv("DATA/March2025/Grasses25.csv", na.strings = c("", "NA"))
            
#define columns to be cleaned
columns_to_clean <- c("Species_name", "Fencing", "Treatment", "DPM_Height")

# Replace empty strings with NA in specified columns (redundant safeguard)
df[columns_to_clean] <- lapply(df[columns_to_clean], function(x) {
  x[x == ""] <- NA
  return(x)
})

# Optional: Confirm how many NA values per column
sapply(df[columns_to_clean], function(x) sum(is.na(x)))

