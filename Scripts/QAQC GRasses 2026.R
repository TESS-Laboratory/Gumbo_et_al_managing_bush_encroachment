
GC <- read_csv("DATA/March2025/GrassesHeight26.csv")

GC <- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Project development - Taps/BE Kobotoolbox/Vegetation2025/March 2026/2Grasses2426.csv")

# ----------------------------
# QUALITY CHECK (QC)
# ----------------------------

cat("---- BASIC STRUCTURE ----\n")
str(GC)
summary(GC)

cat("\n---- HEAD OF DATA ----\n")
print(head(df))

# Check for missing values
cat("\n---- MISSING VALUES ----\n")
print(colSums(is.na(GC)))

# Check for duplicate rows
cat("\n---- DUPLICATES ----\n")
duplicated_rows <- GC[duplicated(GC), ]
cat("Number of duplicated rows: ", nrow(duplicated_rows), "\n")

# Data types of each column
cat("\n---- DATA TYPES ----\n")
print(sapply(GC, class))

# Range and summary of numeric variables
cat("\n---- HEIGHT SUMMARY ----\n")
print(summary(GC$DPM_Height))

cat("\n---- Fencing ----\n")
print(summary(GC$Fencing))

# Unique values for categorical variables
cat("\n---- UNIQUE TREATMENTS ----\n")
print(unique(GC$Treatment))

cat("\n---- UNIQUE SPECIES ----\n")
print(length(unique(GC$Species_name)))
print(head(unique(GC$Species_name), 50))  # show first 10 species

# Outlier detection (boxplot)
boxplot(GC$DPM_Height, main = "Boxplot of Height", ylab = "DPM_Height", col = "lightblue")

####### ----------------------------########################################
# QUALITY ASSURANCE (QA)
# ----------------------------

# Cross-tabulations to check data consistency
cat("\n---- PLOT vs SUBPLOT COMBINATION ----\n")
print(table(GC$Plot, GC$Subplot))

cat("\n---- YEAR vs SUBPLOT COMBINATION ----\n")
print(table(GC$Year, GC$Subplot))

# Controlled vocabulary check
expected_treatments <- c("C", "F", "TF",
                         "TFB", "THF")
invalid_treatments <- setdiff(unique(GC$Treatment), expected_treatments)
cat("\n---- INVALID TREATMENT VALUES ----\n")
print(invalid_treatments)


## Count observations per subplot  # FILTERING NA per column
Qaqc_counts2 <- GC %>%
  group_by(Year, Site, Plot, Subplot) %>%
summarise(
  across(
    c(DPM_Height,Species_name),
    ~ sum(!is.na(.)),
    .names = "n_{.col}"
  ),
  .groups = "drop"
)


# Flag QA/QC issues per column
BQaqc_issues <- Qaqc_counts2 %>%
  filter(if_any(starts_with("n_DPM_Height"), ~ . != 20))


####################################### QC QA
##### Convert to UTF-8 encoding
GC <- GC%>%
  mutate(across(everything(), ~iconv(., from = "latin1", to = "UTF-8")))


# 1. Standardizing case
GC <- GC %>% 
  mutate(Site = str_to_upper(Site),                 # keep this step
         Species_name = str_replace(                        # 1) make everything lower case
           str_to_lower(Species_name),       # 2) capitalise only the very first letter
           "^.",                             #    (the first character in the string)
           ~ str_to_upper(.x))                #    using a lambda replacement
  )


# 2. Trimming spaces
GC <- GC %>%
  mutate(Site = str_trim(Site),
         Species_name = str_trim(Species_name),
         Fencing = str_trim(Fencing),
         Treatment = str_trim(Treatment))


# Checking for consistency
unique_sites <- unique(GC$Site)
unique_species <- unique(GC$Species_name)
unique_fencing <- unique(GC$Fencing)
unique_treatemnt <- unique(GC$Treatment)

print("Unique values in 'Site' after standardization:")
print(unique_sites)

print("Unique values in 'Species_name' after standardization:")
print(unique_species)

# Summary of the cleaned data
print("Cleaned data:")
print(GC)

###### saving cleaned data
write_csv(GC, "DATA/March2025/Grasses2426b.csv")
