library(vegan)

# Load data
heights_data <-  read_csv("DATA/March2025/2Grasses2426.csv")

Grasses <- read_csv("DATA/March2025/Grasses2426Updated.csv")


# Calculate richness per subplot
alpha_df <- Grasses %>%
  filter(!is.na(Species_name)) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing,  Year) %>%
  summarise(richness = n_distinct(Species_name),  # count unique species
            .groups = 'drop')



# Run model with nested random effect (no plot_id column needed)
alpha_model <- lmer(richness ~ Treatment * Fencing + (1|Site:Treatment),
                    data = alpha_df)

summary(alpha_model)


## DETERMINING BETA DIVERSITY. Create Community Matrix
# If you have long format (each row = species occurrence)
comm_matrix <- Grasses %>%
  filter(!is.na(Species_name)) %>%           # Remove NA species entirely
  mutate(presence = 1) %>%
  pivot_wider(id_cols = c(Site, Treatment, Fencing, Subplot),
              names_from = Species_name,
              values_from = presence,
              values_fill = 0,
              values_fn = list(presence = max))
  

##### 2. Extract metadata
metadata <- comm_matrix %>%
  dplyr::select(Site, Treatment, Fencing, Subplot)


### 3. Now add nested_id to metadata
metadata$nested_id <- with(metadata, interaction(Site, Treatment, drop = TRUE))

# 4. Extract species matrix
species_matrix <- comm_matrix %>%
dplyr:: select(-Site, -Treatment, -Fencing, -Subplot) %>%
  as.matrix()


# 5. Calculate Jaccard distance
jaccard_dist <- vegdist(species_matrix, method = "jaccard")

# 6. Run PERMANOVA
permanova_result <- adonis2(jaccard_dist ~ Treatment * Fencing,
                            data = metadata,
                            strata = metadata$nested_id,
                            permutations = 999,
                            by = "terms")

print(permanova_result)




####################################################################################
####################################################################################

# CREATING UNIQUE PLOT AND SUBPLOT ID

# Create unique identifiers
Grasses <- Grasses %>%
  mutate(
    # Unique plot ID (combines Location + Plot + Treatment)
    # Treatment is included because Plot numbers aren't consistent with treatment
    plot_id = paste(Site, Plot, Treatment, sep = "_"),
    
    # Unique subplot ID (combines plot_id + Subplot + fencing)
    subplot_id = paste(plot_id, Subplot, Fencing, sep = "_"),
    
    # Alternative: simpler nested ID for lmer
    nested_id = paste(Site, Plot, sep = "_")  # if Treatment is consistent within Plot
  )


# Verifying 
# Check the mapping of Plot numbers to Treatments within each Location
Grasses %>%
  distinct(Site, Plot, Treatment) %>%
  arrange(Site, Plot) %>%
  print(n = 50)

# This will show you if any Plot number has multiple Treatments within the same Location
# (which would be a problem)


## CALCULATING Species Richness (WITH DELTA APPROACH)
# Calculate richness per subplot per year
alpha_df <- Grasses %>%
  filter(!is.na(Species_name)) %>%
  group_by(Site, Plot,Subplot, Treatment, Fencing,  Year, plot_id, subplot_id) %>%
  summarise(richness = n_distinct(Species_name), .groups = 'drop')

# Convert to wide format for delta calculation
alpha_wide <- alpha_df %>%
 dplyr:: select(Site, Plot,Subplot, Treatment, Fencing,  Year, plot_id, subplot_id, richness) %>%
  pivot_wider(id_cols = c(Site, Plot,Subplot, Treatment, Fencing, plot_id, subplot_id),
              names_from = Year,
              values_from = richness,
              names_prefix = "richness_")

# Calculate delta (change from 2004 to 2016)
alpha_wide <- alpha_wide %>%
  mutate(delta_richness = richness_2026 - richness_2024)


# LMM Analysis 
# Model with Site as random effect (your sites)
alpha_model <- lmer(delta_richness ~ Treatment * Fencing + (1|Site),
                    data = alpha_wide)

  #summary(alpha_model)



#### CALCULATING BETA DIVERSITY  

# Create wide format for beta diversity
comm_matrix <- Grasses %>%
  filter(!is.na(Species_name)) %>%
  mutate(presence = 1) %>%
  dplyr:: select(Site, Plot,Subplot, Treatment, Fencing,  Year, subplot_id, Species_name, presence) %>%
  distinct() %>%  # Remove any duplicates
  pivot_wider(id_cols = c(Site, Plot,Subplot, Treatment, Fencing,  Year, subplot_id),
              names_from = Species_name,
              values_from = presence,
              values_fill = 0)

# Extract metadata
metadata <- comm_matrix %>%
dplyr::  select(Site, Plot,Subplot, Treatment, Fencing,  Year, subplot_id)

# Create nested ID for permutations
metadata$nested_id <- with(metadata, paste(Location, Plot, Treatment, sep = "_"))

# Species matrix
species_matrix <- comm_matrix %>%
dplyr::  select(-Site, -Plot,-Subplot, -Treatment, -Fencing,  -Year, -subplot_id) %>%
  as.matrix()

# Calculate Jaccard distance (for all samples combined)
jaccard_dist <- vegdist(species_matrix, method = "jaccard")


### Calculate temporal beta diversity per subplot (delta composition)
# Get unique subplot IDs 
Subplot <- unique(metadata$subplot_id)

# Calculate beta diversity delta for each subplot
beta_delta <- data.frame()

for(sp in Subplot) {
  # Get species vectors for this subplot in 2024 and 2026
  rows_2024 <- which(metadata$subplot_id == sp & metadata$year == 2024)
  rows_2026 <- which(metadata$subplot_id == sp & metadata$year == 2026)
  
  if(length(rows_2024) > 0 & length(rows_2026) > 0) {
    # Extract species vectors
    vec_2024 <- species_matrix[rows_2024, ]
    vec_2026 <- species_matrix[rows_2026, ]
    
    # Combine and calculate Jaccard dissimilarity
    combined <- rbind(vec_2024, vec_2026)
    temporal_change <- vegdist(combined, method = "jaccard")[1]
    
    # Get metadata for this subplot
    subplot_meta <- metadata[rows_2024[1], ]
    
    beta_delta <- rbind(beta_delta, 
                        data.frame(subplot_id = sp,
                                   Site = subplot_meta$Site,
                                   Plot = subplot_meta$Plot,
                                   Treatment = subplot_meta$Treatment,
                                   Fencing = subplot_meta$Fencing,
                                   Subplot = subplot_meta$Subplot,
                                   delta_composition = temporal_change))
  }
}

# View
head(beta_delta)



############## SIMPLIFIED BETA DIVERSITY CALCULATION 

# First, create a wide matrix with species as columns
# But keep the structure simple - one row per species per subplot per year

# Calculate Jaccard dissimilarity between years for each subplot
beta_delta <- Grasses %>%
  filter(!is.na(Species_name)) %>%
  # Ensure each subplot has both years
  group_by(subplot_id) %>%
  filter(n_distinct(Year) == 2) %>%  # Keep only subplots with both years
  ungroup() %>%
  # Nest data by subplot
  nest(data = -c(subplot_id, Site, Plot, Subplot, Treatment, Fencing)) %>%
  mutate(
    # Calculate Jaccard between years
    jaccard = map_dbl(data, function(df) {
      # Get species lists for each year
      spp_2024 <- df %>% filter(Year == 2024) %>% pull(Species_name) %>% unique()
      spp_2026 <- df %>% filter(Year == 2026) %>% pull(Species_name) %>% unique()
      
      # Calculate Jaccard
      intersection <- length(intersect(spp_2024, spp_2026))
      union <- length(union(spp_2024, spp_2026))
      
      if(union == 0) {
        return(NA)  # No species in either year
      } else {
        return(1 - (intersection / union))  # Jaccard dissimilarity
      }
    })
  ) %>%
  dplyr:: select(-data) %>%
  filter(!is.na(jaccard))

# View results
head(beta_delta)

# Check for NAs
sum(is.na(beta_delta$jaccard))



## LMM analysis
# Model delta composition
beta_model <- lmer(jaccard ~ Treatment * Fencing + (1|Site),
                   data = beta_delta)

# Results

summary(beta_model)

# calculating effect size 
eta_squared(beta_model)  

# Small = 0.01, Medium = 0.06, Large = 0.14
# If your effects are small (<0.01), they're truly negligible


# Residual diagnostics
plot(beta_model)  # Residuals vs fitted
qqnorm(residuals(beta_model))
qqline(residuals(beta_model))
