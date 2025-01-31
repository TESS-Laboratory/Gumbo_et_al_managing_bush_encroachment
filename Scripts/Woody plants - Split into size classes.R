library(tidyverse)
library(vegan)
library(patchwork)

theme_beautiful <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 8, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 8,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 8, color = "black"),
      legend.title = element_text(size = 8, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
}


Woody_split <- read_csv("C:/workspace/gumbo_dev/DATA/WoodyPlants_C.csv")

## CREATING NEW DATASET BY SPILTTING WOODY PLANTS INTO SIZE CLASSES


# Create a new column for size class
Woody_split$Size_Class <- with(Woody_split, 
                        ifelse(Max_Height >= 0.01 & Max_Height <= 0.50, "Seedlings",
                               ifelse(Max_Height >= 0.51 & Max_Height <= 1.49, "Saplings",
                                      ifelse(Max_Height >= 1.5, "Trees", NA))))

# Remove rows with NA in Size_Class (optional, if you want only valid classes)
Woody_split <- na.omit(Woody_split)

# Save the new dataset
write.csv(Woody_split, "woody_with_size_classes.csv", row.names = FALSE)

# View the first few rows of the modified dataset
head(Woody_split)

# Create separate columns for Seedlings, Saplings, and Trees
Woody_split$Seedlings <- ifelse(Woody_split$Size_Class == "Seedlings", Woody_split$Max_Height, NA)
Woody_split$Saplings <- ifelse(Woody_split$Size_Class == "Saplings", Woody_split$Max_Height, NA)
Woody_split$Trees <- ifelse(Woody_split$Size_Class == "Trees", Woody_split$Max_Height, NA)


# Save the new dataset with separate columns
write.csv(Woody_split, "woody_with_separate_columns.csv", row.names = FALSE)
head(Woody_split)
tail(Woody_split)
####################################################################################################
####################################################################################################

################  DETERMINING METRICS FOR TREES
###### TREEE HEIGHT

data <- read_csv ("C:/workspace/gumbo_dev/DATA/Woody_sizeclasses.csv")

# Filter for trees and calculate height statistics at the site level
trees_height_summary <- data %>%
  filter(!is.na(Trees)) %>%  # Keep only rows where Trees are recorded
  group_by(Site, Plot) %>%
  summarise(
    Mean_Height = mean(Max_Height, na.rm = TRUE),   
    Median_Height = median(Max_Height, na.rm = TRUE),
    Max_Height = max(Max_Height, na.rm = TRUE),     
    Min_Height = min(Max_Height, na.rm = TRUE),     
    Count = n(), 
    .groups = "drop"  )

# Filter for trees 
trees_height_summary <- data %>%
  filter(!is.na(Trees))

THgt <- ggplot(trees_height_summary, aes(x = interaction(Site,Plot), y = Max_Height)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site and Plot",
    y = "Tree height (m)"
  ) +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8))

# Saving as png
ggsave(THgt,
       filename = "C:/workspace/gumbo_dev/Plots/ TreeHEIGHT Site&Plot.png",
       width = 16, height = 14, units = "cm" )

########################################################################################

## DETERMINE TREE COMPOSITION

# Filter for trees 

trees_comp <- data %>%
  filter(!is.na(Trees)) %>%  # Keep only rows where Trees are recorded
  group_by(Site, Plot, Spp_name) %>%
  summarise(Count = n(), 
    .groups = "drop"  )

#Number of tree species per site
trees_comp2 <- data %>%
  filter(!is.na(Trees)) %>% 
  group_by(Site) %>%
  summarise(Trees_count = length(unique(Spp_name)))

# 
Tcompa <- ggplot(trees_comp2) + 
  geom_col(aes(x = Site, y = Trees_count), fill = "grey", colour = "grey", width = 0.75) +
  labs(x = "Site", y = "No. of tree species", tag = "(a)") + theme_beautiful() +
  #plot_annotation("(a)")
  theme(
    legend.position = "right",
    plot.tag.position = c(0, 0.95)) # Adjust tag position (x, y)


# Saving as png
ggsave(Tcomp,
       filename = "C:/workspace/gumbo_dev/Plots/ TREESpecies Site.png",
       width = 16, height = 14, units = "cm" )


### Count unique woody species at each plot within each site
Tplot_summary <- data %>%
  filter(!is.na(Trees)) %>% 
  group_by(Site, Plot) %>%
  summarise(
    species_count = n_distinct(Spp_name),
    total_individuals = n (),  # Assuming 'Count' column exists
    .groups = "drop" )

# Plot species richness by plot within each site
Tpp2b <- ggplot(Tplot_summary, aes(x = Plot, y = species_count, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Plot", y = "No. of tree species") +
  theme_beautiful() + 
  theme(legend.position = "right") +
  plot_annotation("(b)") 
  

# ggplot(Tplot_summary, aes(x = Plot, y = species_count, fill = Site)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  labs(x = "Plot", y = "No. of tree species", tag = "(b)") + # optional to add tag = . Remove it when not necessary
#  theme_beautiful() + 
#   theme(                                  ## when adjusting tag 'a'
#    legend.position = "right",
#    plot.tag.position = c(0, 0.95)) # Adjust tag position (x, y)  


ggsave(Tpp2,
       filename = "C:/workspace/gumbo_dev/Plots/ TREESpecies SiteandPlot.png",
       width = 16, height = 14, units = "cm" )

## COMBINING SPECIES COMPOSITION PLOTS INTO A SINGLE MULTI PANEL FOR SITE AND SP

# Combining plots
 Aab <- (Tcompa|Tpp2b)

ggsave(Aab,
    filename = "C:/workspace/gumbo_dev/Plots/SPP COMPO Combinedplots -Trees.png",
  width = 16, height = 10, units = "cm" )



##########################################################################################

### REDOING DIVERSITY CALCULATIONS SO THAT THEY SHOW PER SITE

# Load the dataset
data <- read_csv("C:/workspace/gumbo_dev/DATA/Woody_sizeclasses.csv")

# Filter data for trees and group by Site, Plot, and Spp_name
# Filter data for trees and calculate species abundance
trees_data2 <- data %>%
  filter(!is.na(Trees)) %>%                  # Filter out rows with no Trees
  group_by(Site, Plot, Spp_name) %>%         # Group by Site, Plot, and Species
  summarise(Trees_Count = n(), .groups = "drop")  # Count the number of trees per species

# Calculate Shannon-Wiener Diversity Index at Site and Plot level
diversity_data <- trees_data2 %>%
  group_by(Site, Plot) %>%                   # Group by Site and Plot
  summarise(
    Shannon_Diversity = -sum((Trees_Count / sum(Trees_Count)) * log(Trees_Count / sum(Trees_Count))),
    .groups = "drop"
  )

## Creating plot for shannon diversity at site level
Tsdv <- ggplot(diversity_data, aes(x = Site, y = Shannon_Diversity)) +
  geom_bar(stat = "identity", position = "dodge", fill = "grey", colour = "grey") +
  labs(
    x = "Site",
    y = "Shannon-Weiner Diversity Index"
  ) +
  theme_beautiful() 

#### USING ANNOTATIONS
# Tsdv <- ggplot(diversity_data, aes(x = Site, y = Shannon_Diversity)) +
#  geom_bar(stat = "identity", position = "dodge", fill = "grey", colour = "grey") +
#  labs(
#    x = "Site",
#    y = "Shannon-Weiner Diversity Index", tag = "(a)") + # optional to add tag = . Remove it when not necessary
#   theme_beautiful() + 
#   theme(                                    ## when adjusting tag 'a'
#    legend.position = "right",
#    plot.tag.position = c(0, 0.999)) # Adjust tag position (x, y)  

#Saving data
ggsave(Tsdv,
       filename = "C:/workspace/gumbo_dev/Plots/ ShannonD- Trees SITE.png",
       width = 16, height = 14, units = "cm" )


### Creating plot for Shannon diversity at Site and Plot level
Treep <- ggplot(diversity_data, aes(x = interaction(Site, Plot), y = Shannon_Diversity, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs(x = "Site-Plot", y = "Shannon-Weiner Diversity Index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

## using PLOT ANNOTATIONS
#Treep <- ggplot(diversity_data, aes(x = interaction(Site, Plot), y = Shannon_Diversity, fill = Site)) +
#  geom_bar(stat = "identity", show.legend = FALSE) +
# labs(
#  x = "Site-Plot",
#  y = "Shannon-Weiner Diversity Index", tag = "(b)") + # optional to add tag = . Remove it when not necessary
#  theme_beautiful() + 
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#  theme(                                    ## when adjusting tag 'a'
#    legend.position = "right",
#    plot.tag.position = c(0, 0.999)) # Adjust tag position (x, y)  

##Saving as png
ggsave(Treep,
       filename = "C:/workspace/gumbo_dev/Plots/ Tree Shannon Diversity Index SP.png",
       width = 16, height = 10, units = "cm" )

## COMBINING SHANNON WEINER DIVERSITY PLOTS INTO A SINGLE MULTI PANEL FOR SITE AND SP
# Combining plots
 Ac <- (Tsdv|Treep)

ggsave(Ac,
     filename = "C:/workspace/gumbo_dev/Plots/SHANNON Combinedplots -Trees .png",
    width = 16, height = 10, units = "cm" )

############################################################################################
##########################################################################################
###########         CALCULATING SIMPSONS DIVERSITY INDEX

# Calculate species abundance per plot
Tplot_data <- data %>%
  filter(!is.na(Trees)) %>%  
  group_by(Site, Plot, Spp_name) %>% 
  summarise(abundance = n(), .groups = 'drop')

# Calculate Simpson's Index for each plot
Tplot_diversity_simpson <- Tplot_data %>%
  group_by(Site, Plot) %>%
  summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')


#write.csv(Tplot_diversity_simpson, "Trees Simpsons Diversity.csv")

# Calculate site-level Simpson diversity (mean across plots)
Tsite_diversity_simpson <- Tplot_diversity_simpson %>%
  group_by(Site) %>%
  summarise(site_simpson_index = mean(simpson_index), .groups = 'drop')

# SITE - level Simpson diversity index
TSIMP <- ggplot(Tsite_diversity_simpson, aes(x = Site, y = site_simpson_index)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs( y = "Simpson Diversity Index") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  plot_annotation("(a)")

## USING pLOT TAG ANNOTATION
#TSIMP <- ggplot(Tsite_diversity_simpson, aes(x = Site, y = site_simpson_index)) +
#  geom_bar(stat = "identity", show.legend = FALSE) +
#  theme_beautiful() +
#  labs( y = "Simpson Diversity Index", tag = "(a)") + # optional to add tag = . Remove it when not necessary
#    theme_beautiful() + 
#     theme(                                    ## when adjusting tag 'a'
#      legend.position = "right",
#      plot.tag.position = c(0, 0.999)) # Adjust tag position (x, y)  

# Saving as png
ggsave(TSIMP,
       filename = "C:/workspace/gumbo_dev/Plots/ TreeSIMP Diversity Index SITE.png",
       width = 16, height = 14, units = "cm" )


######  PLOT -level Simpson diversity index
TPSimp <- ggplot(Tplot_diversity_simpson, aes(x = interaction(Site, Plot), y = simpson_index, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs(x = "Site - Plot", y = "Simpson Diversity Index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


###using plot tag annotations
TPSimp <- ggplot(Tplot_diversity_simpson, aes(x = interaction(Site, Plot), y = simpson_index, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs(x = "Site - Plot", y = "Simpson Diversity Index", tag = "(b)") + # optional to add tag = . Remove it when not necessary
      theme_beautiful() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +     
  theme(                                    ## when adjusting tag 'a'
        legend.position = "right",
        plot.tag.position = c(0, 0.999)) # Adjust tag position (x, y)  

# Saving as png
ggsave(TPSimp,
       filename = "C:/workspace/gumbo_dev/Plots/ TreeSIMPSON Diversity Index PLOT.png",
       width = 16, height = 14, units = "cm" )


#### COMBINING SIMPSON PLOTS INTO A SINGLE MULTI PANEL FOR SITE AND SP

# Combining plots
Ab <- (TSIMP|TPSimp)

 ggsave(Ab,
       filename = "C:/workspace/gumbo_dev/Plots/SIMPCombinedplots -Trees .png",
       width = 16, height = 10, units = "cm" )

############################################################################################

######################### DETERMINING BETA DIVERSITY

# Summarize species abundance for Trees only
tree_species_matrix <- data %>%
  filter(!is.na(Trees)) %>%            # Keep rows with Trees only
  group_by(Site, Spp_name) %>%         # Group by Site and Species name
  summarise(abundance = n(), .groups = 'drop') %>%  # Count occurrences
  spread(key = Spp_name, value = abundance, fill = 0)  # Create wide format matrix

# Replace NA values with 0 (if necessary, as safeguard)
tree_species_matrix[is.na(tree_species_matrix)] <- 0

# Compute Bray-Curtis dissimilarity
bc_dist_trees <- vegdist(tree_species_matrix[,-1], method = "bray")  # Exclude Site column

bc_dist_trees

# Save the new data set
write.csv(bc_dist_trees, "Trees Beta diversity.csv", row.names = FALSE)

#######################################################################################
#######################################################################################

# CALCULATING SORENSEN INDEX

data <- read_csv("C:/workspace/gumbo_dev/DATA/Woody_sizeclasses.csv")

# Step 1: Filter data for trees and calculate species presence/absence per site
trees_data <- data %>%
  filter(!is.na(Trees)) %>%
  group_by(Site, Spp_name) %>%
  summarise(Trees_Count = n(), .groups = "drop") %>%
  mutate(Presence = as.integer(Trees_Count > 0))  # Convert to 1 if present

# Step 2: Convert data into a species-site matrix
species_matrix <- trees_data %>%
  select(Site, Spp_name, Presence) %>%
  pivot_wider(names_from = Spp_name, values_from = Presence, values_fill = list(Presence = 0))  # Ensure correct format


# Compute Sorensen similarity index
sorensen_matrix <- vegdist(species_matrix[,-1], method = "bray")  # Bray-Curtis is 1 - Sorensen

sorensen_matrix


# Convert dissimilarity to similarity (Sorensen = 1 - Bray-Curtis)
sorensen_similarity <- 1 - sorensen_matrix
sorensen_similarity

###SAVING AS A TABLE
# Convert the Sorensen similarity matrix into a data frame
sorensen_df <- as.data.frame(as.matrix(sorensen_similarity))

# Add site names as a column (for better interpretation)
sorensen_df <- tibble::rownames_to_column(sorensen_df, var = "Site")

# Save the results to a CSV file
write.csv(sorensen_df, "Tree sorensen_similarity.csv", row.names = FALSE)

#######################################################################################

##CALCULATING JACCARD DIVERSITY INDEX FOR TREES

# Step 1: Filter data for trees and calculate species presence/absence per site
trees_data <- data %>%
  filter(!is.na(Trees)) %>%
  group_by(Site, Spp_name) %>%
  summarise(Trees_Count = n(), .groups = "drop") %>%
  mutate(Presence = as.integer(Trees_Count > 0))  # Convert to 1 if present

# Step 2: Convert data into a species-site matrix
species_matrix <- trees_data %>%
  select(Site, Spp_name, Presence) %>%
  pivot_wider(names_from = Spp_name, values_from = Presence, values_fill = list(Presence = 0))  # Ensure correct format

# Step 2: Compute Jaccard similarity index
jaccard_matrix <- vegdist(species_matrix[,-1], method = "jaccard", binary = TRUE)  # Binary = TRUE for presence/absence

# Convert dissimilarity to similarity (Jaccard similarity = 1 - Jaccard dissimilarity)
jaccard_similarity <- 1 - jaccard_matrix
jaccard_similarity

# Step 3: Convert results into a data frame
jaccard_df <- as.data.frame(as.matrix(jaccard_similarity))
jaccard_df <- tibble::rownames_to_column(jaccard_df, var = "Site")

# Save the results as a CSV file
write.csv(jaccard_df, "jaccard_similarity_results.csv", row.names = FALSE)

#########################################################################################################

############### woody plants density per site

# Load the data
df <- read_csv("C:/workspace/gumbo_dev/DATA/Woody_sizeclasses.csv")

# Group by Site and sum Seedlings, Saplings, and Trees
# Count the number of non-NA entries for Seedlings, Saplings, and Trees per site
site_counts <- df %>%
  group_by(Site) %>%
  summarise(
    Seedlings = sum(!is.na(Seedlings)),
    Saplings = sum(!is.na(Saplings)),
    Trees = sum(!is.na(Trees))
  )%>%
  # Apply the density formula
  mutate(across(c(Total_Seedlings, Total_Saplings, Total_Trees), ~ . * 10000 / 12000))


# Reshape data for plotting
site_density_long <- site_counts %>%
  pivot_longer(cols = c(Seedlings,Saplings,Trees), 
               names_to = "Woody_class", values_to = "Density")


## Reorder woody classes
Site_data_long <- site_density_long %>%
  mutate(Woody_class = factor(Woody_class, levels = c("Seedlings", "Saplings", "Trees")))

# Plot the bar graph
WC <- ggplot(Site_data_long, aes(x = Site, y = Density, fill = `Woody_class`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = "Density per hectare", fill = "Legend") +
  theme_beautiful() + theme(legend.position = "right")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

#Saving as png
ggsave(WC,
       filename = "C:/workspace/gumbo_dev/Plots/ WOODY DENSITY SITE.png",
       width = 16, height = 14, units = "cm" )
