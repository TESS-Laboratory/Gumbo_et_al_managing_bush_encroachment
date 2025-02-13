library(tidyverse)
library(vegan)
library(patchwork)
library(flextable)
library(officer)


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


################  DETERMINING METRICS FOR SAPLINGS
###### SAPLINGS HEIGHT

data <- read_csv ("C:/workspace/gumbo_dev/DATA/Woody_sizeclasses.csv")

# Filter for SAPLINGS and calculate height statistics at the site level
saplings_height_summary <- data %>%
  filter(!is.na(Saplings)) %>%  # Keep only rows where Trees are recorded
  group_by(Site, Plot) %>%
  summarise(
    Mean_Height = mean(Max_Height, na.rm = TRUE),   
    Median_Height = median(Max_Height, na.rm = TRUE),
    Max_Height = max(Max_Height, na.rm = TRUE),     
    Min_Height = min(Max_Height, na.rm = TRUE),     
    Count = n(), 
    .groups = "drop"  )

# Filter for SAPLINGS 
saplings_height_summary <- data %>%
  filter(!is.na(Saplings))

SHgt <- ggplot(saplings_height_summary, aes(x = interaction(Site,Plot), y = Max_Height)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    x = "Site and Plot",
    y = "Tree height (m)"
  ) +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8))

# Saving as png
ggsave(SHgt,
       filename = "C:/workspace/gumbo_dev/Plots/ SAPLINGS HEIGHT Site&Plot.png",
       width = 16, height = 14, units = "cm" )

########################################################################################

## DETERMINE SAPLINGS COMPOSITION

# Filter for SAPLINGS 

saplings_comp <- data %>%
  filter(!is.na(Saplings)) %>%  # Keep only rows where Trees are recorded
  group_by(Site, Plot, Spp_name) %>%
  summarise(Count = n(), 
            .groups = "drop"  )

#Number of tree species per site
saplings_comp2 <- data %>%
  filter(!is.na(Saplings)) %>% 
  group_by(Site) %>%
  summarise(Saplings_count = length(unique(Spp_name)))

# 
Scompa <- ggplot(saplings_comp2) + 
  geom_col(aes(x = Site, y = Saplings_count), fill = "grey", colour = "grey", width = 0.75) +
  labs(x = "Site", y = "No. of Sapling species", tag = "(a)") + theme_beautiful() +
  #plot_annotation("(a)")
  theme(
    legend.position = "right",
    plot.tag.position = c(0, 0.95)) # Adjust tag position (x, y)


# Saving as png
ggsave(Scompa,
       filename = "C:/workspace/gumbo_dev/Plots/ SAPLINGS Species Site.png",
       width = 16, height = 14, units = "cm" )


### Count unique saplings species at each plot within each site
Splot_summary <- data %>%
  filter(!is.na(Saplings)) %>% 
  group_by(Site, Plot) %>%
  summarise(
    species_count = n_distinct(Spp_name),
    total_individuals = n (),  # Assuming 'Count' column exists
    .groups = "drop" )

# Plot species richness by plot within each site
#Spp2b <- ggplot(Splot_summary, aes(x = Plot, y = species_count, fill = Site)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  labs(x = "Plot", y = "No. of saplings species") +
#  theme_beautiful() + 
#  theme(legend.position = "right") +
#  plot_annotation("(b)") 


######USING TAG ANNOTATIONS
Sppb <- ggplot(Splot_summary, aes(x = Plot, y = species_count, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Plot", y = "No. of Sapling species", tag = "(b)") + # optional to add tag = . Remove it when not necessary
  theme_beautiful() + 
   theme(                                  ## when adjusting tag 'a'
    legend.position = "right",
    plot.tag.position = c(0, 0.95)) # Adjust tag position (x, y)  


ggsave(Sppb,
       filename = "C:/workspace/gumbo_dev/Plots/ SAPLINGpecies SiteandPlot.png",
       width = 16, height = 14, units = "cm" )

## COMBINING SPECIES COMPOSITION PLOTS INTO A SINGLE MULTI PANEL FOR SITE AND SP

# Combining plots
Aab <- (Scompa|Sppb)

ggsave(Aab,
       filename = "C:/workspace/gumbo_dev/Plots/SPP COMPO Combinedplots -Saplings.png",
       width = 16, height = 12, units = "cm" )



##########################################################################################

### REDOING SAPLINGS DIVERSITY CALCULATIONS SO THAT THEY SHOW PER SITE

# Filter data for trees and group by Site, Plot, and Spp_name
# Filter data for trees and calculate species abundance
saplings_data2 <- data %>%
  filter(!is.na(Saplings)) %>%                  # Filter out rows with no Trees
  group_by(Site, Plot, Spp_name) %>%         # Group by Site, Plot, and Species
  summarise(Saplings_Count = n(), .groups = "drop")  # Count the number of trees per species

# Calculate Shannon-Wiener Diversity Index at Site and Plot level
diversity_data <- saplings_data2 %>%
  group_by(Site, Plot) %>%                   # Group by Site and Plot
  summarise(
    Shannon_Diversity = -sum((Saplings_Count / sum(Saplings_Count)) * log(Saplings_Count / sum(Saplings_Count))),
    .groups = "drop"
  )

## Creating plot for shannon diversity at site level
#Ssdv <- ggplot(diversity_data, aes(x = Site, y = Shannon_Diversity)) +
#  geom_bar(stat = "identity", position = "dodge", fill = "grey", colour = "grey") +
#  labs(
#    x = "Site",
#    y = "Shannon-Weiner Diversity Index"
#  ) +
#  theme_beautiful() 

#### USING ANNOTATIONS
Ssdv <- ggplot(diversity_data, aes(x = Site, y = Shannon_Diversity)) +
  geom_bar(stat = "identity", position = "dodge", fill = "grey", colour = "grey") +
  labs(
    x = "Site",
    y = "Saplings Shannon-Weiner Index", tag = "(a)") + # optional to add tag = . Remove it when not necessary
   theme_beautiful() + 
   theme(                                    ## when adjusting tag 'a'
    legend.position = "right",
    plot.tag.position = c(0, 0.999)) # Adjust tag position (x, y)  

#Saving data
ggsave(Ssdv,
       filename = "C:/workspace/gumbo_dev/Plots/ ShannonD- Saplings SITE.png",
       width = 16, height = 14, units = "cm" )


### Creating plot for Shannon diversity at Site and Plot level
#Sp <- ggplot(diversity_data, aes(x = interaction(Site, Plot), y = Shannon_Diversity, fill = Site)) +
#  geom_bar(stat = "identity", show.legend = FALSE) +
#  theme_beautiful() +
#  labs(x = "Site-Plot", y = "Saplings Shannon-Weiner Index") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

## using PLOT ANNOTATIONS
Sp <- ggplot(diversity_data, aes(x = interaction(Site, Plot), y = Shannon_Diversity, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
 labs(
  x = "Site-Plot",
  y = "Saplings Shannon-Weiner Index", tag = "(b)") + # optional to add tag = . Remove it when not necessary
  theme_beautiful() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(                                    ## when adjusting tag 'a'
    legend.position = "right",
    plot.tag.position = c(0, 0.999)) # Adjust tag position (x, y)  

##Saving as png
ggsave(Sp,
       filename = "C:/workspace/gumbo_dev/Plots/ Saplings Shannon Diversity Index SP.png",
       width = 16, height = 10, units = "cm" )

## COMBINING SHANNON WEINER DIVERSITY PLOTS INTO A SINGLE MULTI PANEL FOR SITE AND SP
# Combining plots
Ac <- (Ssdv|Sp)

ggsave(Ac,
       filename = "C:/workspace/gumbo_dev/Plots/SHANNON Combinedplots -Saplings .png",
       width = 16, height = 10, units = "cm" )

############################################################################################
##########################################################################################
###########         CALCULATING SIMPSONS DIVERSITY INDEX FOR SAPLINGS

# Calculate species abundance per plot
Splot_data <- data %>%
  filter(!is.na(Saplings)) %>%  
  group_by(Site, Plot, Spp_name) %>% 
  summarise(abundance = n(), .groups = 'drop')

# Calculate Simpson's Index for each plot
Splot_diversity_simpson <- Splot_data %>%
  group_by(Site, Plot) %>%
  summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')


#write.csv(Tplot_diversity_simpson, "Trees Simpsons Diversity.csv")

# Calculate site-level Simpson diversity (mean across plots)
Ssite_diversity_simpson <- Splot_diversity_simpson %>%
  group_by(Site) %>%
  summarise(site_simpson_index = mean(simpson_index), .groups = 'drop')

# SITE - level Simpson diversity index
#SSIMP <- ggplot(Ssite_diversity_simpson, aes(x = Site, y = site_simpson_index)) +
#  geom_bar(stat = "identity", show.legend = FALSE) +
#  theme_beautiful() +
#  labs( y = "Simpson Diversity Index") +
#  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
#  plot_annotation("(a)")

## USING pLOT TAG ANNOTATION
SSIMP <- ggplot(Ssite_diversity_simpson, aes(x = Site, y = site_simpson_index)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs( y = "Saplings Simpson Diversity Index", tag = "(a)") + # optional to add tag = . Remove it when not necessary
    theme_beautiful() + 
     theme(                                    ## when adjusting tag 'a'
      legend.position = "right",
      plot.tag.position = c(0, 0.999)) # Adjust tag position (x, y)  

# Saving as png
ggsave(SSIMP,
       filename = "C:/workspace/gumbo_dev/Plots/ SAPLINGSIMP Diversity Index SITE.png",
       width = 16, height = 14, units = "cm" )


######  PLOT -level Simpson diversity index
#SPSimp <- ggplot(Splot_diversity_simpson, aes(x = interaction(Site, Plot), y = simpson_index, fill = Site)) +
#  geom_bar(stat = "identity", show.legend = FALSE) +
#  theme_beautiful() +
#  labs(x = "Site - Plot", y = "Saplings Simpson Diversity Index") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


###using plot tag annotations
SPSimp <- ggplot(Splot_diversity_simpson, aes(x = interaction(Site, Plot), y = simpson_index, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_beautiful() +
  labs(x = "Site - Plot", y = "Saplings Simpson Diversity Index", tag = "(b)") + # optional to add tag = . Remove it when not necessary
  theme_beautiful() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +     
  theme(                                    ## when adjusting tag 'a'
    legend.position = "right",
    plot.tag.position = c(0, 0.999)) # Adjust tag position (x, y)  

# Saving as png
ggsave(SPSimp,
       filename = "C:/workspace/gumbo_dev/Plots/ Saplings SIMPSON Diversity Index PLOT.png",
       width = 16, height = 14, units = "cm" )


#### COMBINING SIMPSON PLOTS INTO A SINGLE MULTI PANEL FOR SITE AND SP

# Combining plots
Ab <- (SSIMP|SPSimp)

ggsave(Ab,
       filename = "C:/workspace/gumbo_dev/Plots/SAPLING SIMP Combinedplots  .png",
       width = 16, height = 10, units = "cm" )




##CREATING MULTI PANEL PLOT FOR SAPLINGS
## Combine all six plots in a 3-row × 2-column layout. MULTIPANEL PLOT
Smulti_panel_plot <- (Scompa + Sppb) / (Ssdv + Sp) / (SSIMP + SPSimp) +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = "(",       # Add opening bracket
                  tag_suffix = ")" )&     # Add closing bracket)  
  theme(plot.tag.position = c(-0.05, 1)) 

#Smulti_panel_plot

ggsave(Smulti_panel_plot, 
       filename = "C:/workspace/gumbo_dev/Plots/SAPLINGS Multiplanel SP.png",
       width = 16, height = 14, units = "cm" )



############################################################################################

######################### DETERMINING BETA DIVERSITY FOR SAPLINGS

# Summarize species abundance for Trees only
sapling_species_matrix <- data %>%
  filter(!is.na(Saplings)) %>%            # Keep rows with Trees only
  group_by(Site, Spp_name) %>%         # Group by Site and Species name
  summarise(abundance = n(), .groups = 'drop') %>%  # Count occurrences
  spread(key = Spp_name, value = abundance, fill = 0)  # Create wide format matrix

# Replace NA values with 0 (if necessary, as safeguard)
sapling_species_matrix[is.na(sapling_species_matrix)] <- 0

# Compute Bray-Curtis dissimilarity
bc_dist_saplings <- vegdist(sapling_species_matrix[,-1], method = "bray")  # Exclude Site column

bc_dist_saplings

#####

###

# Create a data frame with labeled rows and columns
# Create the matrix with the given data
similarity_matrix <- matrix(c(0.00, 0.55, 0.65, 0.39, 0.53, 0.67,
                              0.55, 0.00, 0.58, 0.58, 0.64, 0.72,
                              0.65, 0.58, 0.00, 0.55, 0.51, 0.54,
                              0.39, 0.58, 0.55, 0.00, 0.36, 0.58,
                              0.53, 0.64, 0.51, 0.36, 0.00, 0.61,
                              0.67, 0.72, 0.54, 0.58, 0.61, 0.00   
                              ), nrow = 6, ncol = 6, byrow = TRUE)



# Assign row and column names
rownames(similarity_matrix) <- colnames(similarity_matrix) <- c("A", "B", "C", "D", "E", "F")

# Convert to a data frame for better readability
similarity_df <- as.data.frame(similarity_matrix)

# Print the table
print(similarity_df)

## SAVE AS DOC FILE     BRAY CURTIS 
# Convert matrix to dataframe
similarity_df <- as.data.frame(similarity_matrix)

# Add row names as a new column
similarity_df <- cbind(Site = rownames(similarity_df), similarity_df)

# Create a flextable
table_word <- flextable(similarity_df)

# Save as a Word document

doc <- read_docx()
doc <- body_add_flextable(doc, table_word)

##Saving as doc.
print(doc, target = "C:/workspace/gumbo_dev/Plots/Saplings Bray Curtis dissimilarity_matrix.docx")

##################################################################################################
#######################################################################################

# CALCULATING SORENSEN INDEX FOR SAPLINGS

# Step 1: Filter data for trees and calculate species presence/absence per site
sapling_data <- data %>%
  filter(!is.na(Saplings)) %>%
  group_by(Site, Spp_name) %>%
  summarise(Saplings_Count = n(), .groups = "drop") %>%
  mutate(Presence = as.integer(Saplings_Count > 0))  # Convert to 1 if present

# Step 2: Convert data into a species-site matrix
species_matrix <- sapling_data %>%
  select(Site, Spp_name, Presence) %>%
  pivot_wider(names_from = Spp_name, values_from = Presence, values_fill = list(Presence = 0))  # Ensure correct format


# Compute Sorensen similarity index
sorensen_matrix <- vegdist(species_matrix[,-1], method = "bray")  # Bray-Curtis is 1 - Sorensen

sorensen_matrix

# Convert dissimilarity to similarity (Sorensen = 1 - Bray-Curtis)
sorensen_similarity <- 1 - sorensen_matrix
sorensen_similarity


###SAVING AS A TABLE SAPLINGS
# Convert the Sorensen similarity matrix into a data frame
sorensen_df <- as.data.frame(as.matrix(sorensen_similarity))

# Add site names as a column (for better interpretation)
sorensen_df <- tibble::rownames_to_column(sorensen_df, var = "Site")
sorensen_df

# Save the results to a CSV file
# write.csv(sorensen_df, "Tree sorensen_similarity.csv", row.names = FALSE)

# Define the matrix ROUNDING OFF TO TWO DECIMAL PLACES
SP4Ldata <- matrix(c(
  0.0000000, 0.7000000, 0.6913580, 0.7346939, 0.6888889, 0.6938776,
  0.7000000, 0.0000000, 0.7384615, 0.6585366, 0.6486486, 0.6097561,
  0.6913580, 0.7384615, 0.0000000, 0.6265060, 0.6933333, 0.6024096,
  0.7346939, 0.6585366, 0.6265060, 0.0000000, 0.7173913, 0.7000000,
  0.6888889, 0.6486486, 0.6933333, 0.7173913, 0.0000000, 0.6739130,
  0.6938776, 0.6097561, 0.6024096, 0.7000000, 0.6739130, 0.0000000
), nrow = 6, byrow = TRUE)

# Round to two decimal places
rounded_data <- round(SP4Ldata, 2)

# Print the rounded matrix
print(rounded_data)

########## Define the SORENSON similarity matrix
Ssimilarity_matrix <- matrix(
  c(0.00, 0.70, 0.69, 0.73, 0.69, 0.69,
 0.70, 0.00, 0.74, 0.66, 0.65, 0.61,
 0.69, 0.74, 0.00, 0.63, 0.69, 0.60,
 0.73, 0.66, 0.63, 0.00, 0.72, 0.70,
 0.69, 0.65, 0.69, 0.72, 0.00, 0.67,
 0.69, 0.61, 0.60, 0.70, 0.67, 0.00),
   nrow = 6, ncol = 6, byrow = TRUE)
 

# Assign row and column names
rownames(Ssimilarity_matrix) <- colnames(Ssimilarity_matrix) <- c("A", "B", "C", "D", "E", "F")

# Convert to a matrix table
Ssimilarity_matrix <- as.matrix(Ssimilarity_matrix)

# Print the matrix
print(Ssimilarity_matrix)

# Convert matrix to dataframe
similarity_Sdf <- as.data.frame(Ssimilarity_matrix)

# Add row names as a new column
similarity_Sdf <- cbind(Site = rownames(similarity_Sdf), similarity_Sdf)

# Create a flextable
table_word <- flextable(similarity_Sdf)

# Save as a Word document

doc <- read_docx()
doc <- body_add_flextable(doc, table_word)

##Saving as doc.
print(doc, target = "C:/workspace/gumbo_dev/Plots/Sorenson Saplings similarity_matrix.docx")

#######################################################################################

##CALCULATING JACCARD DIVERSITY INDEX FOR SAPLINGS

# Step 1: Filter data for saplings and calculate species presence/absence per site
saplings_data <- data %>%
  filter(!is.na(Saplings)) %>%
  group_by(Site, Spp_name) %>%
  summarise(Saplings_Count = n(), .groups = "drop") %>%
  mutate(Presence = as.integer(Saplings_Count > 0))  # Convert to 1 if present

# Step 2: Convert data into a species-site matrix
species_matrix <- saplings_data %>%
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
jaccard_df

# Round all numeric columns to two decimal places
jaccard_df[,-1] <- round(jaccard_df[,-1], 2)
jaccard_df


## JACCARD DIVERSITY INDEX... replaced rows on the with with J   
Jsimilarity_matrix <- matrix(
  c(0.00, 0.54, 0.53, 0.58, 0.53, 0.53,
 0.54, 0.00, 0.59, 0.49, 0.48, 0.44,
 0.53, 0.59, 0.00, 0.46, 0.53, 0.43,
 0.58, 0.49, 0.46, 0.00, 0.56, 0.54,
 0.53, 0.48, 0.53, 0.56, 0.00, 0.51,
 0.53, 0.44, 0.43, 0.54, 0.51, 0.00),
 nrow = 6, ncol = 6, byrow = TRUE)

### Assign row and column names
rownames(Jsimilarity_matrix) <- colnames(Jsimilarity_matrix) <- c("A", "B", "C", "D", "E", "F")

# Convert to a matrix table
Jsimilarity_matrix <- as.matrix(Jsimilarity_matrix)

# Print the matrix
print(Jsimilarity_matrix)


# Convert matrix to dataframe
similarity_Jdf <- as.data.frame(Jsimilarity_matrix)

# Add row names as a new column
similarity_Jdf <- cbind(Site = rownames(similarity_Jdf), similarity_Jdf)

# Create a flextable
table_word <- flextable(similarity_Jdf)

# Save as a Word document

doc <- read_docx()
doc <- body_add_flextable(doc, table_word)

##Saving as doc.
print(doc, target = "C:/workspace/gumbo_dev/Plots/Saplings Jaccard similarity_matrix.docx")

#########################################################################
######################################

##########
#########################################################################################################


