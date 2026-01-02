
### TABLE SHOWING RESULTS FOR SEEDLING DENSITY

# Calculate enhanced summary statistics
Seedsummary_stats <- Seedlings %>%
  group_by(Treatment, Fencing, Year) %>%
  summarise(
    N = n(),
    mean_density = mean(density_ha, na.rm = TRUE),
    sd_density = sd(density_ha, na.rm = TRUE),
    se_density = sd_density / sqrt(N), # Calculate Standard Error
    .groups = 'drop'  # This replaces ungroup()
  ) %>%
  # Create a single, nicely formatted character column for Mean ± SE
  mutate(
    `Mean Density (±SE)` = paste0(
      format(round(mean_density, 1), nsmall = 1), 
      " ± ", 
      format(round(se_density, 1), nsmall = 1)
    )
  ) %>%
  # Select and order the final columns for the table
  select(Treatment, Fencing, Year, N, `Mean Density (±SE)`, sd_density)

## Create the flextable object
pub_table2 <- Seedsummary_stats %>%
  # Rename columns for clarity and proper capitalization
  rename(
    Treatment = Treatment,
    `Fencing status` = Fencing,
    Year = Year,
    `Sample Size (N)` = N,
    `Std. Dev.` = sd_density
  ) %>%
  mutate(`Year` = gsub(",", "", `Year`))%>%
# Remove the raw sd column if you only want Mean ± SE
select(-`Std. Dev.`) %>% 
  flextable() %>%
  # Set the overall table theme
  theme_vanilla() %>%
  # Add top border to the entire table
  border_outer(part = "all", border = fp_border(width = 1)) %>%
  # REMOVE all horizontal lines from the body (data rows)
  border_remove %>%
  # Remove horizontal lines from the header
 # border_remove(part = "header") %>%
  # Add TOP border to the column header row (Treatment, Kraaling row)
  border(i = 1, part = "header", border.top = fp_border(width = 1.5)) %>%
  # Add BOTTOM border to the column header row (Treatment, Kraaling row)
  border(i = 1, part = "header", border.bottom = fp_border(width = 1.5)) %>%
  # Center align all headers and body cells
  align(align = "center", part = "all") %>%
  # Bold the header row
  bold(part = "header") %>%
  # Bold and center the title
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "center") %>%
  # Adjust the font and size
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 12, part = "all") %>%
  # Automatically adjust column widths to fit content
  autofit()

# Save to Word
save_as_docx(pub_table2, path = "Plots/Seedling_Density_Summary_Table.docx")



############################################################################

#SAPLING DENSITY 

#Summary stats for saplings 
Saplsummary_stats <- Saplings %>%
  group_by(Treatment, Year) %>%
  summarise(
    N = n(),                                   # number of observations per treatment
    mean_density = mean(density_ha, na.rm = TRUE),
    sd_density = sd(density_ha, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculate enhanced summary statistics
Saplsummary_stats <- Saplings %>%
  group_by(Treatment, Fencing, Year) %>%
  summarise(
    N = n(),
    mean_density = mean(density_ha, na.rm = TRUE),
    sd_density = sd(density_ha, na.rm = TRUE),
    se_density = sd_density / sqrt(N), # Calculate Standard Error
    .groups = 'drop'  # This replaces ungroup()
  ) %>%
  # Create a single, nicely formatted character column for Mean ± SE
  mutate(
    `Mean Density (±SE)` = paste0(
      format(round(mean_density, 1), nsmall = 1), 
      " ± ", 
      format(round(se_density, 1), nsmall = 1)
    )
  ) %>%
  # Select and order the final columns for the table
  select(Treatment, Fencing, Year, N, `Mean Density (±SE)`, sd_density)

## Create the flextable object
Spub_table <- Saplsummary_stats %>%
  # Rename columns for clarity and proper capitalization
  rename(
    Treatment = Treatment,
    `Fencing status` = Fencing,
    Year = Year,
    `Sample Size (N)` = N,
    `Std. Dev.` = sd_density
  ) %>%
  mutate(`Year` = gsub(",", "", `Year`))%>%
  # Remove the raw sd column if you only want Mean ± SE
  select(-`Std. Dev.`) %>% 
  flextable() %>%
  # Set the overall table theme
  theme_vanilla() %>%
  # Add top border to the entire table
  border_outer(part = "all", border = fp_border(width = 1)) %>%
  # REMOVE all horizontal lines from the body (data rows)
  border_remove %>%
  # Remove horizontal lines from the header
  # border_remove(part = "header") %>%
  # Add TOP border to the column header row (Treatment, Kraaling row)
  border(i = 1, part = "header", border.top = fp_border(width = 1.5)) %>%
  # Add BOTTOM border to the column header row (Treatment, Kraaling row)
  border(i = 1, part = "header", border.bottom = fp_border(width = 1.5)) %>%
  # Center align all headers and body cells
  align(align = "center", part = "all") %>%
  # Bold the header row
  bold(part = "header") %>%
  # Bold and center the title
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "center") %>%
  # Adjust the font and size
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 12, part = "all") %>%
  # Automatically adjust column widths to fit content
  autofit()

# Save to Word
save_as_docx(Spub_table, path = "Plots/Sapling_Density_Summary_Table.docx")


######################################################################################

# GRASS HEIGHT 

# Calculate summary statistics with enhanced metrics
summary_Gr <- GrassHEIGHT %>%
  filter(!is.na(DPM_Height),
         Year %in% c(2024, 2025),!Treatment %in% c("TFB")) %>% 
  group_by(Treatment, Fencing, Year)%>% 
  summarise(
    N = n(),
    mean_Height = mean(DPM_Height, na.rm = TRUE),
    sd_Height = sd(DPM_Height, na.rm = TRUE),
    se_Height = sd_Height / sqrt(N),
    .groups = 'drop'
  ) %>%
  # Create formatted mean ± SE column
  mutate(
    `Mean Height (cm ± SE)` = paste0(
      sprintf("%.1f", round(mean_Height, 1)),
      " ± ",
      sprintf("%.1f", round(se_Height, 1))
    )
  ) %>%
  mutate(`Year` = gsub(",", "", `Year`))%>%
  select(Treatment, Fencing, Year, N, `Mean Height (cm ± SE)`)

# Create publication-ready table
grassH_table <- summary_Gr %>%
  flextable() %>%
  # Set clean theme and borders
  theme_vanilla() %>%
  border_remove() %>%
  # Add outer border
 # border_outer(part = "all", border = fp_border(width = 1)) %>%
  # Add top and bottom borders to header only
  border(i = 1, part = "header", 
         border.top = fp_border(width = 1.0),
         border.bottom = fp_border(width = 1.0)) %>%
  # Center alignment
  align(align = "center", part = "all") %>%
  # Header styling
  bold(part = "header") %>%
  # Add professional title
  #add_header_lines(values = "Table 1. Grass height response to experimental treatments across consecutive growing seasons (2014-2015)") %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "center") %>%
  # Font formatting
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 12, part = "all") %>%
  # Column width optimization
  autofit() 

# Display table
grassH_table


# Save to Word
save_as_docx(grassH_table, path = "Plots/Grass height_Summary_Table.docx")




################### GRASS SPECIES RICHNESS
#### Grass Species richness

Grass_Fb <- Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025),
         !Treatment %in% c("TFB")) %>%   
  group_by(Treatment, Fencing, Year) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop")%>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))%>%
  mutate(Period = factor(Period, levels = c("Pre-treatment", "Post-treatment")))

###DELTA

Grass_F <- Grasses %>%
filter(!is.na(Species_name),
       Year %in% c(2024, 2025),!Treatment %in% c("TFB")) %>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Year, Fencing) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop")

#Delta
grassR_delta <- Grass_F %>%
pivot_wider(names_from = Year, values_from = spp_richness, names_prefix = "Y") %>%
  mutate(delta = Y2025 - Y2024)

#Visualise: boxplot of Δ‑  species richness by interaction
GrRRbx2 <- ggplot(grassR_delta,
                  aes(x = Treatment, y = delta)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-5, 10), breaks = seq(-5, 10, by = 2)) +
  labs(x = "Treatment", y = "Change in grass species richness") +
  theme_beautiful()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))




#### STATISTICAL ANALYSIS TO TEST DIFFERENCE
# Summary statistics table
summary_stats <- Grass_Fb %>%
  group_by(Treatment, Period, Fencing) %>%
  summarise(
    n = n(),
    mean_richness = mean(spp_richness),
    sd_richness = sd(spp_richness),
    se_richness = sd_richness / sqrt(n),
    .groups = "drop"
  )%>%
  # Format numbers for scientific publication
  mutate(
    mean_richness = round(mean_richness, 2),
    sd_richness = round(sd_richness, 2),
    se_richness = round(se_richness, 2),
    # Create combined mean ± SE column (common in scientific tables)
    mean_se = paste0(mean_richness, " ± ", se_richness)
  )


scientific_table <- summary_stats %>%
  select(Treatment, Period, Fencing, n, mean_se, mean_richness, sd_richness, se_richness) %>%
  flextable() %>%
  set_caption("Table 1: Species Richness Summary Statistics by Treatment, Period and Fencing Intensity") %>%
  set_header_labels(
    Treatment = "Treatment",
    Period = "Sampling Period",
    Fencing = "Fencing",
    n = "n",
    mean_se = "Mean ± SE",
    mean_richness = "Mean",
    sd_richness = "SD",
    se_richness = "SE"
  ) %>%
  add_header_row(
    values = c("", "", "", "Sample", "Species Richness", "", "", ""),
    colwidths = c(1, 1, 1, 1, 1, 1, 1, 1)
  ) %>%
  theme_booktabs() %>%
  align(align = "center", part = "all") %>%
  align(align = "left", j = 1:3) %>%  # Left align categorical variables
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  autofit() %>%
  padding(padding = 3, part = "all")



# Display the summary table
print(summary_stats)
# For a nicer table
kable(summary_stats, caption = "Species Richness Summary by Treatment, Period, and Kraaling")

### Create a flextable
ft <- flextable(summary_stats) %>%
  set_caption("Species Richness Summary by Treatment, Period, and Kraaling") %>%
  theme_box() %>%
  autofit()

# Save as Word document
 save_as_docx(scientific_table, path = "Plots/Species_Richness_Summary.docx")


 
 ###### CREATING A LIST OF 5 COMMON SPECIES BY SUBPLOT
## Most common species by treatment, year AND kraaling - ranked by frequency
 common_species2 <- Grasses %>%
   filter(!is.na(Species_name)) %>%  # Exclude NA species
   mutate(`Year` = gsub(",", "", `Year`))%>%
   group_by(Treatment, Year, Fencing, Species_name) %>%
   summarise(
    # n_plots = n_distinct(Subplot),
     frequency = n(),
     .groups = "drop"
   ) %>%
   group_by(Treatment, Year, Fencing) %>%
   arrange(desc(frequency), .by_group = TRUE) %>%  # Changed from n_plots to frequency
   mutate(rank = row_number()) %>%
   filter(rank <= 5) %>%
   select(Treatment, Year, Fencing, Rank = rank, Species_name,frequency) %>%
   arrange(Treatment, Year, Fencing, Rank)
 
 # Display results
 print(common_species2, n = 100)
 
 # Save as Word document
 common_species2 %>%
   flextable() %>%
   set_caption("Top 5 Most Common Species (Ranked by Frequency)") %>%
   theme_booktabs() %>%
   # Font formatting
   font(fontname = "Times New Roman", part = "all") %>%
   fontsize(size = 12, part = "all") %>%
   autofit() %>%
   save_as_docx(path = "Plots/Top five_Species_By_Frequency.docx")
 
 
 
 #### GRASS SHANNON-WEINER DIVERSITY
 grSWdiversity <- GrSWeiner %>%
   group_by(Site, Plot, Subplot,Treatment, Year, Fencing) %>%                   # Group by Site and Plot
   summarise(
     Shannon_Diversity = -sum((Spp_count / sum(Spp_count)) * log(Spp_count / sum(Spp_count))),
     .groups = "drop")%>%
   mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))
 
#Summary grass diversity 
 GrassDsummary_stats <- grSWdiversity %>%
   group_by(Treatment,Fencing, Year) %>%
   summarise(
     N = n(),                                   # number of observations per treatment
     ShannonDiversity = mean(Shannon_Diversity, na.rm = TRUE),
     sd_diversity = sd(Shannon_Diversity, na.rm = TRUE)
   ) %>%
   ungroup()
 
 
 
 
   summary_statsGSW <- grSWdiversity %>%
   group_by(Treatment, Year, Fencing) %>%
   summarise(
     n = n(),
     mean_Shannon = mean(Shannon_Diversity),
     sd_Shannon = sd(Shannon_Diversity),
     se_Shannon = sd_Shannon / sqrt(n),
     .groups = "drop"
   )%>%
   # Format numbers for scientific publication
   mutate(
     mean_Shannon = round(sd_Shannon, 2),
     sd_Shannon = round(sd_Shannon, 2),
     # Create combined mean ± SE column (common in scientific tables)
     mean_se = paste0(mean_Shannon, " ± ", se_Shannon)
   )

   

   # Calculate diversity metrics with standard error
   GrassDsummary_stats <- grSWdiversity %>%
     group_by(Treatment,Fencing, Year) %>%
     summarise(
       N = n(),                                   # number of observations per treatment
       ShannonDiversity = mean(Shannon_Diversity, na.rm = TRUE),
       sd_diversity = sd(Shannon_Diversity, na.rm = TRUE),
       se_diversity = sd_diversity / sqrt(N),     # Standard error
       .groups = "drop"
     ) %>%
     # Create combined Mean ± SE column
     mutate(
       Mean_SE = paste0(round(ShannonDiversity, 3), " ± ", round(se_diversity, 3))
     ) %>%
     # Add Period for better organization
     mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"),
            Period = factor(Period, levels = c("Pre-treatment", "Post-treatment"))) %>%
     # Sort logically
     arrange(Treatment, Period, Fencing)
   
  # Create scientific table
   
   scientific_table <- GrassDsummary_stats %>%
     select(Treatment, Period, Fencing, N, Mean_SE, ShannonDiversity, sd_diversity, se_diversity) %>%
     flextable() %>%
     set_caption("Table 1: Shannon Diversity Index by Treatment, Period and Kraaling Intensity") %>%
     #set_header_labels(
      #Treatment = "Treatment",
      # Period = "Sampling Period",
       #Fencing = "Fencing",
       #N = "n",
       #Mean_SE = "Mean Shannon-Weiner diversity ± SE"
       #ShannonDiversity = "Mean Diversity",
       #sd_diversity = "SD",
       #se_diversity = "SE"
     #) %>%
     add_header_row(
       values = c("", "", "", "Sample", "Shannon Diversity", "", "", ""),
       colwidths = c(1, 1, 1, 1, 1, 1, 1, 1)
     ) %>%
     theme_booktabs() %>%
     align(align = "center", part = "all") %>%
     align(align = "left", j = 1:3) %>%  # Left align categorical variables
     bold(part = "header") %>%
     fontsize(size = 10, part = "all") %>%
     autofit()
   
   # Display table
   print(scientific_table, preview = "docx")
   
   # Save as Word document
   save_as_docx(scientific_table, path = "Shannon_Diversity_Table.docx") 
   
   
   
   
   ###### Post hoc results converted to word document
   
   contrasts_summary <- summary(Grass_diversity$contrasts)

   
   # Simplified version focusing on key results
   simple_contrasts_table <- contrasts_summary %>%
     as.data.frame() %>%
     select(contrast, estimate, SE, p.value) %>%
     flextable() %>%
     set_caption("Table X: Treatment Effect Estimates and P-values") %>%
     set_header_labels(
       contrast = "Comparison",
       estimate = "Difference",
       SE = "Standard Error", 
       p.value = "P-value"
     ) %>%
     colformat_num(j = c("estimate", "SE"), digits = 3) %>%
     colformat_num(j = "p.value", digits = 4) %>%
     theme_zebra() %>%
     align(align = "center", part = "all") %>%
     align(align = "left", j = "contrast") %>%
     bold(part = "header") %>%
     autofit()
   
   save_as_docx(simple_contrasts_table, path = "Plots/Simple_Contrasts_Table.docx")
 