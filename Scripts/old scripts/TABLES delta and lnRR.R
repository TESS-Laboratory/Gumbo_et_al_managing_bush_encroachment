
library(flextable)
library(officer)

#####
## summarising seedling delta per treatment
SeedSumm <- Seedlings %>% 
  group_by(Treatment, Fencing, Year) %>% 
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop")  

Seedlings_DeltaSUM <- SeedSumm %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Seeddens = dens_2026 - dens_2024)

# rounding off to 2 decimaL places
Seedlings_DeltaSUM <- Seedlings_DeltaSUM %>%
  mutate(across(where(is.numeric), round, 2))

# Convert object to table
ft <- flextable(Seedlings_DeltaSUM) %>%
  theme_booktabs() %>%
  autofit()

# Autofit column widths
doc <- read_docx()

 # Create Word document 
doc <- body_add_par(
  doc,
  "Table 1. Absolute change in mean seedling density"
)

# Add table
doc <- body_add_flextable(doc, ft)

#save
print(doc, target = "Plots/DeltaSEEDLING_Table.docx")


####################### Percent change relative to control SEEDLINGS

# Convert object to table
ft <- flextable(plot_data) %>%
  theme_booktabs() %>%
  autofit()

# Autofit column widths
pcntSeed <- read_docx()

# Create Word document 
pcntSeed <- body_add_par(
  pcntSeed,
  "Table 1. Relative change in mean seedling density",
  style = "heading 1"
)

# Add table
pcntSeed <- body_add_flextable(pcntSeed, ft)

#save
print(pcntSeed, target = "Plots/RelativeSEEDLING_Table.docx")


########################################################## SAPLINGS 

## summarising sAPling delta per treatment
SapSumm <- Saplings %>% 
  group_by(Treatment, Fencing, Year) %>% 
  summarise(mean_dens_ha = round(mean(density_ha),2),
            N    = n())

Saplings_DeltaSUM <- SapSumm %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Sapdens = dens_2026 - dens_2024)

# rounding off to 2 decimaL places
Saplings_DeltaSUM <- Saplings_DeltaSUM %>%
  mutate(across(where(is.numeric), round, 2))


# Convert object to table
sapft <- flextable(Saplings_DeltaSUM) %>%
  theme_booktabs() %>%
  autofit()


# Autofit column widths
sapdoc <- read_docx()

# Create Word document 
sapdoc <- body_add_par(
  sapdoc,
  "Table 1. Absolute change in mean sapling density"
)

# Add table
sapdoc <- body_add_flextable(sapdoc, sapft)

#save
print(sapdoc, target = "Plots/DeltaSAPLING_Table.docx")

#################################### RELATIVE CHANGE SAPLINGS

# Convert object to table
sapft <- flextable(Splot_data) %>%
  theme_booktabs() %>%
  autofit()

# Autofit column widths
pcntSap <- read_docx()

# Create Word document 
pcntSap <- body_add_par(
  pcntSap,
  "Table 1. Relative change in mean sapling density")

# Add table
pcntSap <- body_add_flextable(pcntSap, sapft)

#save
print(pcntSap, target = "Plots/RelativeSAPLING_Table.docx")


########################################################################### GRASSES
## GRASS BIOMASS

# Summarise delta_GRBiomass_bc (bias-corrected Biomass_mean_kg_ha) to Treatment x Fencing
GRBSumm_bc <- delta_GRBiomass_bc %>%
  group_by(Treatment, Fencing) %>%
  summarise(
    Biomass_2024 = round(mean(Biomass_2024, na.rm = TRUE), 2),
    Biomass_2026 = round(mean(Biomass_2026, na.rm = TRUE), 2),
    delta_GR     = round(mean(delta_GR,     na.rm = TRUE), 2),
    N            = n(),
    .groups = "drop"
  )

# Convert to table
grasft_bc <- flextable(GRBSumm_bc) %>%
  theme_booktabs() %>%
  autofit()

# Create Word document
grabdoc_bc <- read_docx()
grabdoc_bc <- body_add_par(
  grabdoc_bc,
  "Table 1. Absolute change in mean aboveground grass biomass (bias-corrected)"
)
grabdoc_bc <- body_add_flextable(grabdoc_bc, grasft_bc)

print(grabdoc_bc, target = "Plots/DeltaGrassB_Table.docx")

#################################### RELATIVE CHANGE GRASS BIOMASS (bias-corrected)

# GBpct_bc: % change from emmeans on GBiomlog_bc (LMM fitted to bias-corrected LRR)
grasft_pct <- flextable(GBpct_bc) %>%
  theme_booktabs() %>%
  autofit()

pcntGRB_bc <- read_docx()
pcntGRB_bc <- body_add_par(
  pcntGRB_bc,
  "Table 1. Relative change in aboveground grass biomass relative to control (bias-corrected)"
)
pcntGRB_bc <- body_add_flextable(pcntGRB_bc, grasft_pct)

print(pcntGRB_bc, target = "Plots/RelativeGB_Table.docx")


##################################### GRASS RICHNESS

SummGrassR <- Grasses %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2026)) %>%   # keep only pre/post years
  group_by(Treatment, Year, Fencing) %>%
  summarise(spp_richness = n_distinct(Species_name),
            # N    = n(),
            .groups = "drop")
#%>%
  # pivot_wider(
  #   names_from  = Year,
  #   values_from = spp_richness,
  #   names_glue  = "Y_{Year}"
  # )

##Pivot the two years side‑by‑side and compute Δ GRASS RICHNESS ─────────────
graR_delta <- SummGrassR %>%
  pivot_wider(names_from = Year, values_from = spp_richness, names_prefix = "Y") %>%
  mutate(delta = Y2026 - Y2024)


# rounding off to 2 decimaL places
SummGrassR <- SummGrassR %>%
  mutate(across(where(is.numeric), round, 2))


# Convert object to table
grarft <- flextable(graR_delta) %>%
  theme_booktabs() %>%
  autofit()


# Autofit column widths
grardoc <- read_docx()

# Create Word document 
grardoc <- body_add_par(
  grardoc,
  "Table 1. Absolute change in mean species richness"
)

# Add table
grardoc <- body_add_flextable(grardoc, grarft)

#save
print(grardoc, target = "Plots/DeltaGrassR_Table2.docx")

#################################### RELATIVE CHANGE GRASS Richness

# Convert object to table
graRft <- flextable(plot_data) %>%
  theme_booktabs() %>%
  autofit()

# Autofit column widths
pcntGRR <- read_docx()

# Create Word document 
pcntGRR <- body_add_par(
  pcntGRR,
  "Table 1. Relative change in grass species richness")

# Add table
pcntGRR <- body_add_flextable(pcntGRR, graRft)

#save
print(pcntGRR, target = "Plots/RelativeGRichness_Table.docx")

########################################################## GRASS DIVERSITY 

SummGrasD <- subplot_abundance %>%
  group_by( Treatment, Fencing, Year) %>%
  summarise(
    Shannon = diversity(total_abundance, index = "shannon"),
    Richness = n_distinct(Species_name),
    Total_grass = sum(total_abundance),
    .groups = "drop"
  )%>%
  mutate(Period = ifelse(Year == 2024, "Pre-treatment", "Post-treatment"))


##Pivot the two years side‑by‑side and compute Δ GRASS RICHNESS ─────────────
graD_delta <- SummGrasD %>%
  dplyr::select(Treatment, Fencing, Year, Shannon) %>%
  pivot_wider(
    names_from = Year,
    values_from = Shannon,
    names_prefix = "Shannon_"
  ) %>%
  mutate(
    delta_SW = Shannon_2026 - Shannon_2024
  ) 

# rounding off to 2 decimaL places
SummGrasD <- SummGrasD %>%
  mutate(across(where(is.numeric), round, 2))

# Convert object to table
graDft <- flextable(graD_delta) %>%
  theme_booktabs() %>%
  autofit()


# Autofit column widths
graDdoc <- read_docx()

# Create Word document 
graDdoc <- body_add_par(
  graDdoc,
  "Table 1. Absolute change in Shannon-Weiner index"
)

# Add table
graDdoc <- body_add_flextable(graDdoc, graDft)

#save
print(graDdoc, target = "Plots/DeltaGrassDIV_Table1.docx")


################################# RELATIVE CHANGE GRASS DIVERSITY

# Convert object to table
graDift <- flextable(GDplot_data) %>%
  theme_booktabs() %>%
  autofit()

# Autofit column widths
pcntGRD <- read_docx()

# Create Word document 
pcntGRD <- body_add_par(
  pcntGRD,
  "Table . Relative change in Shannon-Weiner index")

# Add table
pcntGRD <- body_add_flextable(pcntGRD, graDft)

#save
print(pcntGRD, target = "Plots/RelativeGDiversity_Table2.docx")



####################### PERCENT CHANGE GRASS DIVERSITY


# Create a formatted table
ft <- flextable(GDplot_data) %>%
  theme_vanilla() %>%  # Clean, professional look
  autofit() %>%        # Auto-adjust column widths
  bold(part = "header") %>%  # Bold headers
  # Optional: Add significance stars or custom formatting
  # bg(., bg = "grey90", part = "header")  # Header background color
  
  # Save as Word document
  doc <- read_docx() %>%
  body_add_flextable(ft) %>%
  body_add_par("", style = "Normal")  # Add a blank line

print(doc, target = "Plots/RelativeGDiversity_Table2.docx")




######################## TREE DENSITY

Trees_Summ <- Trees %>% 
  group_by(Treatment, Fencing, Year) %>% 
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop",
            N    = n())  


trees_DeltaSUM <- Trees_Summ %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Treedens = dens_2026 - dens_2024)

# rounding off to 2 decimaL places
trees_DeltaSUM <- trees_DeltaSUM %>%
  mutate(across(where(is.numeric), round, 2))


# Convert object to table
treeft <- flextable(trees_DeltaSUM) %>%
  theme_booktabs() %>%
  autofit()


# Autofit column widths
treedoc <- read_docx()

# Create Word document 
treedoc <- body_add_par(
  treedoc,
  "Table 1. Absolute change in mean tree density"
)

# Add table
treedoc <- body_add_flextable(treedoc, treeft)

#save
print(treedoc, target = "Plots/DeltaTREES2_Table.docx")

#################################### RELATIVE CHANGE TREES

# Convert object to table
treeft <- flextable(Tplot_data) %>%
  theme_booktabs() %>%
  autofit()

# rounding off to 2 decimaL places
trees_DeltaSUM <- trees_DeltaSUM %>%
  mutate(across(where(is.numeric), round, 2))

# Autofit column widths
pcnttree <- read_docx()

# Create Word document 
pcnttree <- body_add_par(
  pcnttree,
  "Table 1. Relative change in mean tree density")

# Add table
pcnttree <- body_add_flextable(pcnttree, treeft)

#save
print(pcnttree, target = "Plots/RelativeTREE_Table2.docx")


###################################################################################







