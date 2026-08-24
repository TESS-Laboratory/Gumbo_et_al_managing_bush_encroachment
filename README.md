# Project overview

This repository supports research on managing bush encroachment on a holistically 
managed rangeland in a semi-arid African savanna, conducted in partnership with Shangani Holistic
as part of the Oppenheimer Programme in African Landscape Systems (OPALS; https://opals-exeter.org/). 
The study uses vegetation survey data to analyse changes in woody plant and grass communities across treatments. 
The repository contains raw and processed data, analysis scripts, and output figures.

Contact: Tapiwa Gumbo (tg488@exeter.ac.uk) and/or Andrew Cunliffe (a.cunliffe@exeter.ac.uk)

---

# Repository contents

## Scripts

| Script | Description |
|---|---|
| `01_QAQC_Grasses2026.R` | Quality assurance and quality control for 2026 grass survey data. |
| `02_QAQC_Woody.R` | Quality assurance and quality control for woody plant survey data. |
| `03_Taxonomic_Harmonisation.R` | Taxonomic harmonisation for grasses and woody plants using the World Flora Online backbone. |
| `04_Woody_and_Grasses_Analysis.R` | Statistical analysis and figure generation for woody plants and grasses. |

## Data

| File | Description |
|---|---|
| `Coordinates/Coordinates_SHR.xlsx` | Geode coordinates for Shangani Holistic Ranch (SHR) subplots used in analysis. |
| `GEODE_Subplot_area` | Subplot areas calculated from precise Geode coordinates. |
| `March 2026/2Grasses2426updated.csv` | 2024 and 2026 grass survey data used for biomass calculations. |
| `March 2026/Grasses2426Updated1.csv` | 2024 and 2026 grass survey data. |
| `March 2026/WOODY2426.csv` | 2024 and 2026 woody plant survey data. |
| `Harmonized Taxonomisation/Harmonized_WPspp26` | Taxonomic harmonisation output for woody plants. |
| `Harmonized Taxonomisation/Harmonized2026b_GRspp.csv` | Taxonomic harmonisation output for grasses. |
| `chirps-v2.0.monthly` | Monthly CHIRPS rainfall data used in climate analysis. |
| `WFO_Backbone/` | World Flora Online backbone data used for taxonomic harmonisation. |

## Other directories

| File | Description |
|---|---|
| `Plots/` | Figures generated from analysis scripts. |
| `QUARTO_files/` | Output documents generated from Quarto. |
