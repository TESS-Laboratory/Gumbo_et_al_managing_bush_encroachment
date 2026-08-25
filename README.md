# Project overview

This repository supports research on managing bush encroachment on a holistically 
managed rangeland in a semi-arid African savanna, conducted in partnership with Shangani Holistic
as part of the Oppenheimer Programme in African Landscape Systems (OPALS; https://opals-exeter.org/). 

This study assessed how targeted bush-encroachment management interventions impacted 
woody plant densities across life-history stages and the cascading effects on grass
productivity and community composition.

The repository contains raw and processed data, analysis scripts, and output figures.

Contact: Tapiwa Gumbo (tg488@exeter.ac.uk) and/or Andrew Cunliffe (a.cunliffe@exeter.ac.uk)

---


## **Repository contents**

## Scripts

|Script |   Description |
|---|---|
| `01_QAQC_Grasses2026.R` | Quality assurance and quality control for 2026 grass survey data. |
| `02_QAQC_Woody.R` | Quality assurance and quality control for woody plant survey data. |
| `03_Taxonomic_Harmonisation.R` | Taxonomic harmonisation for grasses and woody plants using the World Flora Online backbone. |
| `04_Woody_and_Grasses_Analysis.R` | Statistical analysis and figure generation for woody plants and grasses. |

## Data

|File |    Description |
|---|---|
| `Coordinates/Coordinates_SHR.xlsx` | Geode coordinates for study subplots used in analysis. |
| `GEODE_Subplot_area` | Subplot areas calculated from precise Geode coordinates. |
| `March 2026/2Grasses2426updated.csv` | 2024 and 2026 grass survey data used for biomass calculations. |
| `March 2026/Grasses2426Updated1.csv` | 2024 and 2026 grass survey data. |
| `March 2026/WOODYP2426.csv` | 2024 and 2026 woody plant survey data. |
| `Harmonized Taxonomisation/Harmonized_WPspp26` | Taxonomic harmonisation output for woody plants. |
| `Harmonized Taxonomisation/Harmonized2026b_GRspp.csv` | Taxonomic harmonisation output for grasses. |
| `WFO_Backbone/` | World Flora Online backbone data used for taxonomic harmonisation. |

## Plots
|File |    Description |
|:---|:---|
| `Plots/` |    Figures generated from analysis scripts. | 



## **Getting started**

   Clone this repository and review the project overview and repository structure before running the analysis.

## 1. Cloning the repository:
a.From terminal

  git clone https://github.com/TESS-Laboratory/Gumbo_et_al_managing_bush_encroachment.git

b.From RStudio

   File → New Project → Version Control → Git
   Paste the repository URL: https://github.com/TESS-Laboratory/Gumbo_et_al_managing_bush_encroachment
   Choose a local directory → Create Project

## 2. Setting up the R environment

This project will use renv to ensure reproducibility. 

**Install the pinned package versions:**

*renv::restore()*

**If renv is not yet installed:**

   *install.packages("renv")*
   
   *renv::restore()*

## 3. Running the analysis

  Unless otherwise stated, scripts should be run in numerical order:

   1.`01_QAQC_Grasses2026.R`  
  
   2.`02_QAQC_Woody.R` 
  
   3.`03_Taxonomic_Harmonisation.R`  
  
   4.`04_Woody_and_Grasses_Analysis.R` 
   
  
     
     
