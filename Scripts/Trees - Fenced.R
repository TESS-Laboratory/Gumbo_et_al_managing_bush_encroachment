library(tidyverse)
library(vegan)
library(multcompView)
library(patchwork)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(Matrix)
library(lme4)
library(emmeans)
library(here) ## MOST IMPORTANT NOT TO FORGET THIS ONE in quarto
library(robustbase)
library(sjPlot)
library(flextable)
library(officer)
library(glmmTMB)
library(performance)  # model diagnostics

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

#Loading data
Ttdata <- read_csv("DATA/March2025/woody_with_separate_columns25.csv")

Ttdata <- Ttdata %>%
  filter(!is.na(Trees) %>%  
           mutate(
             Year = as.factor(Year),
             Site = as.factor(Site),
             Plot = as.factor(Plot),
             Subplot = as.factor(Subplot),
             Treatment = as.factor(Treatment),
             Fenced = as.factor(Fenced),
             Trees = as.numeric(Trees) # Ensure numeric
           )
         
        # Check missing values
         summary(Ttdata$Trees)
         
     ###2. Create Pre/Post Variable
  Ttdata <- Ttdata %>%
  mutate(period = ifelse(as.numeric(as.character(Year)) < 2025, "PRE", "POST")) %>%
  mutate(period = factor(period, levels = c("PRE", "POST"))
         
 ### Summarize by Subplot or Plot..Average height per subplot and period:
         
 summary_Ttrdata <- Ttdata %>%
 group_by(Site, Plot, Subplot, Treatment, period, Fenced) %>%
           summarise(mean_height = mean(Trees, na.rm = TRUE)) %>%
           ungroup()
         
####4. Calculate Change in Height (Post - Pre)
Ttheight_change <- summary_Ttrdata %>%
 pivot_wider(names_from = period, values_from = mean_height) %>%
  mutate(delta = POST - PRE)
         
### Compare Treatment Effects (ANOVA on Delta)
Ttanova_model <- aov(delta ~ Treatment, data = Ttheight_change)
 summary(Ttanova_model)
  # the results show that the treatment groups do not differ significantly in their effect on tree height
         
## Optional: Tukey post-hoc test
   TukeyHSD(Ttanova_model)
         
#### USE OF Mixed-Effects Model (Handles Repeated Measures)
 #This is more robust as it uses the original data, accounts for plot/subplot as random effects, and tests interaction between time and treatment:
         
  Ttlmm <- lmer(Trees ~ period * Treatment * Fenced + (1 | Site/Plot/Subplot), data = Ttdata)
         summary(Ttlmm)
         
  ##Plotting boxplot for seedlings before and after treatment
ggplot(Ttdata[!is.na(Ttdata$Trees), ],
 aes(x = Fenced, y = Trees, fill = Fenced)) + facet_wrap(~Treatment)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
           #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
 geom_hline(yintercept = 0, linetype = "dashed") +
 labs(x = "Treatment", y = "Tree max.height (m)") +
 theme_beautiful() +
 theme(legend.position = "none")
         
  # ### Violin plot for  height
ggplot(Ttdata[!is.na(Ttdata$Trees), ],
        aes(x = Fenced, y = Trees, fill = Fenced)) + facet_wrap(~Treatment)+
   geom_violin(trim = FALSE)+
   labs(x = "Treatment", y = "Tree max.height (m)") +
   theme_beautiful() +
   theme(legend.position = "none")
   
#saving violin plot
 #ggsave(TV,filename ="Plots/Height TREES FENCED Violinplot.png",
  #     width = 16, height = 14, units = "cm")


##################### DELTA CHANGE TREE HEIGHT
delta_density <- Tf %>%                             # your raw data frame
  group_by(Site, Plot, Subplot, Treatment, Fenced, Year) %>% 
  summarise(Trees = sum(!is.na(Trees)), .groups = "drop") %>%   # count trees
  mutate(Trees = Trees * 10000 / 600) %>%                       # m² → ha
  

Delta_height <- Ttdata %>%
  group_by(Site, Plot, Subplot, Treatment, Year, Fenced) %>%
  summarise(mean_height = mean(Trees, na.rm = TRUE)) %>%
  ungroup() %>%
filter(Year %in% c(2024, 2025)) %>%                           # keep the two years
  pivot_wider(names_from = Year,
              values_from = mean_height,
              names_glue = "d_{Year}") %>%                   
  mutate(delta_height25 = d_2024 - d_2025)   


# ### Violin plot for tree density
TH <- ggplot(Delta_height,
              aes(x = Fenced, y = delta_height25, fill = Fenced)) + facet_wrap(~Treatment)+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fencing", y = "Δ Trees max. height (m)") +
  theme_beautiful() +
  theme(legend.position = "none")

#saving violin plot
ggsave(TH,filename ="Plots/DELTA TREE Height FENCED Violinplot.png",
       width = 16, height = 14, units = "cm")


############################################## TREES  DENSITY /HA CHANGE 

Tf <- read_csv("DATA/March2025/woody_with_separate_columns25.csv")
#Group by Site and sum Seedlings, Saplings, and Trees
# Count the number of non-NA entries for Seedlings, Saplings, and Trees per site

site_counts <- Tf %>%
  group_by(Site, Plot, Subplot, Treatment, Year, Fenced) %>%
  summarise(
    Trees = sum(!is.na(Trees)))%>%
  mutate(across(c(Trees), ~ . * 10000 / 600))


# Reshape data for plotting
site_density_long <- site_counts %>%
  pivot_longer(cols = c(Trees), 
               names_to = "Tree", values_to = "Density")

# Plot the boxplot
ggplot(site_density_long,
       aes(x = Fenced, y = Density, fill = Fenced)) + facet_wrap(~Treatment)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fenced", y = "Trees per hectare") +
  theme_beautiful() +
  theme(legend.position = "none")


############################### DELTA CHANGE TREE DENSITY

delta_density <- Tf %>%                             # your raw data frame
  group_by(Site, Plot, Subplot, Treatment, Fenced, Year) %>% 
  summarise(Trees = sum(!is.na(Trees)), .groups = "drop") %>%   # count trees
  mutate(Trees = Trees * 10000 / 600) %>%                       # m² → ha
  filter(Year %in% c(2024, 2025)) %>%                           # keep the two years
  pivot_wider(names_from = Year,
              values_from = Trees,
              names_glue = "d_{Year}") %>%                   
  mutate(delta_dens_24_25 = d_2024 - d_2025)   

# Plot the box plot
ggplot(delta_density,
       aes(x = Fenced, y = delta_dens_24_25, fill = Fenced)) + facet_wrap(~Treatment)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fenced", y = "Trees per hectare") +
  theme_beautiful() +
  theme(legend.position = "none")

# plot violin
# ### Violin plot for tree density
Trd <- ggplot(delta_density,
aes(x = Fenced, y = delta_dens_24_25, fill = Fenced)) + facet_wrap(~Treatment)+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fenced", y = "Δ Trees per hectare") +
  theme_beautiful() +
  theme(legend.position = "none")

#saving violin plot
ggsave(Trd,filename ="Plots/TREE Density FENCED Violinplot.png",
     width = 16, height = 14, units = "cm")


###########################################################################################
#################################### SAPLING DENSITY  SAPLING DENSITY SALING DENSITY

# lLoad data
SapF <- read_csv("DATA/March2025/WoodyPC.csv")

# Converting heights to saplings and seedlings and excluding cut stump. 
SapF <- SapF %>% 
  mutate(
    woody_cat = case_when(
      Woody_class == "Cut stump"          ~ "Cut stump",
      between(`Max_height(m)`, 0.05, 0.50)          ~ "Seedlings",
      between(`Max_height(m)`, 0.51, 1.49)          ~ "Saplings",
      TRUE                                 ~ NA_character_
    )
  )

###
# Step 1: Filter seed only and years 2021 and 2022
seed_df <- SapF %>%
  filter(
    woody_cat == "Seedlings",
    Year %in% c(2024, 2025)
  )

# Step 2: Create a boxplot for seedlings height 

ggplot(seed_df,
       aes(x = Fenced, y =`Max_height(m)`, fill = Fenced)) + facet_wrap(~Treatment)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fenced", y = "Seedlings Max_height(m)") +
  theme_beautiful() +
  theme(legend.position = "none")


############# Compute Δ‑height (2024 − 2025) per Site/Plot/Subplot/Treatment/Fenced ----
# Take the mean height within each grouping for each year before differencing.

delta_height <- seed_df %>%
  group_by(Site, Plot, Subplot, Treatment, Fenced, Year) %>%
  summarise(mean_height = mean(`Max_height(m)`, na.rm = TRUE), .groups = "drop_last") %>%
  pivot_wider(
    names_from  = Year,
    values_from = mean_height,
    names_glue  = "height_{Year}"
  ) %>%
  mutate(delta_S = height_2025 - height_2024) %>%
  drop_na(delta_S)   # keep groups where both years are present

# --- 4. Visualise: boxplot of Δ‑height by Treatment & Fencing -------------

Sdc <- ggplot(delta_height,
       aes(x = Fenced, y = delta_S, fill = Fenced)) + facet_wrap(~Treatment)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fenced", y = "Δ Seedlings Max_height(m)") +
  theme_beautiful() +
  theme(legend.position = "none")

#saving BOXPLOT plot
ggsave(Sdc,filename ="Plots/Delta Seedlings Height FENCED boxplotplot.png",
       width = 16, height = 14, units = "cm")


#### ### Violin plot for delta seedling height
 Sdv <- ggplot(delta_height,
 aes(x = Fenced, y = delta_S, fill = Fenced)) + facet_wrap(~Treatment)+  
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fenced", y = "Δ Seedling max. height (m)") +
  theme_beautiful() +
  theme(legend.position = "none")

 #saving BOXPLOT plot
 ggsave(Sdv,filename ="Plots/Delta Seedlings Height FENCED Violinplot.png",
        width = 16, height = 14, units = "cm")
 
 ########
 #############################################SEEDLINGS DENSITY SEEDLING DENSITY
 ######## ── 1. Density per subplot‑year ───────────────────────────────────────────
 seed_sub <- SapF %>% 
   filter(woody_cat == "Seedlings", Year %in% c(2024, 2025)) %>% 
   count(Site, Plot, Subplot, Treatment, Fenced, Year, name = "Seedlings") %>% 
   mutate(density_ha = Seedlings * 10000 / 600)     # convert to ha⁻¹
 
 # ── 2. Aggregate to Treatment × Fenced × Year (mean density) ──────────────
 seed_treat <- seed_sub %>% 
   group_by(Site, Plot, Subplot, Treatment, Fenced, Year) %>% 
   summarise(mean_dens_ha = mean(density_ha), .groups = "drop")  # ← use sum() if preferred
 
 # ── 3. Pivot the two years side‑by‑side and compute Δ‑density ─────────────
 Seedling_Delta <- seed_treat %>% 
   pivot_wider(names_from  = Year,
               values_from = mean_dens_ha,
               names_glue  = "dens_{Year}") %>% 
   mutate(delta_Sdens = dens_2025 - dens_2024)        # 2022 − 2021
 
 ##Mixed models analysis for seedlings
   #sd_delta <- lmer(delta_Sdens~ Treatment * Fenced +(1|Site), data = Seedling_Delta)
   #summary(sd_delta)
 
 
 # ── 4. Boxplot of change in density by Treatment & Fencing ────────────────
 Seb <- ggplot(Seedling_Delta,
        aes(x = Fenced, y = delta_Sdens, fill = Fenced)) + facet_wrap(~Treatment)+ 
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Δ Seedlings density per ha") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 
 #saving BOXPLOT plot
 ggsave(Seb,filename ="Plots/Delta Seedlings density FENCED Boxplotplot.png",
        width = 16, height = 14, units = "cm")

 ########################################
 ########################################### SAPLINGS SAPLINGS SAPLINGS HEIGHT
 
 # Step 1: Filter saplings only and years 
 saplings_df <- SapF %>%
   filter(
     woody_cat == "Saplings",
     Year %in% c(2024, 2025)
   )
 
 # Step 2: Create a boxplot for saplings height 
 
 Sdp <- ggplot(saplings_df,
        aes(x = Fenced, y =`Max_height(m)`, fill = Fenced)) + facet_wrap(~Treatment)+ 
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Saplings max_height(m)") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 #saving BOXPLOT plot
 ggsave(Sdp,filename ="Plots/Saplings Height FENCED boxplotplot.png",
        width = 16, height = 14, units = "cm")
 
 ############# Compute saplings Δ‑height (2024 − 2025) per Site/Plot/Subplot/Treatment/Fenced ----
 # Take the mean height within each grouping for each year before differencing.
 
 delta_heightSp <- saplings_df %>%
   group_by(Site, Plot, Subplot, Treatment, Fenced, Year) %>%
   summarise(mean_height = mean(`Max_height(m)`, na.rm = TRUE), .groups = "drop_last") %>%
   pivot_wider(
     names_from  = Year,
     values_from = mean_height,
     names_glue  = "height_{Year}"
   ) %>%
   mutate(delta_S = height_2025 - height_2024) %>%
   drop_na(delta_S)   # keep groups where both years are present
 
 # --- 4. Visualise: boxplot of Δ‑height by Treatment & Fencing -------------
 
 Spc <- ggplot(delta_heightSp,
               aes(x = Fenced, y = delta_S, fill = Fenced)) + facet_wrap(~Treatment)+ 
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Δ Seedlings Max_height(m)") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 #saving BOXPLOT plot
 ggsave(Spc,filename ="Plots/Delta Saplings Height FENCED boxplotplot.png",
        width = 16, height = 14, units = "cm")
 
 
 #### ### Violin plot for delta seedling height
 Spv <- ggplot(delta_heightSp,
               aes(x = Fenced, y = delta_S, fill = Fenced)) + facet_wrap(~Treatment)+  
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Δ Saplings max. height (m)") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 #saving BOXPLOT plot
 ggsave(Spv,filename ="Plots/Delta Saplings Height FENCED Violinplot.png",
        width = 16, height = 14, units = "cm")
 
 
 ############################################################
 ####################################################SAPLING DENSITY SAPLING DENSITY
 ######## ── 1. Density per subplot‑year ───────────────────────────────────────────
 sapling_sub <- SapF %>% 
   filter(woody_cat == "Saplings", Year %in% c(2024, 2025)) %>% 
   count(Site, Plot, Subplot, Treatment, Fenced, Year, name = "Saplings") %>% 
   mutate(density_ha = Saplings * 10000 / 600)     # convert to ha⁻¹
 
 # ── 2. Aggregate to Treatment × Fenced × Year (mean density) ──────────────
 sap_treat <- sapling_sub %>% 
   group_by(Site, Plot, Subplot, Treatment, Fenced, Year) %>% 
   summarise(mean_dens_ha = mean(density_ha), .groups = "drop")  # ← use sum() if preferred
 
 # ── 3. Pivot the two years side‑by‑side and compute Δ‑density ─────────────
 Sapling_Delta <- sap_treat %>% 
   pivot_wider(names_from  = Year,
               values_from = mean_dens_ha,
               names_glue  = "dens_{Year}") %>% 
   mutate(delta_Spdens = dens_2025 - dens_2024)   
 
 ### Removing non‑finite (NA, ±Inf) *and* (if on log scale) non‑positive ──
 sapling_delta_clean <-  Sapling_Delta %>% 
   filter(
     is.finite(delta_Spdens),   # drop NA / Inf / -Inf
     delta_Spdens != 0          # <- only if you’re using a log scale; otherwise omit
   )
 
##Mixed models analysis
 #sp_delta <- lmer(delta_Spdens~ Treatment * Fenced +(1|Site), data =sapling_delta_clean)
 #summary(sp_delta)
 
 # ── 4. Boxplot of change in density by Treatment & Fencing ────────────────
 Spb <- ggplot(sapling_delta_clean,
               aes(x = Fenced, y = delta_Spdens, fill = Fenced)) + facet_wrap(~Treatment)+ 
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Δ Saplings density per ha") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 #saving BOXPLOT plot
 ggsave(Spb,filename ="Plots/Delta Saplings density FENCED Boxplotplot.png",
        width = 16, height = 14, units = "cm")
 
 
 ###### ### Violin plot for delta sapling density
 Spv1 <- ggplot(sapling_delta_clean,
               aes(x = Fenced, y = delta_Spdens, fill = Fenced)) + facet_wrap(~Treatment)+   
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Δ Saplings density per ha") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 #saving VIOLIN PLOT plot
 ggsave(Spv1,filename ="Plots/Delta Saplings density FENCED Violinplot.png",
        width = 16, height = 14, units = "cm")
 
 
 ##########################################################
 ######################################################## RESPROUTS RESPROUTS
 
 # Step 1: Filter Cut stumps only and years 
 resprouts_df <- SapF %>%
   filter(
     woody_cat == "Cut stump",
     Year %in% c(2025),
     !Treatment %in% c("C", "F")  # exclude the two treatments
   )

 ##Mixed models analysis for resprouts
 res_delta <- lmer(No_of_resprouts~ Treatment * Fenced +(1|Site), data = resprouts_df)
 summary(res_delta)
 
  
 # Step 2: Create a boxplot for  resprout
 Rbp <- ggplot(resprouts_df,
               aes(x = Fenced, y = No_of_resprouts, fill = Fenced)) + facet_wrap(~Treatment)+ 
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Average no. of resprouts on cut stumps") +
   theme_beautiful() +
   theme(legend.position = "none")
 
## Save boxplot Resprouts
 #ggsave(Rbp,filename ="Plots/Resprouts count FENCED boxplot.png",
        #width = 16, height = 14, units = "cm")
 
 #### ### Violin plot for resprouts on cut stumps
 Rpv <- ggplot(resprouts_df,
               aes(x = Fenced, y = No_of_resprouts, fill = Fenced)) + facet_wrap(~Treatment)+ 
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Average no. of resprouts on cut stumps") +
   theme_beautiful() +
   theme(legend.position = "none")
 
### Save Violinplot Resprouts
 #ggsave(Rpv,filename ="Plots/Resprouts count FENCED Violinplot.png",
       # width = 16, height = 14, units = "cm")
 
###################################################################################################
##########################################################################################
 
 #SIMPSONS DIVEERSITY INDEX FOR WOODY PLANTS
 SapF <- read_csv("DATA/March2025/WoodyPC.csv")
 
 # Converting heights to saplings and seedlings and excluding cut stump. 
 SapF <- SapF %>% 
   mutate(
     woody_cat = case_when(
       Woody_class == "Cut stump"          ~ "Cut stump",
       between(`Max_height(m)`, 0.05, 0.50)          ~ "Seedlings",
       between(`Max_height(m)`, 0.51, 1.49)          ~ "Saplings",
       between(`Max_height(m)`, 1.5, 21.0)           ~ "Trees",
       TRUE                                 ~ NA_character_
     )
   )
 
####
 # Step 1: Filter seed only and years 2024 and 2025
 seedS <- SapF %>%
   filter(
     woody_cat == "Seedlings",
     Year %in% c(2024, 2025)
   )
 
#### Calculate species abundance per plot
 SeedSimp <- seedS %>%
   filter(!is.na(Species_name)) %>%  
   group_by(Site, Plot, Subplot, Fenced, Treatment, Year, Species_name) %>% 
   summarise(abundance = n())%>%
   summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')
 
 ##Mixed models
 #sempler <- lmer(simpson_index~ Treatment * Fenced +(1|Site), data =SeedSimp)
 #summary(sempler) # no diffference in Simpsons diversity across treatments. Though Fencing matters removing the fence lowers the seedling simpson index
 
 ##Visualisation; Boxplot
 See <- ggplot(SeedSimp, aes(x = Fenced, y = simpson_index, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Seedlings Simpsons diversity index") +
   theme_beautiful() +
   theme(legend.position = "none")
 
#saving BOXPLOT plot
 #ggsave(See,filename ="Plots/Seedlings Simpsons Boxplot.png",
  #width = 16, height = 14, units = "cm")

 #### Violin plot for Simpson's diversity
 SEV <- ggplot(SeedSimp, aes(x = Fenced, y = simpson_index, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Seedlings Simpsons diversity index") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 #saving VIOLINPLOT plot
 ggsave(SEV,filename ="Plots/Seedlings Simpsons Diversity Violinplot.png",
 width = 16, height = 14, units = "cm")

######################################################################################################
############################################## DELTA SEEDLINGS SIMPSONS DIVERSITY 
 
 ## Calculate species abundance per plot
 SeedSimp <- seedS %>%
   filter(!is.na(Species_name)) %>%  
   group_by(Site, Plot, Subplot, Fenced, Treatment, Year, Species_name) %>% 
   summarise(abundance = n())%>%
   summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')
 
 # . Pivot the two years side‑by‑side and compute Δ SIMPSONS DIVERSITY
 Seem_Delta <- SeedSimp %>% 
   pivot_wider(names_from  = Year,
               values_from = simpson_index,
               names_glue  = "sw_{Year}") %>% 
   mutate(delta_Seed = sw_2025 - sw_2024)   
 
 ### Removing non‑finite (NA, ±Inf) *and* (if on log scale) non‑positive ──
 seed_delta_clean <- Seem_Delta %>% 
   filter(
     is.finite(delta_Seed),   # drop NA / Inf / -Inf
     delta_Seed != 0          # <- only if you’re using a log scale; otherwise omit
   )
 
 
###Mixed models analysis
 #sem_delta <- lmer(delta_Seed~ Treatment * Fenced +(1|Site), data =seed_delta_clean)
 #summary(sem_delta) # No significant difference amongst treatments  
 
 ##Visualisation; Boxplot
 Sed <- ggplot(seed_delta_clean, aes(x = Fenced, y = delta_Seed, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Δ Seedlings Simpsons diversity index") +
   theme_beautiful() +
   theme(legend.position = "none")
 
###saving BOXPLOT plot
  #ggsave(Sed,filename ="Plots/Delta Seedlings Simpsons Boxplot.png",
  #width = 16, height = 14, units = "cm")
 
 #### Violin plot for Simpson's diversity
 SEVi <- ggplot(seed_delta_clean, aes(x = Fenced, y = delta_Seed, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Δ Seedlings Simpsons diversity index") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 #saving VIOLINPLOT plot
 ggsave(SEVi,filename ="Plots/ Delta Seedlings Simpsons Violinplot.png",
        width = 16, height = 14, units = "cm")
 
 
############################### SAPLINGS SIMPSONS DIVERSITY