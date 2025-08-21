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
library(DHARMa)   # model diagnostics GLMM
library(performance)  # model diagnostics
library(car)



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
         
#######         
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
#################################### SEEDLING HEIGHT AND  SEEDLING DENSITY SEEDLING DENSITY

# lLoad data
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
 #############################################   SEEDLINGS DENSITY SEEDLING DENSITY
 ######## ── 1. Density per subplot‑year ───────────────────────────────────────────
 SapF <- read_csv("DATA/March2025/WoodyPC.csv")
 
 ## create 
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
 
 
 seed_sub <- SapF %>% 
   filter(woody_cat == "Seedlings", Year %in% c(2024, 2025)) %>% 
   count(Site, Plot, Subplot, Treatment, Fencing, Encroachment_level, Year, name = "Seedlings") %>% 
   mutate(density_ha = Seedlings * 10000 / 600)     # convert to ha⁻¹
 
 # ── 2. Aggregate to Treatment × Fenced × Year (mean density) ──────────────
 seed_treat <- seed_sub %>% 
   group_by(Site, Plot, Subplot, Treatment, Fencing, Encroachment_level, Year) %>% 
   summarise(mean_dens_ha = mean(density_ha), .groups = "drop")  # ← use sum() if preferred
 
 
 seed_treat1 <- seed_sub %>%
   group_by(Site, Subplot) %>%
   filter(all(c(2024, 2025) %in% Year)) %>%
   ungroup() %>%
   group_by(Site, Plot, Subplot, Treatment, Fencing, Encroachment_level, Year) %>%
   summarise(meam_dens_ha = mean(density_ha), .groups = "drop")
 
 
 # ── 3. Pivot the two years side‑by‑side and compute Δ‑density ─────────────
 Seedling_Delta <- seed_treat1 %>% 
   pivot_wider(names_from  = Year,
               values_from = mean_dens_ha,
               names_glue  = "dens_{Year}") %>% 
   mutate(delta_Sdens = dens_2025 - dens_2024)        
 

 ### Removing non‑finite (NA, ±Inf) *and* (if on log scale) non‑positive ──
 sed_delta_clean <-  Seedling_Delta %>% 
   filter(
     is.finite(delta_Sdens),   # drop NA / Inf / -Inf
     delta_Sdens != 0          # <- only if you’re using a log scale; otherwise omit
   )
 
 
 ##Mixed models analysis for seedlings
   #sd_delta <- lmer(delta_Sdens~ Treatment * Fenced +(1|Site), data = Seedling_Delta)
   #summary(sd_delta)
 
seD <- glmmTMB(delta_Sdens ~ Treatment * Fenced  + (1 | Site/Plot/Subplot),
                   data = sed_delta_clean, family = gaussian(link = "identity")) 
summary(seD)


 
## GLMM FOR 3 WAY INTERACTIONS
seDd1 <- glmmTMB(delta_Sdens ~ Treatment * Encroachment_level + (1 | Site/Plot/Subplot),
               data = sed_delta_clean, family = gaussian(link = "identity")) 
summary(seDd1)


 # ── 4. Boxplot of change in density by Treatment & Fencing ────────────────
 Seb <- ggplot(sed_delta_clean,
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
 
 
 
 #### visualisation ENCROACHEMNT LEVEL
 
 Seb1 <- ggplot(Seedling_Delta,
               aes(x = Encroachment_level, y = delta_Sdens, fill = Encroachment_level)) + facet_wrap(~Treatment)+ 
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Encroachment_level", y = "Δ Seedlings density per ha") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 
 #saving BOXPLOT plot - ENCROACHMEMNT LEVEL
 ggsave(Seb1,filename ="Plots/Delta Seedlings density EL Boxplotplot.png",
        width = 16, height = 14, units = "cm")
 
## Violin plot - Seedling density ENCROACHMENT LEVEL
 ggplot(Seedling_Delta,
        aes(x = Encroachment_level, y = delta_Sdens, fill = Encroachment_level)) + facet_wrap(~Treatment)+  
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Encroachment_level", y = "Δ Seedling density per ha") +
   theme_beautiful() +
   theme(legend.position = "none")

##################################################################################################
 #############################################################
 ########################################### SAPLINGS SAPLINGS SAPLINGS HEIGHT
 
 #Load data
 SapF <- read_csv("DATA/March2025/WoodyPC.csv")
 
## create 
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
 
##################################################### 
###################################################NUMBER OF SAPLING RECRUITMENT
 
 # Summarize total weight by Site, Treatment, and Year
 sap_summary <- saplings_df %>%  
   filter(!is.na(`Max_height(m)`), Year %in% c(2024, 2025)) %>%
   group_by(Site, Plot, Subplot, Fencing, Treatment, Year) %>%
   summarise(total_height = round(sum(`Max_height(m)`)), .groups = "drop")
 
 #Mixed models analysis for number of saplings
 sapl <- lmer(total_height~ Treatment * Fenced +(1|Site), data = sap_summary)
 summary(sapl)
 
 # Plot the results
 ggplot(grass_summary,
        aes(x = Fenced, y = total_height, fill = Fenced)) + facet_wrap(~Treatment)+ 
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Total saplings") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 

 
 
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
 ######################################################## RESPROUTS RESPROUTS RESPROUTS
 SapF <- read_csv("DATA/March2025/WoodyPC.csv")
 
 ## create 
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
 
 
 # Step 1: Filter Cut stumps only and years 
 resprouts_df <- SapF %>%
   filter(
     woody_cat == "Cut stump",
     Year %in% c(2025),
     !Treatment %in% c("C", "F")  # exclude the two treatments
   )

 
 ##Mixed models analysis for resprouts
    #res_delta <- lmer(No_of_resprouts~ Treatment * Fenced +(1|Site), data = resprouts_df)
     # summary(res_delta)
 
 ## GLMM for resprouts
Resc <- glmmTMB(No_of_resprouts ~ Treatment * Fenced  + (1 | Site/Plot/Subplot),
                 data = resprouts_df, family = poisson (link = "log"))
summary(Resc)  

#Checking for oversdispersion
performance::check_overdispersion(Resc) # Overdispersion detected hence using alternative model NB

##Glmm using Negative binomial - FENCING
Res1 <- glmmTMB(No_of_resprouts ~ Treatment * Fenced  + (1 | Site/Plot/Subplot),
                data = resprouts_df, family = nbinom2())

summary(Res1) 


##GLMM ENCROACHMENT LEVELS
Res2 <- glmmTMB(No_of_resprouts ~ Treatment * Encroachment_level  + (1 | Site/Plot/Subplot),
                data = resprouts_df, family = nbinom2())

summary(Res2) 

### GLMM FIXED FACTORS INTERACTION
Res3 <- glmmTMB(No_of_resprouts ~ Treatment *  Fenced * Encroachment_level  + (1 | Site/Plot/Subplot),
                data = resprouts_df, family = nbinom2())
summary(Res3) 


 # Step 2: Create a boxplot for  resprout (FENCING) 
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
 
 ###### Visualisation No. of resprout (ENCROACHMENT LEVELS)
 Rbp1 <- ggplot(resprouts_df,
               aes(x = Encroachment_level, y = No_of_resprouts, fill = Encroachment_level)) + facet_wrap(~Treatment)+ 
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Encroachment_level", y = "Average no. of resprouts on cut stumps") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 ## Save boxplot Resprouts - ENCROACHMENT LEVEL
 ggsave(Rbp1,filename ="Plots/Resprouts count ENCROACHED boxplot.png",
 width = 16, height = 14, units = "cm")
 
 
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
 
 
 ## CUT STUMPS DETERMINED AT SPECIES LEVEL 
 Sppresprouts2 <- SapF %>%
   filter(
     woody_cat == "Cut stump",
     Year %in% c(2025),
     !Treatment %in% c("C", "F")  # exclude the two treatments
   ) %>%
   group_by(Site, Plot, Subplot, Treatment, Fenced, Encroachment_level, Species_name) %>%
   summarise(
     No_of_resprouts = sum(No_of_resprouts, na.rm = TRUE),
     mean_No_of_resprouts = mean(No_of_resprouts, na.rm = TRUE),
     n_Cut_stump = n(),
     .groups = "drop"
   )
 
## Correcting GLMM for some factors not accounted for in Encroachment level
 
 #Sppresprouts2$Encroachment_level <- as.factor(Sppresprouts2$Encroachment_level)
 
 #contrasts(Sppresprouts2$Encroachment_level) <- contr.sum(2)
 
 #str(Sppresprouts2$Encroachment_level)  # Should say "Factor w/ 2 levels"
 
 
## GLMM resprouts per species  
 ResSP <- glmmTMB(No_of_resprouts ~ Treatment * Encroachment_level * Fenced  + (1 | Site/Plot/Subplot),
                 data = Sppresprouts2, family = nbinom2())
 
summary(ResSP) 
 

emmeans(ResSP, ~ Encroachment_level)
 
 ## Visualisation of RESPROUTING AT SPECIES LEVEL 
 
 sRP<-Sppresprouts2 %>%
   ggplot(aes(x = reorder(Species_name, -No_of_resprouts), y = No_of_resprouts)) +
   geom_col(fill = "forestgreen") +
   labs(
     x = "Woody Species",
     y = "Number of resprouts"
   ) +
   theme_beautiful() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 ## Save boxplot Resprouts at SPECIES LEVEL
 ggsave(sRP,filename ="Plots/Resprouts at  SPECIES boxplot.png",
        width = 16, height = 14, units = "cm")
 
 
 
###################################################################################################
##########################################################################################
 
 #SIMPSONS DIVEERSITY INDEX FOR WOODY PLANTS
 SapF <- read_csv("DATA/March2025/WoodyPC.csv")
 
 # Converting heights to  seedlings, saplings and trees  and excluding cut stump. 
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
 
##### Violin plot for Simpson's diversity
 SEVi <- ggplot(seed_delta_clean, aes(x = Fenced, y = delta_Seed, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Δ Seedlings Simpsons diversity index") +
   theme_beautiful() +
   theme(legend.position = "none")
 
##saving VIOLINPLOT plot
 #ggsave(SEVi,filename ="Plots/ Delta Seedlings Simpsons Violinplot.png",
        width = 16, height = 14, units = "cm")
 
##################################################################################### 
############################### SAPLINGS SIMPSONS DIVERSITY
 
 #####
  SapF <- read_csv("DATA/March2025/WoodyPC.csv")
  
  # Converting heights to  seedlings, saplings and trees  and excluding cut stump. 
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
  
## new 
   sapli <- SapF %>%
   filter(
     woody_cat == "Saplings",
     Year %in% c(2024, 2025)
   )
 
 #### Calculate species abundance per plot
 SapSimp <- sapli %>%
   filter(!is.na(Species_name)) %>%  
   group_by(Site, Plot, Subplot, Fenced, Treatment, Year, Species_name) %>% 
   summarise(abundance = n())%>%
   summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')
 
###Mixed models
 #sappler <- lmer(simpson_index~ Treatment * Fenced +(1|Site), data =SapSimp)
 #summary(sappler) #TF shows significant difference with the control. It increases Simpsons diversity.
  #Unfenced plots do not differ with fenced plots 
 
 ##Visualisation; Boxplot
 Sap <- ggplot(SapSimp, aes(x = Fenced, y = simpson_index, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Saplings Simpsons diversity index") +
   theme_beautiful() +
   theme(legend.position = "none")
 
##saving BOXPLOT plot
 ggsave(Sap,filename ="Plots/Saplings Simpsons Boxplot.png",
 width = 16, height = 14, units = "cm")
 
 #### Violin plot for Simpson's diversity
 SPV <- ggplot(SapSimp, aes(x = Fenced, y = simpson_index, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Saplings Simpsons diversity index") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 #saving VIOLINPLOT plot
 ggsave(SPV,filename ="Plots/Saplings Simpsons Diversity Violinplot.png",
        width = 16, height = 14, units = "cm")
 
 ######################################################################################################
 ############################################## DELTA SAPLINGS SIMPSONS DIVERSITY 
 
 
 ## Calculate species abundance per plot
 SapSimp <- sapli %>%
   filter(!is.na(Species_name)) %>%  
   group_by(Site, Plot, Subplot, Fenced, Treatment, Year, Species_name) %>% 
   summarise(abundance = n())%>%
   summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')
 
 # . Pivot the two years side‑by‑side and compute Δ SIMPSONS DIVERSITY
 Sap_Delta <- SapSimp %>% 
   pivot_wider(names_from  = Year,
               values_from = simpson_index,
               names_glue  = "sw_{Year}") %>% 
   mutate(delta_Sap = sw_2025 - sw_2024)   
 
 ### Removing non‑finite (NA, ±Inf) *and* (if on log scale) non‑positive ──
 sap_delta_clean <- Sap_Delta %>% 
   filter(
     is.finite(delta_Sap),   # drop NA / Inf / -Inf
     delta_Sap != 0          # <- only if you’re using a log scale; otherwise omit
   )
 
 
####Mixed models analysis
 #sap_delta <- lmer(delta_Sap~ Treatment * Fenced +(1|Site), data =sap_delta_clean)
 #summary(sap_delta) # No significant difference amongst treatments  
 
 ##Visualisation; Boxplot
 Spd <- ggplot(sap_delta_clean, aes(x = Fenced, y = delta_Sap, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Δ Saplings Simpsons diversity index") +
   theme_beautiful() +
   theme(legend.position = "none")
 
###saving BOXPLOT plot
 #ggsave(Spd,filename ="Plots/Delta Saplings Simpsons Boxplot.png",
 #width = 16, height = 14, units = "cm")
 
 #### Violin plot for Simpson's diversity
 SPVi <- ggplot(sap_delta_clean, aes(x = Fenced, y = delta_Sap, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Δ Saplings Simpsons diversity index") +
   theme_beautiful() +
   theme(legend.position = "none")
 
##saving VIOLINPLOT plot
 #ggsave(SPVi,filename ="Plots/ Delta Saplings Simpsons Violinplot.png",
    #    width = 16, height = 14, units = "cm")
 

###############################################################################
  # TREE SIMPSON'S DIVERSITY  TREE SIMPSON'S DIVERSITY
 
 SapF <- read_csv("DATA/March2025/WoodyPC.csv")

#Data   
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
### new 
 treeS <- SapF %>%
   filter(
     woody_cat == "Trees",
     Year %in% c(2024, 2025)
   )
 
 #### Calculate species abundance per plot
 TrSimp <- treeS %>%
   filter(!is.na(Species_name)) %>%  
   group_by(Site, Plot, Subplot, Fenced, Treatment, Year, Species_name) %>% 
   summarise(abundance = n())%>%
   summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')
 
 
 ####Mixed models
   #trler <- lmer(simpson_index~ Treatment * Fenced +(1|Site), data =TrSimp)
 #summary(trler) #TF and TF8 shows significant differ with the control. they increases Simpsons diversity.
  # -Unfenced plots do not differ with fenced plots 
 
 ##Visualisation; Boxplot
 tr <- ggplot(TrSimp, aes(x = Fenced, y = simpson_index, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Trees Simpsons diversity index") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 ##saving BOXPLOT plot
 ggsave(tr,filename ="Plots/Trees Simpsons Boxplot.png",
        width = 16, height = 14, units = "cm")
 
 #### Violin plot for Simpson's diversity
 TRV <- ggplot(TrSimp, aes(x = Fenced, y = simpson_index, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Trees Simpsons diversity index") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 #saving VIOLINPLOT plot
  #ggsave(SPV,filename ="Plots/Trees Simpsons Diversity Violinplot.png",
   #     width = 16, height = 14, units = "cm")
 
 ######################################################################################################
 ############################################## DELTA TREES SIMPSONS DIVERSITY 
 
 ## Calculate species abundance per plot
 TrSimp <- treeS %>%
   filter(!is.na(Species_name)) %>%  
   group_by(Site, Plot, Subplot, Fenced, Treatment, Year, Species_name) %>% 
   summarise(abundance = n())%>%
   summarise(simpson_index = sum((abundance / sum(abundance))^2), .groups = 'drop')
 
 # . Pivot the two years side‑by‑side and compute Δ SIMPSONS DIVERSITY
 tr_Delta <- TrSimp %>% 
   pivot_wider(names_from  = Year,
               values_from = simpson_index,
               names_glue  = "sw_{Year}") %>% 
   mutate(delta_Tr = sw_2025 - sw_2024)   
 
 ### Removing non‑finite (NA, ±Inf) *and* (if on log scale) non‑positive ──
 tre_delta_clean <- tr_Delta %>% 
   filter(
     is.finite(delta_Tr),   # drop NA / Inf / -Inf
     delta_Tr != 0          # <- only if you’re using a log scale; otherwise omit
   )
 
 ####Mixed models analysis
 tre_delta <- lmer(delta_Tr~ Treatment * Fenced +(1|Site), data =tre_delta_clean)
 summary(tre_delta) # No significant difference amongst treatments  
 
 ##Visualisation; Boxplot
 trd <- ggplot(tre_delta_clean, aes(x = Fenced, y = delta_Tr, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Δ Trees Simpsons diversity index") +
   theme_beautiful() +
   theme(legend.position = "none")
 
####saving BOXPLOT plot
  #ggsave(trd,filename ="Plots/Delta Trees Simpsons Boxplot.png",
  #width = 16, height = 14, units = "cm")
 
 #### Violin plot for Simpson's diversity
 tvi <- ggplot(sap_delta_clean, aes(x = Fenced, y = delta_Sap, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Δ Trees Simpsons diversity index") +
   theme_beautiful() +
   theme(legend.position = "none")
 
##saving VIOLINPLOT plot
 #ggsave(tvi,filename ="Plots/ Delta Trees Simpsons Violinplot.png",
     #width = 16, height = 14, units = "cm")
 

###############################################################
 ########### SIMPSONS INDEX OF DIVERSITY (1 - D)
 
 SapF <- read_csv("DATA/March2025/WoodyPC.csv")
 
 #Data   
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
 ### new 
 treeS <- SapF %>%
   filter(
     woody_cat == "Trees",
     Year %in% c(2024, 2025)
   )
 
 #create vector
 TrSimpD <- treeS %>%
   filter(!is.na(Species_name)) %>%  
   group_by(Site, Plot, Subplot, Fenced, Treatment,Encroachment_level, Year, Species_name) %>% 
   summarise(abundance = n())%>%
   summarise(
     Simpson_index1 = 1 - sum((abundance / sum(abundance))^2),
     .groups = "drop"
   )
 
 
 #### USING Mixed models
 # GLMM - Use Beta regression as its designed for continuous outcomes between 0 and 1

 tsimpD3 <- glmmTMB(Simpson_index1 ~ Treatment * Year * Fenced  + (1 | Site/Plot/Subplot),
                    data = TrSimpD, family = beta_family(link = "logit"))

  summary(tsimpD3)
 
###Including encroachment level on interactions
  tsimpD4 <- glmmTMB(Simpson_index1 ~ Treatment * Year * Encroachment_level  + (1 | Site/Plot/Subplot),
                     data = TrSimpD, family = beta_family(link = "logit"))
  
  summary(tsimpD4)  
  
# Test and compare estimated marginal means of treatment over time
  #emm1 <- emmeans(tsimpD3, ~ Treatment | Fenced)
  #pairs(emm1)
  
# Or plot interactions
  plot(emm1)  
  

 ##Visualisation; Boxplot for 1-D (FENCING) 
 tsr <- ggplot(TrSimpD, aes(x = Fenced, y = Simpson_index1, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Trees Simpsons index of diversity (1-D)") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 # Visualisation for Encroachment level  
 betsd <- ggplot(TrSimpD, aes(x = Encroachment_level, y = Simpson_index1, fill =  Encroachment_level)) + facet_wrap(~Treatment) +
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Encroachment_level", y = "Trees Simpsons index of diversity (1-D)") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 ##saving BOXPLOT plot for Fenced 
 #ggsave(tsr,filename ="Plots/Trees Simpsons INDEX of DIVERSITY  Boxplot.png",
        #width = 16, height = 14, units = "cm")
 
 ggsave(betsd,filename ="Plots/Trees Simpsons INDEX of DIVERSITY BE  Boxplot.png",
        width = 16, height = 14, units = "cm")
 
 #### Violin plot for Simpson's diversity
 tsdv <- ggplot(TrSimpD, aes(x = Fenced, y = Simpson_index1, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Trees Simpsons index of diversity (1-D)") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 #saving VIOLINPLOT plot
 ggsave(tsdv,filename ="Plots/Trees Simpsons Iindex of Diversity Violinplot.png",
      width = 16, height = 14, units = "cm")
 
 ###############################################################################
 ###################################  Δ SIMPSONS INDEX OF DIVERSITY (1-D) TREES 
 
 ## Calculate species abundance per plot
 TrSimpD <- treeS %>%
   filter(!is.na(Species_name)) %>%  
   group_by(Site, Plot, Subplot, Fenced, Treatment,Encroachment_level, Year, Species_name) %>% 
   summarise(abundance = n())%>%
   summarise(
     Simpson_index1 = 1 - sum((abundance / sum(abundance))^2),
     .groups = "drop"
   )
 
 
 # . Pivot the two years side‑by‑side and compute Δ SIMPSONS DIVERSITY
 tsd_Delta <- TrSimpD %>% 
   pivot_wider(names_from  = Year,
               values_from = Simpson_index1,
               names_glue  = "sw_{Year}") %>% 
   mutate(Simpson_index1 = sw_2025 - sw_2024)   
 
 ### Removing non‑finite (NA, ±Inf) *and* (if on log scale) non‑positive ──
 tsd_delta_clean <- tsd_Delta %>% 
   filter(
     is.finite(Simpson_index1),   # drop NA / Inf / -Inf
     Simpson_index1 != 0          # <- only if you’re using a log scale; otherwise omit
   )
 
 ####Mixed models analysis
 tsimpDD <- glmmTMB(Simpson_index1 ~ Treatment * Fenced  + (1 | Site/Plot/Subplot),
                    data = tsd_delta_clean, family = gaussian(link = "identity"))
 
 summary(tsimpDD)
 
 ###Including encroachment level on interactions
 tsimpD1 <- glmmTMB(Simpson_index1 ~ Treatment * Encroachment_level  + (1 | Site/Plot/Subplot),
                    data = tsd_delta_clean, family = gaussian(link = "identity"))
 
 summary(tsimpD1)  
 

 ##Visualisation; Boxplot (1-D)
 trsd <- ggplot(tsd_delta_clean, aes(x = Fenced, y = Simpson_index1, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Δ Trees Simpsons index of diversity (1-D)") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 ####saving BOXPLOT plot
 ggsave(trsd,filename ="Plots/Delta Trees Simpsons index of diversity Boxplot.png",
 width = 16, height = 14, units = "cm")
 
 #### Violin plot for Simpson's diversity - FENCING
 tsvi <- ggplot(tsd_delta_clean, aes(x = Fenced, y = Simpson_index1, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fenced", y = "Δ Trees Simpsons index of diversity (1-D)") +
   theme_beautiful() +
   theme(legend.position = "none")
 
##saving VIOLINPLOT plot - FENCING
 ggsave(tsvi,filename ="Plots/ Delta Trees Simpsons ID Violinplot.png",
        width = 16, height = 14, units = "cm")
 
 
 ##### Violin plot for Simpson's diversity - ENCROACHMENT LEVEL
 tsvii <- ggplot(tsd_delta_clean, aes(x = Encroachment_level, y = Simpson_index1, fill = Fenced)) + facet_wrap(~Treatment) +
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Encroachment_level", y = "Δ Trees Simpsons index of diversity (1-D)") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 ##saving VIOLINPLOT plot - ENCROACHMENT LEVEL
 ggsave(tsvii,filename ="Plots/ Delta Trees Simpsons ID Violinplot BE.png",
        width = 16, height = 14, units = "cm")
 
#################################################
 
#### TREE HEIGHT REDONE

SapF <- read_csv("DATA/March2025/WoodyPC2.csv")
 
 # Converting heights to tress, saplings and seedlings and excluding cut stump. 
Ttdata  <- SapF %>% 
mutate(
     woody_cat = case_when(
       Woody_class == "Cut stump"          ~ "Cut stump",
       between(`Max_height(m)`, 0.05, 0.50)          ~ "Seedlings",
       between(`Max_height(m)`, 0.51, 1.49)          ~ "Saplings",
       between(`Max_height(m)`, 1.5, 21.0)           ~ "Trees",
       TRUE                                 ~ NA_character_
     )
   )
 
 
 #### Step 1: Filter trees only and years 2021 and 2022
 Ttdata  <- SapF %>%
   filter(
     woody_cat == "Trees",
     Year %in% c(2024, 2025)
   )         
 
 # Step 2: Create a boxplot for TREEs height 
 ggplot(Ttdata ,
        aes(x = Fencing, y =`Max_height(m)`, fill = Fencing)) + facet_wrap(~Treatment)+ 
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fencing", y = "Trees Max_height(m)") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 ####Mixed models analysis
 treeH <- glmmTMB(`Max_height(m)` ~ Treatment * Fencing  + (1 | Site/Plot/Subplot),
                    data = Ttdata, family = nbinom12 (link = "log")) 
 summary(treeH)
 
## GLMM FOR 3 WAY INTERACTIONS
treeHg <- glmmTMB(`Max_height(m)` ~ Treatment * Fencing * Encroachment_level   + (1 | Site/Plot/Subplot),
                  data = Ttdata, family = poisson (link = "log")) 
summary(treeHg) #data severly overdispersed than the nbinomial can handle. take next steps to check mean, variance

# checking if variance is > 1
var(Ttdata$`Max_height(m)`) / mean(Ttdata$`Max_height(m)`) # var = 0.4390

##test for zero inflation 
sum(Ttdata$`Max_height(m)` ) / length(Ttdata$`Max_height(m)`)  # Proportion of zeros

# Run diagnostics
summary(treeHg)  # Check for warnings  
diagnostics::check_convergence(treeHg)   
 
 ############# Compute Δ‑height (2024 − 2025) per Site/Plot/Subplot/Treatment/Fenced ----
 # Take the mean height within each grouping for each year before differencing.
 delta_Theight <- Ttdata  %>%
   group_by(Site, Plot, Subplot, Treatment, Fencing, Encroachment_level, Year) %>%
   summarise(mean_height = mean(`Max_height(m)`, na.rm = TRUE), .groups = "drop_last") %>%
   pivot_wider(
     names_from  = Year,
     values_from = mean_height,
     names_glue  = "height_{Year}"
   ) %>%
   mutate(delta_T = height_2025 - height_2024) %>%
   drop_na(delta_T)   # keep groups where both years are present
 
 # --- 4. Visualise: boxplot of Δ‑height by Treatment & Fencing -------------
 Tree <- ggplot(delta_Theight,
                aes(x = Fencing, y = delta_T, fill = Fencing)) + facet_wrap(~Treatment)+ 
   geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fencing", y = "Δ Trees max_height(m)") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 #saving BOXPLOT plot
 ggsave(Tree,filename ="Plots/Delta Trees Height FENCED boxplotplot.png",
        width = 16, height = 14, units = "cm")
 
 
 #### ### Violin plot for delta TREE height
 vT <- ggplot(delta_Theight,
               aes(x = Fencing, y = delta_T, fill = Fencing)) + facet_wrap(~Treatment)+  
   geom_violin(trim = FALSE)+
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Fencing", y = "Δ Tree max. height (m)") +
   theme_beautiful() +
   theme(legend.position = "none")
 
 #saving BOXPLOT plot
 ggsave(vT,filename ="Plots/Delta Tree Height FENCED Violinplot.png",
        width = 16, height = 14, units = "cm")               
 
 
 
###### Visualisation TREE HEIGHT - ENCROACHMENT LEVEL
 ggplot(delta_Theight,
 aes(x = Encroachment_level, y = delta_T, fill = Encroachment_level)) + facet_wrap(~Treatment)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Encroachment level", y = "Δ Trees max_height(m)") +
  theme_beautiful() +
  theme(legend.position = "none")

#saving BOXPLOT plot
ggsave(Tree,filename ="Plots/Delta Trees Height ENCROACHED boxplotplot.png",
       width = 16, height = 14, units = "cm")


#### ### Violin plot for delta TREE height
vT1 <- ggplot(delta_Theight,
             aes(x = Encroachment_level, y = delta_T, fill = Fenced)) + facet_wrap(~Treatment)+  
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Encroachment level", y = "Δ Tree max. height (m)") +
  theme_beautiful() +
  theme(legend.position = "none")

#saving VIOLIN PLOT - ENCROACHED
ggsave(vT1,filename ="Plots/Delta Tree Height ENCROACHED Violinplot.png",
       width = 16, height = 14, units = "cm")               


## GLMM analysis for tree Height
tg<- glmmTMB(delta_T ~ Treatment * Fenced + (1|Site/Plot), data = delta_Theight,
             family = gaussian(link = "identity"))
summary(tg)

# 3 way interaction 
trHeight <- glmmTMB( delta_T ~ Treatment * Fencing * Encroachment_level + (1|Site/Plot/Subplot), data = delta_Theight, 
              family = gaussian(link = "identity"))  
summary(trHeight)




tg1<- lmer(delta_T ~ Treatment * Fencing + (1|Site/Plot), data = delta_Theight)

summary(tg1)


## Make "Unfenced" the reference level (to see "fenced" coefficients)
# Check current class of FieldType

class(delta_Theight$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
delta_Theight$Fencing <- factor(delta_Theight$Fencing, ordered = FALSE)

# Verify
levels(delta_Theight$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level (to see "Unfenced" coefficients)
delta_Theight$Fencing <- relevel(delta_Theight$Fencing, ref = "Unfenced")

# Check if your model exists and is a valid GLMM object
class(tg1)  # Should print "lmerMod" or "glmmTMB"

# Correct way to extract random effects variances
summary(tg1)$varcor  # For lmer models

# check sample size per group
(table(delta_Theight$Treatment, delta_Theight$Fencing, delta_Theight$Encroachment_level))

#MIXED MODEL for Treatment and Fencing
tg1<- lmer(delta_T ~ Treatment * Fencing + (1|Site), data = delta_Theight)
summary(tg1)

#Mixed models T*F*E
tg2<- lmer(delta_T ~ Treatment * Fencing * Encroachment_level* + (1|Site), data = delta_Theight)
summary(tg2)

########################################
##################################### TREE DENSITY TREE DENSITY TREE DENSITY

SapF <- read_csv("DATA/March2025/WoodyPC2.csv")


## create 
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


Treesub <- SapF %>% 
  filter(woody_cat == "Trees", Year %in% c(2024, 2025)) %>% 
  count(Site, Plot, Subplot, Treatment, Fencing, Encroachment_level, Year, name = "Trees") %>% 
  mutate(density_ha = Trees * 10000 / 600)     # convert to ha⁻¹

# ── 2. Aggregate to Treatment × Fenced × Year (mean density) ──────────────
trees_treat <- Treesub %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Encroachment_level, Year) %>% 
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop")  # ← use sum() if preferred



# ── 3. Pivot the two years side‑by‑side and compute Δ‑density ─────────────
Tree_DeltaD <- trees_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Tdens = dens_2025 - dens_2024)        



### Make "Unfenced" the reference level (to see "fenced" coefficients)
# Check current class of FieldType

class(Tree_DeltaD$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
Tree_DeltaD$Fencing <- factor(Tree_DeltaD$Fencing, ordered = FALSE)

# Verify
levels(Tree_DeltaD$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level (to see "Unfenced" coefficients)
Tree_DeltaD$Fencing <- relevel(Tree_DeltaD$Fencing, ref = "Unfenced")

# Check if your model exists and is a valid GLMM object
class(tre)  # Should print "lmerMod" or "glmmTMB"

# Correct way to extract random effects variances
summary(tre)$varcor  # For lmer models


##Mixed models analysis for seedlings

tre <- glmmTMB(delta_Tdens ~ Treatment * Fencing  + (1 | Site/Plot/Subplot),
               data = Tree_DeltaD, family = gaussian(link = "identity")) 
summary(tre)



## GLMM FOR 3 WAY INTERACTIONS
tre2 <- glmmTMB(delta_Tdens ~ Treatment * Fencing * Encroachment_level  + (1 | Site/Plot/Subplot),
               data = Tree_DeltaD, family = gaussian(link = "identity")) 
summary(tre2)


treL <- lmer(delta_Tdens ~ Treatment * Fencing * Encroachment_level + (1|Site), data = Tree_DeltaD)
summary(treL)


tre3 <- glmmTMB(delta_Tdens ~ Treatment * Fencing * Encroachment_level  + (1 | Site),
                family = gaussian(link = "identity"), data = Tree_DeltaD, dispformula = ~1) 
summary(tre3)


# Using + (1 |Site) +  (1 |Plot)
  #tre3 <- glmmTMB(delta_Tdens ~ Treatment * Fencing * Encroachment_level  + (1 | Site)+ (1 |Plot) + (1 |Subplot),
              #  data = Tree_DeltaD, family = gaussian(link = "identity")) 
  #summary(tre3)


#check for NAs
any(is.na(resid(tre2)))  # Check for NA residuals
any(is.infinite(resid(tre2)))  # Check for Inf/-Inf

##2. Verify Model Convergence.  For non-lm models (e.g., glm, lmer), check convergence:

 #tre2$converged  # Should be TRUE



##Check for residuals
qqnorm(resid(tre2))
qqline(resid(tre2))


##Check for outliers
residuals_std <- scale(resid(tre2))  # Standardize residuals
outliers <- which(abs(residuals_std) > 3)  # Find outliers
print(outliers)


## Shapiro-Wilk Test for Normality

shapiro.test(resid(tre2))  #p < 0.05 → residuals are non-normal.
  # result p = 0.000167, hence residuals are normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(tre2), breaks = 5)
leveneTest(resid(tre2) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)

##Plot the conditional effects (e.g., using emmeans or ggeffects):
emm <- emmeans(tre2, ~ Treatment | Fencing * Encroachment_level)
plot(emm)


## Use likelihood ratio tests (LRTs) to compare nested models
anova(tre2, complex_model, test = "LRT")  


###filter control
TrefILTERTHF <- SapF %>% 
  filter(woody_cat == "Trees", Year %in% c(2024, 2025), Treatment %in% "F") %>% 
  count(Site, Plot, Subplot, Treatment, Fencing, Encroachment_level, Year, name = "Trees") %>% 
  mutate(density_ha = Trees * 10000 / 600)     # convert to ha⁻¹



## refitting and comparing 2-way and 3-way interactions 

# Fit the 2-way interaction model (simpler model)
tre2a <- glmmTMB(delta_Tdens ~ Treatment * Fencing + Treatment * Encroachment_level + Fencing * Encroachment_level  + (1 | Site/Plot/Subplot),
                data = Tree_DeltaD, family = gaussian(link = "identity"))
summary(tre2a)

# Fit the 3-way interaction model (complex model)
tre3a <- glmmTMB(delta_Tdens ~ Treatment  * Encroachment_level * Fencing + (1 | Site),
                 data = Tree_DeltaD, family = gaussian(link = "identity"))
summary(tre3a) 


#Model failed to converge, try this one 
tre3a <- glmmTMB(delta_Tdens ~ Treatment  * Encroachment_level * Fencing + (1 | Site),
                control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")),
                data = Tree_DeltaD)

#try another model for converging
tre3_opt <- glmmTMB(
  delta_Tdens ~ Treatment  * Encroachment_level * Fencing +
      (1 | Site),
    control = glmmTMBControl(
      optimizer = optim,
      optArgs = list(method = "BFGS")
    ),
    data = Tree_DeltaD
  )

summary(tre3_opt) # this model too has failed to converge, results producing NaN


#Check for excess zeros 
table(Tree_DeltaD$delta_Tdens == 0)  # Check for excess zeros

# visualising 3way 
ggplot(Tree_DeltaD, aes(x = Treatment, y = delta_Tdens, color = Fencing)) +
  geom_boxplot() +
  facet_wrap(~ Encroachment_level) +
  theme_beautiful()

###2. Now Compare the two models tre2a and tre3a with Likelihood Ratio Test (LRT)

anova(tre2a, tre3a, test = "LRT")  # Or test = "Chisq"


summary(tre3a)

