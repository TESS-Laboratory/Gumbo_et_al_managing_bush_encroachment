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
library(openxlsx)
library(broom.mixed) #convert model objects to data frames
library(robustlmm)

#create theme beautiful
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



#TREE DENSITY TREE DENSITY TREE DENSITY

 SapF <- read_csv("DATA/March2025/WoodyPC2.csv")


## create 
SapF <- SapF %>% 
  mutate(
    woody_cat = case_when(
      Woody_class == "Cut stump"          ~ "Cut stump",
      between(`Max_height(m)`, 0.05, 0.50)          ~ "Seedlings",
      between(`Max_height(m)`, 0.51, 1.49)          ~ "Saplings",
      between(`Max_height(m)`, 1.5, 21.0)           ~ "Trees",
      TRUE                             ~ NA_character_
    )
  )


#Filter 
Treesub <- SapF %>% 
  filter(woody_cat == "Trees", Year %in% c(2024, 2025)) %>% 
  count(Site, Plot, Subplot, Treatment, Fencing, Encroachment_level, Year, name = "Trees") %>% 
  mutate(density_ha = Trees * 10000 / 600)     # convert to ha⁻¹

#comparing at treatment level
#TreesWPC <- SapF %>% 
 # filter(woody_cat == "Trees", Year %in% c(2024, 2025), Treatment %in% c("F")) %>% 
  #count(Site, Plot, Subplot, Treatment, Fencing, Encroachment_level, Year,  name = "Trees") %>% 
  #mutate(density_ha = Trees * 10000 / 600)      # convert to ha⁻¹




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
levels(Tree_DeltaD$Fencing)  # Should show "Fenced" "Unfenced" (or vice versa)

# Set "Fenced" as the reference level (to see "Unfenced" coefficients)
Tree_DeltaD$Fencing <- relevel(Tree_DeltaD$Fencing, ref = "Unfenced")


# Correct way to extract random effects variances
summary(tre)$varcor  # For lmer models


## Change Encroachment level referencing to Moderate so that High can be seen
# Check current class of FieldType

class(Tree_DeltaD$Encroachment_level)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
Tree_DeltaD$Encroachment_level <- factor(Tree_DeltaD$Encroachment_level, ordered = FALSE)

# Verify
levels(Tree_DeltaD$Encroachment_level)  # Should show "Open" "Closed" (or vice versa)

# Set "Moderate" as the reference level 
Tree_DeltaD$Encroachment_level <- relevel(Tree_DeltaD$Encroachment_level, ref = "Moderate")


# Convert to unordered factor explicitly
Tree_DeltaD$Fencing <- factor(Tree_DeltaD$Fencing, ordered = FALSE)

# Set "Unfenced" as the reference level 
Tree_DeltaD$Fencing <- relevel(Tree_DeltaD$Fencing, ref = "Unfenced")



## GLMM FOR FIXED EFFECT INTERACTIONS
# Fit the 2-way interaction model (simpler model)
tre2a <- glmmTMB(delta_Tdens ~ Treatment * Fencing + Treatment * Encroachment_level + Fencing * Encroachment_level  + (1 | Site),
                 data = Tree_DeltaD, family = gaussian(link = "identity"))
summary(tre2a)


tre3 <- glmmTMB(delta_Tdens ~ Treatment * Fencing + (1 | Site),
                 data = Tree_DeltaD, family = gaussian(link = "identity"))
summary(tre3)

#check for NAs
any(is.na(resid(tre2a)))  # Check for NA residuals
any(is.infinite(resid(tre2a)))  # Check for Inf/-Inf

##2. Verify Model Convergence.  For non-lm models (e.g., glm, lmer), check convergence:

#tre2a$converged  # Should be TRUE

##Check for residuals
qqnorm(resid(tre2a))
qqline(resid(tre2a))

# For comprehensive diagnostics:
performance::check_convergence(tre3) # true with all RE. without nested RE = true

##Check for outliers
residuals_std <- scale(resid(tre2a))  # Standardize residuals
outliers <- which(abs(residuals_std) > 3)  # Find outliers
print(outliers)


## Shapiro-Wilk Test for Normality

shapiro.test(resid(tre2a))  #p < 0.05 → residuals are non-normal.
# result p = 0.00005, hence residuals are non normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(tre2a), breaks = 5)
leveneTest(resid(tre2a) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
   #result p = 0.1741, homoscedastsicity


##Plot the conditional effects (e.g., using emmeans or ggeffects):
emm <- emmeans(tre2a, ~ Treatment | Fencing * Encroachment_level)
plot(emm)


##Converting results to excel.
# Export as excel 
results <- tidy(tre2a, conf.int = TRUE)
write.xlsx (results,"Data/Delta Trees Density GLMM_results.xlsx")

### visualising the interactions - boxplot

Tdbx <- ggplot(Tree_DeltaD,
       aes(x = Treatment, y = delta_Tdens, color = Fencing)) + facet_wrap(~Encroachment_level)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Trees density per ha") +
  theme_beautiful() #

##saving BOXPLOT  -  including encroachment level
ggsave(Tdbx,filename ="Plots/2Delta Tree Density INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")  


# boxplot - excluding encroachment level

Tdbx3 <- ggplot(Tree_DeltaD,
               aes(x = Treatment, y = delta_Tdens)) + facet_wrap(~Fencing)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Trees density per ha") +
  theme_beautiful() #

##saving BOXPLOT  -  excluding encroachment level
ggsave(Tdbx3,filename ="Plots/3Delta Tree Density INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")  

##Violin plot for Tree density Interactions

Tdvi <- ggplot(Tree_DeltaD,
       aes(x = Treatment, y = delta_Tdens, color = Fencing)) + 
  facet_wrap(~Encroachment_level)+   
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Tree density per ha") +
  theme_beautiful() 

#saving VIOLIN PLOT - INTERACTIONS
ggsave(Tdvi,filename ="Plots/2Delta Tree density INTERACTIONS Violinplot.png",
       width = 16, height = 14, units = "cm")         



######################################### TREE HEIGHT TREEE HEIGHT
SapF <- read_csv("DATA/March2025/WoodyPC.csv")


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


#### Step 1: Filter trees only and years 
Ttdata  <- SapF %>%
  filter(
    woody_cat == "Trees",
    Year %in% c(2024, 2025)
  )         

## Compute Δ‑height (2024 − 2025) per Site/Plot/Subplot/Treatment/Fenced ----
# Take the mean height within each grouping for each year before differencing.
delta_THeight <- Ttdata  %>%
  group_by(Site, Plot, Subplot, Treatment, Fencing, Encroachment_level, Year) %>%
  summarise(mean_height = mean(`Max_height(m)`, na.rm = TRUE), .groups = "drop_last") %>%
  pivot_wider(
    names_from  = Year,
    values_from = mean_height,
    names_glue  = "height_{Year}"
  ) %>%
  mutate(delta_T = height_2025 - height_2024) %>%
  drop_na(delta_T)   # keep groups where both years are present


## Change Encroachment level referencing to Moderate so that High can be seen
# Check current class of FieldType

class(delta_THeight$Encroachment_level)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
delta_THeight$Encroachment_level <- factor(delta_THeight$Encroachment_level, ordered = FALSE)

# Verify
levels(delta_THeight$Encroachment_level)  # Should show "Open" "Closed" (or vice versa)

# Set "Moderate" as the reference level (to see "High" coefficients)
delta_THeight$Encroachment_level <- relevel(delta_THeight$Encroachment_level, ref = "Moderate")


# Convert to unordered factor explicitly
delta_THeight$Fencing <- factor(delta_THeight$Fencing, ordered = FALSE)

# Set "Unfenced" as the reference level (to see "Fenced" coefficients)
delta_THeight$Fencing <- relevel(delta_THeight$Fencing, ref = "Unfenced")


# ## GLMM analysis for tree Height
tg1<- glmmTMB(delta_T ~ Treatment * Fencing + Treatment * Encroachment_level + Fencing * Encroachment_level  
             + (1 | Site), 
             data = delta_THeight,
             family = gaussian(link = "identity"))
summary(tg1)

# For comprehensive diagnostics:
performance::check_convergence(tg1) # FALSE with all RE. without nested RE = true

##Check for residuals
qqnorm(resid(tg1))
qqline(resid(tg1))


##Check for outliers
residuals_std <- scale(resid(tg1))  # Standardize residuals
outliers <- which(abs(residuals_std) > 3)  # Find outliers
print(outliers)


## Shapiro-Wilk Test for Normality

shapiro.test(resid(tg1))  #p < 0.05 → residuals are non-normal.
# result p = 0.04914, hence residuals are non normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(tg1), breaks = 5)
leveneTest(resid(tg1) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
#result p = 0.1264, homoscedastsicity

#using augment to check for observations with highest leverage and influence
augment(tre3)%>%
 

##Plot the conditional effects (e.g., using emmeans or ggeffects):
emm <- emmeans(tg1, ~ Treatment | Fencing * Encroachment_level)
plot(emm)


##Converting results to excel.
# Export as excel 
results <- tidy(tg1, conf.int = TRUE)
write.xlsx (results,"Data/Delta Trees Height GLMM_results.xlsx")


## --- 4. Visualise: boxplot of Δ‑height by interaction
Thebx <- ggplot(delta_THeight,
       aes(x = Treatment, y = delta_T, color = Fencing)) + facet_wrap(~Encroachment_level)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Tree height (m)") +
  theme_beautiful() #

##saving BOXPLOT - INTERACTIONS
ggsave(Thebx,filename ="Plots/2Delta Tree height INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")  

##Violin plot for Tree density Interactions
Thvi <- ggplot(delta_THeight,
               aes(x = Treatment, y = delta_T, color = Fencing)) + facet_wrap(~Encroachment_level)+   
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Tree height (m)") +
  theme_beautiful() 

#saving VIOLIN PLOT - INTERACTIONS
ggsave(Thvi,filename ="Plots/2Delta Tree height INTERACTIONS Violinplot.png",
       width = 16, height = 14, units = "cm")         

######################################################
#################################### Δ SIMPSONS INDEX OF DIVERSITY (1-D) TREES 
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


## Calculate species abundance per plot
TrSimpDi <- SapF %>%
  filter(!is.na(Species_name)) %>%  
  group_by(Site, Plot, Subplot, Fencing, Treatment,Encroachment_level, Year, Species_name) %>% 
  summarise(abundance = n())%>%
  summarise(
    Simpson_index1 = 1 - sum((abundance / sum(abundance))^2),
    .groups = "drop"
  )


# . Pivot the two years side‑by‑side and compute Δ SIMPSONS DIVERSITY
tsdi_Delta <- TrSimpDi %>% 
  pivot_wider(names_from  = Year,
              values_from = Simpson_index1,
              names_glue  = "sw_{Year}") %>% 
  mutate(Simpson_index1 = sw_2025 - sw_2024)   


### Change Encroachment level referencing to Moderate so that High can be seen
# Check current class of FieldType

class(tsdi_Delta$Encroachment_level)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
tsdi_Delta$Encroachment_level <- factor(tsdi_Delta$Encroachment_level, ordered = FALSE)

# Verify
levels(tsdi_Delta$Encroachment_level)  # Should show "Open" "Closed" (or vice versa)

# Set "Moderate" as the reference level (to see "High" coefficients)
tsdi_Delta$Encroachment_level <- relevel(tsdi_Delta$Encroachment_level, ref = "Moderate")


# For FENCING Convert to unordered factor explicitly
tsdi_Delta$Fencing <- factor(tsdi_Delta$Fencing, ordered = FALSE)

# Set "Unfenced" as the reference level 
tsdi_Delta$Fencing <- relevel(tsdi_Delta$Fencing, ref = "Unfenced")


# ## GLMM analysis for tree SIMPSONS DIVERSITY
tsdi1<- glmmTMB(Simpson_index1 ~ Treatment * Fencing + Treatment * Encroachment_level + Fencing * Encroachment_level  
              + (1 | Site), 
              data = tsdi_Delta,
              family = gaussian(link = "identity"))

summary(tsdi1)

## with subplot and plot
tsdi2<- glmmTMB(Simpson_index1 ~ Treatment * Fencing + Treatment * Encroachment_level + Fencing * Encroachment_level  
                + (1 | Site/Plot/Subplot), 
                data = tsdi_Delta,
                family = gaussian(link = "identity"))

summary(tsdi2)

# Checking fo residuals
qqnorm(resid(tsdi1))
qqline(resid(tsdi1))

# For comprehensive diagnostics:
performance::check_convergence(tsdi2) # true 

## Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(tsdi2))  #p < 0.05 → residuals are non-normal.
# result p = 0.051, hence residuals are normal distributed

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(tsdi2), breaks = 5)
leveneTest(resid(tsdi2) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.3082 equal variance 


##Converting results to excel.
# Export as excel 
results <- tidy(tsdi2, conf.int = TRUE)
write.xlsx (results,"Data/3TREE Simpsons diversity GLMM_results.xlsx")



##Converting results to excel.
# Export as excel 
  #results <- tidy(tsdi1, conf.int = TRUE)
  #write.xlsx (results,"Data/Delta Tree Simp Diversity GLMM_results.xlsx")

## --- 4. Visualise: boxplot of Δ‑SIMPSON'S INDEX OF DIVERSITY by interaction
Tsimbx <- ggplot(tsdi_Delta,
                aes(x = Treatment, y = Simpson_index1, color = Fencing)) + facet_wrap(~Encroachment_level)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Tree Simpson's index of diversity ") +
  theme_beautiful() #

##saving BOXPLOT - INTERACTIONS
ggsave(Tsimbx,filename ="Plots/2Delta Tree Simpsons BOXplot.png",
       width = 16, height = 14, units = "cm")  

##Violin plot for Tree density Interactions
Thvi <- ggplot(delta_THeight,
               aes(x = Treatment, y = delta_T, color = Fencing)) + facet_wrap(~Encroachment_level)+   
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Tree height (m)") +
  theme_beautiful() 

#saving VIOLIN PLOT - INTERACTIONS
ggsave(Thvi,filename ="Plots/2Delta Tree height INTERACTIONS Violinplot.png",
       width = 16, height = 14, units = "cm")     

############################################################################
################################ SEEDLINGS SEEDLINGS SEEDLINGS DENSITY

SapF <- read_csv("DATA/March2025/WoodyPC2.csv")


## create 
SapF <- SapF %>% 
  mutate(
    woody_cat = case_when(
      Woody_class == "Cut stump"          ~ "Cut stump",
      between(`Max_height(m)`, 0.05, 0.50)          ~ "Seedlings",
      between(`Max_height(m)`, 0.51, 1.49)          ~ "Saplings",
      between(`Max_height(m)`, 1.5, 21.0)           ~ "Trees",
      TRUE                             ~ NA_character_
    )
  )

#Filter 
Seedlingsub <- SapF %>% 
  filter( Year %in% c(2024, 2025)) %>% 
  count(Site, Plot, Subplot, Treatment, Fencing, Encroachment_level, Year, name = "Seedlings") %>% 
  mutate(density_ha = Seedlings * 10000 / 600)     # convert to ha⁻¹

# ── 2. Aggregate to Treatment × Fenced × Year (mean density) ──────────────
seedlings_treat <- Seedlingsub %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Encroachment_level, Year) %>% 
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop")  # ← use sum() if preferred



# ── 3. Pivot the two years side‑by‑side and compute Δ seedling density ─────────────
Seed_DeltaD <- seedlings_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Sdens = dens_2025 - dens_2024)        



### Make "Unfenced" the reference level 

class(Seed_DeltaD$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
Seed_DeltaD$Fencing <- factor(Seed_DeltaD$Fencing, ordered = FALSE)

# Verify
levels(Seed_DeltaD$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level 
Seed_DeltaD$Fencing <- relevel(Seed_DeltaD$Fencing, ref = "Unfenced")


# Correct way to extract random effects variances
#summary(tre)$varcor  # For lmer models


## Change Encroachment level referencing to Moderate so that High can be seen
# Check current class of FieldType

class(Seed_DeltaD$Encroachment_level)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
Seed_DeltaD$Encroachment_level <- factor(Seed_DeltaD$Encroachment_level, ordered = FALSE)

# Verify
levels(Seed_DeltaD$Encroachment_level)  # Should show "Open" "Closed" (or vice versa)

# Set "Moderate" as the reference level (to see "High" coefficients)
Seed_DeltaD$Encroachment_level <- relevel(Seed_DeltaD$Encroachment_level, ref = "Moderate")

## GLMM FOR FIXED EFFECT INTERACTIONS
# Fit the 2-way interaction model (simpler model)
  Seed <- glmmTMB(delta_Sdens ~ Treatment * Fencing + Treatment * Encroachment_level +
                  Fencing * Encroachment_level  + (1 | Site),
                 data = Seed_DeltaD, family = gaussian(link = "identity"))
#summary(Seed)

## with plot and subplot
Seed2 <- glmmTMB(delta_Sdens ~ Treatment * Fencing + Treatment * Encroachment_level +
                  Fencing * Encroachment_level  + (1 | Site/Subplot),
                data = Seed_DeltaD, family = gaussian(link = "identity"))
summary(Seed2)

# check for model residuals and distribution
qqnorm(resid(Seed))
qqline(resid(Seed))

# For comprehensive diagnostics:
performance::check_convergence(Seed2) # TRUE without nested RE, with nested RE = False


##Check for outliers
residuals_std <- scale(resid(Seed2))  # Standardize residuals
outliers <- which(abs(residuals_std) > 3)  # Find outliers
print(outliers)


## Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(Seed2))  #p < 0.05 → residuals are non-normal.
# result p = 0.3021, hence residuals are normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(Seed2), breaks = 5)
leveneTest(resid(Seed2) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.2598 equal variance 


##Converting results to excel.
# Export as excel 
results <- tidy(Seed2, conf.int = TRUE)
write.xlsx (results,"Data/3Delta Seedling Density GLMM_results.xlsx")

## --- 4. Visualise: boxplot of Δ‑ SEEDLING DENSITY by interaction
Seedbx1 <- ggplot(Seed_DeltaD,
                 aes(x = Treatment, y = delta_Sdens , color = Fencing)) + facet_wrap(~Encroachment_level)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Seedling density per ha") +
  theme_beautiful() #

##saving BOXPLOT - INTERACTIONS
ggsave(Seedbx1,filename ="Plots/2Delta Seedling Density INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")  

##Violin plot for  density Interactions
shvi <- ggplot(Seed_DeltaD,
               aes(x = Treatment, y = delta_Sdens , color = Fencing)) + facet_wrap(~Encroachment_level)+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Seedling density per ha") +
  theme_beautiful() 

#saving VIOLIN PLOT - INTERACTIONS
ggsave(shvi,filename ="Plots/2Delta Seedling density INTERACTIONS Violinplot.png",
       width = 16, height = 14, units = "cm")     

#################################################################################
######################### SAPLING DENSITY DENSITY 

#SapF <- read_csv("DATA/March2025/WoodyPC.csv")
SapF <- read_csv("DATA/March2025/Saplings25.csv")

## create 
SapF <- SapF %>% 
  mutate(
    woody_cat = case_when(
      Woody_class == "Cut stump"          ~ "Cut stump",
      between(`Max_height(m)`, 0.05, 0.50)          ~ "Seedlings",
      between(`Max_height(m)`, 0.51, 1.49)          ~ "Saplings",
      between(`Max_height(m)`, 1.5, 21.0)           ~ "Trees",
      TRUE                             ~ NA_character_
    )
  )


#Filter 
Saplingsub <- SapF %>% 
  filter( Year %in% c(2024, 2025)) %>% 
  count(Site, Plot, Subplot, Treatment, Fencing, Encroachment_level, Year, name = "Saplings") %>% 
  mutate(density_ha = Saplings * 10000 / 600)     # convert to ha⁻¹

# ── 2. Aggregate to Treatment × Fenced × Year (mean density) ──────────────
saplings_treat <- Saplingsub %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Encroachment_level, Year) %>% 
  summarise(mean_dens_ha = mean(density_ha), .groups = "drop")  # ← use sum() if preferred

# ── 3. Pivot the two years side‑by‑side and compute Δ‑density ─────────────
Sap_DeltaD <- saplings_treat %>% 
  pivot_wider(names_from  = Year,
              values_from = mean_dens_ha,
              names_glue  = "dens_{Year}") %>% 
  mutate(delta_Spdens = dens_2025 - dens_2024)        


### Make "Unfenced" the reference level (to see "fenced" coefficients)
# Check current class of FieldType

class(Sap_DeltaD$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
Sap_DeltaD$Fencing <- factor(Sap_DeltaD$Fencing, ordered = FALSE)

# Verify
levels(Sap_DeltaD$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level (to see "Unfenced" coefficients)
Sap_DeltaD$Fencing <- relevel(Sap_DeltaD$Fencing, ref = "Unfenced")

## Change Encroachment level referencing to Moderate so that High can be seen
# Check current class of FieldType

class(Sap_DeltaD$Encroachment_level)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
Sap_DeltaD$Encroachment_level <- factor(Sap_DeltaD$Encroachment_level, ordered = FALSE)

# Verify
levels(Sap_DeltaD$Encroachment_level)  # Should show "Open" "Closed" (or vice versa)

# Set "moderate" as the reference level 
Sap_DeltaD$Encroachment_level <- relevel(Sap_DeltaD$Encroachment_level, ref = "Moderate")


## GLMM FOR FIXED EFFECT INTERACTIONS
# Fit the 2-way interaction model (simpler model)

#Sap <- glmmTMB(delta_Spdens ~ Treatment * Fencing + Treatment * Encroachment_level +
                  #Fencing * Encroachment_level  + (1 | Site/Plot/Subplot),
                #data = Sap_DeltaD, family = gaussian(link = "identity"))
   #summary(Sap)


Sap1 <- glmmTMB(delta_Spdens ~ Treatment * Fencing + Treatment * Encroachment_level +
                  Fencing * Encroachment_level  + (1 | Site),
                data = Sap_DeltaD, family = gaussian(link = "identity"))
 summary(Sap1)

##
Sap2 <- lmer(delta_Spdens ~ Treatment * Fencing +
                   (1 | Site/Plot),
                data = Sap_DeltaD) #, family = gaussian(link = "identity"))
 summary(Sap2)


# For comprehensive diagnostics:
performance::check_convergence(Sap2) # TRUE without nested RE, with nested RE = false 

#Test for Collinearity
# For fixed effects only (use lm() as a shortcut)
car::vif(lm(delta_Spdens ~ Treatment + Fencing, data = Sap_DeltaD)) #VIF > 5 indicates problematic collinearity.


#Inspect Random Effects
summary(Sap2)$varcor

#check for outliers
boxplot(Sap_DeltaD$delta_Spdens, main = "Outlier Check")

# check for model residuals and distribution
qqnorm(resid(Sap2))
qqline(resid(Sap2))

#check for NAs
any(is.na(resid(Sap)))  # Check for NA residuals
any(is.infinite(resid(Sap)))  # Check for Inf/-Inf

## Verify Model Convergence.  For non-lm models (e.g., glm, lmer), check convergence:

Sap1$converged  # Should be TRUE


##Check for outliers
residuals_std <- scale(resid(Sap2))  # Standardize residuals
outliers <- which(abs(residuals_std) > 3)  # Find outliers
print(outliers)

## Shapiro-Wilk Test for Normality

shapiro.test(resid(Sap2))  #p < 0.05 → residuals are non-normal.
# result p = 0.0143, hence residuals are non-normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(Sap2), breaks = 5)
leveneTest(resid(Sap2) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
  # result p = 0.959, hence equal variance 

##Plot the conditional effects (e.g., using emmeans or ggeffects):
emm <- emmeans(Sap, ~ Treatment | Fencing * Encroachment_level)
plot(emm)


##Converting results to excel.
# Export as excel 
results <- tidy(Sap2, conf.int = TRUE)
write.xlsx (results,"Data/Delta Sapling Density GLMM_results.xlsx")

## --- 4. Visualise: boxplot of Δ‑ SAPLING DENSITY by interaction
Sapbx <- ggplot(Sap_DeltaD,
                 aes(x = Treatment, y = delta_Spdens , color = Fencing)) + facet_wrap(~Encroachment_level)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Sapling density per ha") +
  theme_beautiful() #

##saving BOXPLOT - INTERACTIONS
ggsave(Sapbx,filename ="Plots/2Delta Sapling Density INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")  

##Violin plot for SAPLING density Interactions
Sapvi <- ggplot(Sap_DeltaD,
               aes(x = Treatment, y = delta_Spdens, color = Fencing)) + facet_wrap(~Encroachment_level)+   
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Sapling density") +
  theme_beautiful() 

#saving VIOLIN PLOT - INTERACTIONS
ggsave(Sapvi,filename ="Plots/2Delta Sapling density INTERACTIONS Violinplot.png",
       width = 16, height = 14, units = "cm")     



###################### SAPLINGS SAPLINGS SAPLINGS HEIGHT

# Take the mean height within each grouping for each year 

## Step : Filter saplings only and years 
saplings_df <- SapF %>%
  filter(
    woody_cat == "Saplings",
    Year %in% c(2024, 2025)
  )

############## Compute saplings Δ‑height (2024 − 2025) per Site/Plot/Subplot/Treatment/Fenced ----
delta_heightSp <- saplings_df %>%
  group_by(Site, Plot, Subplot, Treatment, Fencing,Encroachment_level, Year) %>%
  summarise(mean_height = mean(`Max_height(m)`, na.rm = TRUE), .groups = "drop_last") %>%
  pivot_wider(
    names_from  = Year,
    values_from = mean_height,
    names_glue  = "height_{Year}"
  ) %>%
  mutate(delta_Sp = height_2025 - height_2024) %>%
  drop_na(delta_Sp)   # keep groups where both years are present

### Make "Unfenced" the reference level (to see "fenced" coefficients)
# Check current class of FieldType

class(delta_heightSp$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
delta_heightSp$Fencing <- factor(delta_heightSp$Fencing, ordered = FALSE)

# Verify
levels(delta_heightSp$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level (to see "Unfenced" coefficients)
delta_heightSp$Fencing <- relevel(delta_heightSp$Fencing, ref = "Unfenced")

## Change Encroachment level referencing to Moderate so that High can be seen
# Check current class of FieldType

class(delta_heightSp$Encroachment_level)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
delta_heightSp$Encroachment_level <- factor(delta_heightSp$Encroachment_level, ordered = FALSE)

# Verify
levels(delta_heightSp$Encroachment_level)  # Should show "Open" "Closed" (or vice versa)

# Set "moderate" as the reference level 
delta_heightSp$Encroachment_level <- relevel(delta_heightSp$Encroachment_level, ref = "Moderate")



## Glmm 
SapH <- glmmTMB(delta_Sp ~ Treatment * Fencing + Treatment * Encroachment_level +
                  Fencing * Encroachment_level  + (1 | Site),
                data = delta_heightSp, family = gaussian(link = "identity"))
summary(SapH)


###with plot and subplot
SapH2 <- glmmTMB(delta_Sp ~ Treatment * Fencing + Treatment * Encroachment_level +
                  Fencing * Encroachment_level  + (1 | Site/Plot/Subplot),
                data = delta_heightSp, family = gaussian(link = "identity"))
summary(SapH2)

###
SapH4 <- glmmTMB(delta_Sp ~ Treatment * Fencing * Encroachment_level  + (1 | Site/Plot),
                 data = delta_heightSp, family = gaussian(link = "identity"))

summary(SapH4)



# check for model residuals and distribution
qqnorm(resid(SapH4))
qqline(resid(SapH4))

# For comprehensive diagnostics:
performance::check_convergence(SapH4) # TRUE the model converged

## MODEL performance
performance::check_model(SapH4)

##
VIF(SapH4) ## CHECK ON THIS,  

##Check for outliers
residuals_std <- scale(resid(SapH4))  # Standardize residuals
outliers <- which(abs(residuals_std) > 3)  # Find outliers
print(outliers)


## Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(SapH4))  #p < 0.05 → residuals are non-normal.
# result p = 0.9589, hence residuals are normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(SapH4), breaks = 5)
leveneTest(resid(SapH4) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.56 equal variance 

##Plot the conditional effects (e.g., using emmeans or ggeffects):
emm <- emmeans(SapH2, ~ Treatment | Fencing * Encroachment_level)
plot(emm)

##Converting results to excel.
# Export as excel 
results <- tidy(SapH2, conf.int = TRUE)
write.xlsx (results,"Data/3Delta Sapling Height GLMM_results.xlsx")


# --- 4. Visualise: boxplot of Δ‑ SAPLING height by INTERACTIONS -------------
SpHbx <- ggplot(delta_heightSp,
                aes(x = Treatment, y = delta_Sp , color = Fencing)) + facet_wrap(~Encroachment_level)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Sapling height (m)") +
  theme_beautiful() #

##saving BOXPLOT - INTERACTIONS
ggsave(SpHbx,filename ="Plots/2Delta Sapling Height INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")  

##Violin plot for SAPLING density Interactions
SpHvi <- ggplot(delta_heightSp,
                aes(x = Treatment, y = delta_Sp, color = Fencing)) + facet_wrap(~Encroachment_level)+   
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Sapling height (m)") +
  theme_beautiful() 

#saving VIOLIN PLOT - INTERACTIONS
ggsave(SpHvi,filename ="Plots/2Delta Sapling height INTERACTIONS Violinplot.png",
       width = 16, height = 14, units = "cm")  


#############################################################
######################################################## CUT STUMPS RESPROUT RESPROUT
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



### Make "Unfenced" the reference level (to see "fenced" coefficients)
# Check current class of FieldType

class(resprouts_df$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
resprouts_df$Fencing <- factor(resprouts_df$Fencing, ordered = FALSE)

# Verify
levels(resprouts_df$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level (to see "Unfenced" coefficients)
resprouts_df$Fencing <- relevel(resprouts_df$Fencing, ref = "Unfenced")

## Change Encroachment level referencing to Moderate so that High can be seen
# Check current class of FieldType

class(resprouts_df$Encroachment_level)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
resprouts_df$Encroachment_level <- factor(resprouts_df$Encroachment_level, ordered = FALSE)

# Verify
levels(resprouts_df$Encroachment_level)  # Should show "Open" "Closed" (or vice versa)

# Set "moderate" as the reference level 
resprouts_df$Encroachment_level <- relevel(resprouts_df$Encroachment_level, ref = "Moderate")

# Scale Variables to ensure convergence:
#resprouts_df$deltens_scaled <- as.numeric(scale(resprouts_df$No_of_resprouts))

## GLMM for resprouts


Resc2 <- glmmTMB(No_of_resprouts ~ Treatment * Fencing + Treatment * Encroachment_level
                 + Fencing * Encroachment_level + (1 | Site),
                 data = resprouts_df, family = gaussian (link = "identity"))
summary(Resc2)

##with plot and subplot
Resc3 <- glmmTMB(No_of_resprouts ~ Treatment * Fencing + Treatment * Encroachment_level
                 + Fencing * Encroachment_level + (1 | Site/Plot/Subplot),
                 data = resprouts_df, family = gaussian (link = "identity"))
summary(Resc3)

# check for model residuals and distribution
qqnorm(resid(Resc2))
qqline(resid(Resc2))

# For comprehensive diagnostics:
performance::check_convergence(Resc3) # TRUE the model converged

 
##Check for outliers
residuals_std <- scale(resid(Resc2))  # Standardize residuals
outliers <- which(abs(residuals_std) > 3)  # Find outliers
print(outliers)


## Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(Resc3))  #p < 0.05 → residuals are non-normal.
# result p = 0.000002, hence residuals are non-normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(Resc3), breaks = 5)
leveneTest(resid(Resc3) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
 # results p = 0.000002 unequal variance 


##Converting results to excel.
# Export as excel 
results <- tidy(Resc3, conf.int = TRUE)
write.xlsx (results,"Data/3 Resprouts GLMM_results.xlsx")

# --- 4. Visualise: boxplot of Δ‑ RESPROUTS by INTERACTIONS -------------
Resbx <- ggplot(resprouts_df,
                aes(x = Treatment, y = No_of_resprouts , color = Fencing)) + facet_wrap(~Encroachment_level)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "No. of resprouts on cut stumps") +
  theme_beautiful() #

##saving BOXPLOT - INTERACTIONS
ggsave(Resbx,filename ="Plots/RESPROUTS INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")  

##Violin plot for RESPROUTS Interactions
Resvi <- ggplot(resprouts_df,aes(x = Treatment, y = No_of_resprouts , color = Fencing)) + facet_wrap(~Encroachment_level)+    
  geom_violin(trim = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "No. of resprouts on cut stumps") +
  theme_beautiful() 

#saving VIOLIN PLOT - INTERACTIONS
ggsave(Resvi,filename ="Plots/RESPROUTS INTERACTIONS Violinplot.png",
       width = 16, height = 14, units = "cm")  



##########################################################################################
####################################### GRASSES GRASSES GRASSES GRASSES GRASSES

#Load data (DPM HEIGHT)
GrassES <- read_csv("DATA/March2025/GrassesCombinedCleaned2.csv")

#summary_Gr <- GrassES %>%
 # mutate(Encroachment_level = case_when(
  #    Site == "A" ~ "Moderate",
   #   Site == "B" ~ "Moderate",
    #  Site == "C" ~ "High",
     # Site == "D" ~ "High",
      #Site == "E" ~ "High",
      #Site == "F" ~ "Moderate",
      #TRUE        ~ NA_character_
    #)
  #) 
#write_csv(summary_Gr, "DATA/March2025/GrassesCombinedCleaned2.csv")

GraHeight <- GrassES %>%
  filter(!is.na(DPM_Height),
         Year %in% c(2024, 2025)) %>% 
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year, Encroachment_level) %>%
  summarise(mean_DPM_Height = mean(DPM_Height, na.rm = TRUE)) %>%
  ungroup() 


############## Compute grass Δ‑height (2024 − 2025) per Site/Plot/Subplot/Treatment/Fenced ----
deltaGra_height <- GrassES %>%
  group_by(Site, Plot, Subplot, Treatment, Fencing, Year, Encroachment_level) %>%
  summarise(mean_height = mean(DPM_Height, na.rm = TRUE), .groups = "drop_last") %>%
  pivot_wider(
    names_from  = Year,
    values_from = mean_height,
    names_glue  = "height_{Year}"
  ) %>%
  mutate(delta_Grs = height_2025 - height_2024) %>%
  drop_na(delta_Grs)   # keep groups where both years are present


#### Make "Unfenced" the reference level (to see "fenced" coefficients)
# Check current class of FieldType

class(deltaGra_height$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
deltaGra_height$Fencing <- factor(deltaGra_height$Fencing, ordered = FALSE)

# Verify
levels(deltaGra_height$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level (to see "Unfenced" coefficients)
deltaGra_height$Fencing <- relevel(deltaGra_height$Fencing, ref = "Unfenced")

## Change Encroachment level referencing to Moderate so that High can be seen
# Check current class of FieldType

class(deltaGra_height$Encroachment_level)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
deltaGra_height$Encroachment_level <- factor(deltaGra_height$Encroachment_level, ordered = FALSE)

# Verify
levels(deltaGra_height$Encroachment_level)  # Should show "Open" "Closed" (or vice versa)

# Set "Moderate" as the reference level 
deltaGra_height$Encroachment_level <- relevel(deltaGra_height$Encroachment_level, ref = "Moderate")

# Scale Variables to ensure convergence:
# deltaGra_height$deltens_scaled <- as.numeric(scale(deltaGra_height$mean_DPM_Height))

## GLMM for resprouts
Grsc1 <- glmmTMB(delta_Grs ~ Treatment * Fencing + Treatment * Encroachment_level
                 + Fencing * Encroachment_level + (1 | Site),
                 data = deltaGra_height, family = gaussian (link = "identity"))
#summary(Grsc1)  


# with plot and subplot
Grsc2 <- glmmTMB(delta_Grs ~ Treatment * Fencing + Treatment * Encroachment_level
                 + Fencing * Encroachment_level + (1 | Site/Plot/Subplot),
                 data = deltaGra_height, family = gaussian (link = "identity"))
summary(Grsc2)  


# For comprehensive diagnostics:
performance::check_convergence(Grsc2) # TRUE for Grsc1
  
# check for model residuals and distribution
qqnorm(resid(Grsc1))
qqline(resid(Grsc1))


##Check for outliers
residuals_std <- scale(resid(Grsc2))  # Standardize residuals
outliers <- which(abs(residuals_std) > 3)  # Find outliers
print(outliers)

## Shapiro-Wilk Test for Normal distribution of residuals

shapiro.test(resid(Grsc2))  # IF p < 0.05 → residuals are non-normal.
# result p = 0.1478, hence residuals are normally distributed. 
  #model meets the normality assumption for Gaussian models.

##B. Levene’s Test for Homoscedasticity
fitted_binned <- cut(fitted(Grsc1), breaks = 5)
leveneTest(resid(Grsc2) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
      # p= 0.083, thus there is equal variance

##Converting results to excel.
# Export as excel 
results <- tidy(Grsc2, conf.int = TRUE)
write.xlsx (results,"Data/3Delta Grass height GLMM_results.xlsx")


# --- 4. Visualise: boxplot of Δ‑ GRASS HEIGHT by INTERACTIONS -------------
Grsbx <- ggplot(deltaGra_height,
          aes(x = Treatment, y = delta_Grs , color = Fencing)) + facet_wrap(~Encroachment_level)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Mean grass height (cm)") +
  theme_beautiful() #

##saving BOXPLOT - INTERACTIONS
ggsave(Grsbx,filename ="Plots/GRASS HEIGHT INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")  

##Violin plot for RESPROUTS Interactions
Grsvi <- ggplot(deltaGra_height,
        aes(x = Treatment, y = delta_Grs , color = Fencing)) + facet_wrap(~Encroachment_level)+    
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Mean grass height (cm)") +
  theme_beautiful() 

#saving VIOLIN PLOT - INTERACTIONS
ggsave(Grsvi,filename ="Plots/GRASS HEIGHT INTERACTIONS Violinplot.png",
       width = 16, height = 14, units = "cm") 

##############################################################################
#################################### GRASSES GRASSES DELTA SHANON -WEINER DIVERSITY 

# 1. Prepare data: 
Gdiv <- GrassES%>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025))%>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Year, Fencing, Encroachment_level, Species_name)%>%
  summarise(Spp_count = n(), .groups = "drop" )


# Calculate Shannon-Wiener Diversity Index at Site and Plot level
Sdiversity_data <- Gdiv %>%
  group_by(Site, Plot, Subplot,Treatment, Year, Fencing, Encroachment_level) %>%                   # Group by Site and Plot
  summarise(
    Shannon_Diversity = -sum((Spp_count / sum(Spp_count)) * log(Spp_count / sum(Spp_count))),
    .groups = "drop"
  )


# 3. Pivot the two years side‑by‑side and compute Δ‑ ─────────────
SW_Delta <- Sdiversity_data %>% 
pivot_wider(names_from  = Year,
              values_from = Shannon_Diversity,
              names_glue  = "sw_{Year}") %>% 
mutate(delta_SW = sw_2025 - sw_2024)   


#### Make "Unfenced" the reference level (to see "fenced" coefficients)
# Check current class of FieldType

class(SW_Delta$Fencing)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
SW_Delta$Fencing <- factor(SW_Delta$Fencing, ordered = FALSE)

# Verify
levels(SW_Delta$Fencing)  # Should show "Open" "Closed" (or vice versa)

# Set "Fenced" as the reference level (to see "Unfenced" coefficients)
SW_Delta$Fencing <- relevel(SW_Delta$Fencing, ref = "Unfenced")

## Change Encroachment level referencing to Moderate so that High can be seen
# Check current class of FieldType

class(SW_Delta$Encroachment_level)  # Likely "character" or "ordered factor"

# Convert to unordered factor explicitly
SW_Delta$Encroachment_level <- factor(SW_Delta$Encroachment_level, ordered = FALSE)

# Verify
levels(SW_Delta$Encroachment_level)  # Should show "Open" "Closed" (or vice versa)

# Set "High" as the reference level (to see "Moderate" coefficients)
SW_Delta$Encroachment_level <- relevel(SW_Delta$Encroachment_level, ref = "Moderate")

# Scale Variables to ensure convergence:
# deltaGra_height$deltens_scaled <- as.numeric(scale(deltaGra_height$mean_DPM_Height))

## GLMM for resprouts
GrSW <- glmmTMB(delta_SW ~ Treatment * Fencing + Treatment * Encroachment_level
                 + Fencing * Encroachment_level + (1 | Site),
                 data = SW_Delta, family = gaussian (link = "identity"))
summary(GrSW)  

# with plot and subplot
GrSW2 <- glmmTMB(delta_SW ~ Treatment * Fencing + Treatment * Encroachment_level
                + Fencing * Encroachment_level + (1 | Site/Plot/Subplot),
                data = SW_Delta, family = gaussian (link = "identity"))
summary(GrSW2) 

# For comprehensive diagnostics:
performance::check_convergence(GrSW2) # TRUE for GrSW2

# check for model residuals and distribution
qqnorm(resid(GrSW))
qqline(resid(GrSW))


##Check for outliers
residuals_std <- scale(resid(GrSW))  # Standardize residuals
outliers <- which(abs(residuals_std) > 3)  # Find outliers
print(outliers)

## Shapiro-Wilk Test for Normal distribution of residuals

shapiro.test(resid(GrSW2))  #  if p < 0.05 → residuals are non-normal.
# result p = 0.000001, hence residuals are not normally distributed. 
 #Residuals are not normally distributed

##B. Levene’s Test for Homoscedasticity
fitted_binned <- cut(fitted(GrSW2), breaks = 5)
leveneTest(resid(GrSW2) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# p= 0.3131, thus there is equal variance

##Converting results to excel.
# Export as excel 
results <- tidy(GrSW2, conf.int = TRUE)
write.xlsx (results,"Data/3 Delta Grass diversity GLMM_results.xlsx")

# --- 4. Visualise: boxplot of Δ‑ GRASS DIVERSITY by INTERACTIONS -------------
Gdvbx <- ggplot(SW_Delta,
                aes(x = Treatment, y = delta_SW , color = Fencing)) + facet_wrap(~Encroachment_level)+ 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Grass Shannon-Weiner diversity") +
  theme_beautiful() #

##saving BOXPLOT - INTERACTIONS
ggsave(Gdvbx,filename ="Plots/GRASS diversity INTERACTIONS BOXplot.png",
       width = 16, height = 14, units = "cm")  

##Violin plot Δ‑ GRASS DIVERSITY by INTERACTIONS
Gdvi <- ggplot(SW_Delta,
      aes(x = Treatment, y = delta_SW , color = Fencing)) + facet_wrap(~Encroachment_level)+    
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Grass Shannon-Weiner diversity") +
  theme_beautiful() 

## saving VIOLIN PLOT - INTERACTIONS
ggsave(Gdvi,filename ="Plots/GRASS diversity Violinplot.png",
       width = 16, height = 14, units = "cm") 




##########################################################################################
############################ SQUARE ROOT TRANSFORMED VARIABLES

#Create a new transformed variable in your dataset:

SW_Delta$del_transformed <- sign(SW_Delta$delta_SW) * sqrt(abs(SW_Delta$delta_SW))

#Fit transformed model

model_transformed <- glmmTMB(del_transformed ~ Treatment * Fencing + 
    Treatment * Encroachment_level +
    Fencing * Encroachment_level + 
    (1 | Site),data = SW_Delta,
  family = gaussian(link = "identity"))  # Default; optional to specify

 summary(model_transformed)
 
 
 # For comprehensive diagnostics:
 performance::check_convergence(model_transformed) # TRUE for model_transformed
 
 # check for model residuals and distribution
 qqnorm(resid(model_transformed))
 qqline(resid(model_transformed))
 
 
 ##Check for outliers
 residuals_std <- scale(resid(model_transformed))  # Standardize residuals
 outliers <- which(abs(residuals_std) > 3)  # Find outliers
 print(outliers)
 
 ## Shapiro-Wilk Test for Normal distribution of residuals
 
 shapiro.test(resid(model_transformed))  #  if p < 0.05 → residuals are non-normal.
 # result p = 0.00375, hence residuals are not normally distributed. 
 #Residuals are not normally distributed
 
 ##B. Levene’s Test for Homoscedasticity
 fitted_binned <- cut(fitted(model_transformed), breaks = 5)
 leveneTest(resid(model_transformed) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
 # p= 0.4833, thus there is equal variance
 
 ##Converting results to excel.
 # Export as excel 
 results <- tidy(GrSW, conf.int = TRUE)
 write.xlsx (results,"Data/Grass diversity GLMM_results.xlsx")
 
 
 
 
 ######log transforming if square root fails to reduce non-normality
 # Best robust approach for mixed-sign data:

 SW_Delta$del_transformed <- sign(SW_Delta$delta_SW) * log1p(abs(SW_Delta$delta_SW))
 
#fit log transformed to model
 model_t <- glmmTMB(del_transformed ~ Treatment * Fencing + 
   Treatment * Encroachment_level +
   Fencing * Encroachment_level + 
   (1 | Site),data = SW_Delta,
   family = gaussian(link = "identity"))  # Default; optional to specify
 
 summary(model_t)
 
 shapiro.test(resid(model_t))  #  if p < 0.05 → residuals are non-normal.
 # result p = 0.001277, hence residuals are not normally distributed. 
 #Residuals are not normally distributed
 
 ##B. Levene’s Test for Homoscedasticity
 fitted_binned <- cut(fitted(model_t), breaks = 5)
 leveneTest(resid(model_t) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
 # p= 0.1205, thus there is equal variance
 
 results <- tidy(model_t, conf.int = TRUE)
 write.xlsx (results,"Data/log Grass diversity GLMM_results.xlsx")
 
 
################################################# LOG RESPROUTS RESPROUTS RESPROUTS
 
 # LOG TRANSFORMING
 resprouts_df$del_transformed <- sign(resprouts_df$No_of_resprouts) * log1p(abs(resprouts_df$No_of_resprouts))
 
 #fit log transformed to model
 model_rs <- glmmTMB(del_transformed ~ Treatment * Fencing + 
                      Treatment * Encroachment_level +
                      Fencing * Encroachment_level + 
                      (1 | Site),data = resprouts_df,
                    family = gaussian(link = "identity"))  # Default; optional to specify
 
 summary(model_rs)
 
 # For comprehensive diagnostics:
 performance::check_convergence(model_rs) # TRUE for model_transformed
 
 shapiro.test(resid(model_rs))  #  if p < 0.05 → residuals are non-normal.
 # result p = 0.001277, hence residuals are not normally distributed. 
 #Residuals are not normally distributed
 
 ##B. Levene’s Test for Homoscedasticity
 fitted_binned <- cut(fitted(model_rs), breaks = 5)
 leveneTest(resid(model_rs) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
 # p= 0.00000165, thus there is uequal variance
 
 #results <- tidy(model_t, conf.int = TRUE)
 #write.xlsx (results,"Data/log Grass diversity GLMM_results.xlsx")
 
 
 
###################################LOG TRANSFORMED TREE SIMPSONS DIVERSITY 
 
 tsdi_Delta$del_transformed <- sign(tsdi_Delta$Simpson_index1) * log1p(abs(tsdi_Delta$Simpson_index1))
 
 #fit log transformed to model
 model_ts1 <- glmmTMB(del_transformed ~ Treatment * Fencing + 
                       Treatment * Encroachment_level +
                       Fencing * Encroachment_level + 
                       (1 | Site/Plot/Subplot),data = tsdi_Delta,
                     family = gaussian(link = "identity"))  # Default; optional to specify
 
 summary(model_ts1)
 
 # For comprehensive diagnostics:
 performance::check_convergence(model_ts1) # TRUE for model_transformed
 
 shapiro.test(resid(model_ts1))  #  if p < 0.005 → residuals are non-normal.
 # result p = 0.0238, hence residuals are not normally distributed. 
 
 
 ##B. Levene’s Test for Homoscedasticity
 fitted_binned <- cut(fitted(model_ts1), breaks = 5)
 leveneTest(resid(model_ts1) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
 # p= 0.5488, thus there is equal variance
 
 
 
 tsdi1<- glmmTMB(Simpson_index1 ~ Treatment * Fencing + Treatment * Encroachment_level + Fencing * Encroachment_level  
                 + (1 | Site), 
                 data = tsdi_Delta,
                 family = gaussian(link = "identity"))

  <- (y ~ x + (1|group), data = your_data)
 
 robust_model <- rlmer(Simpson_index1 ~ Treatment * Fencing + 
              Treatment * Encroachment_level +
              Fencing * Encroachment_level + 
              (1 | Site/Plot/Subplot),data = tsdi_Delta, family = gaussian(link = "identity"))
 
 summary(robust_model)
 