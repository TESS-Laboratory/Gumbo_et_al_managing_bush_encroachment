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
library(here) ## MOST IMPORTANT NOT TO FORGET THIS ONE
library(robustbase)
library(sjPlot)
library(flextable)
library(officer)
library(glmmTMB)
library(performance)  # model diagnostics
library(stringi)
library(dataMaid)


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


GC <- read_csv("DATA/March2025/GrassesCombined_withFencing.csv")


# ----------------------------
# QUALITY CHECK (QC)
# ----------------------------

cat("---- BASIC STRUCTURE ----\n")
str(GC)
summary(GC)

cat("\n---- HEAD OF DATA ----\n")
print(head(df))

# Check for missing values
cat("\n---- MISSING VALUES ----\n")
print(colSums(is.na(GC)))

# Check for duplicate rows
cat("\n---- DUPLICATES ----\n")
duplicated_rows <- GC[duplicated(GC), ]
cat("Number of duplicated rows: ", nrow(duplicated_rows), "\n")

# Data types of each column
cat("\n---- DATA TYPES ----\n")
print(sapply(GC, class))

# Range and summary of numeric variables
cat("\n---- HEIGHT SUMMARY ----\n")
print(summary(GC$DPM_Height))

cat("\n---- Fencing ----\n")
print(summary(GC$Fencing))

# Unique values for categorical variables
cat("\n---- UNIQUE TREATMENTS ----\n")
print(unique(GC$Treatment))

cat("\n---- UNIQUE SPECIES ----\n")
print(length(unique(GC$Species_name)))
print(head(unique(GC$Species_name), 50))  # show first 10 species

# Outlier detection (boxplot)
boxplot(GC$DPM_Height, main = "Boxplot of Height", ylab = "DPM_Height", col = "lightblue")

####### ----------------------------########################################
# QUALITY ASSURANCE (QA)
# ----------------------------

# Cross-tabulations to check data consistency
cat("\n---- PLOT vs SUBPLOT COMBINATION ----\n")
print(table(GC$Plot, GC$Subplot))

cat("\n---- YEAR vs SUBPLOT COMBINATION ----\n")
print(table(GC$Year, GC$Subplot))

# Controlled vocabulary check
expected_treatments <- c("C", "F", "TF",
                         "TFB", "THF")
invalid_treatments <- setdiff(unique(GC$Treatment), expected_treatments)
cat("\n---- INVALID TREATMENT VALUES ----\n")
print(invalid_treatments)

####################################### QC QA
##### Convert to UTF-8 encoding
GC <- GC%>%
  mutate(across(everything(), ~iconv(., from = "latin1", to = "UTF-8")))


# 1. Standardizing case
GC <- GC %>% 
  mutate(Site = str_to_upper(Site),                 # keep this step
    Species_name = str_replace(                        # 1) make everything lower case
      str_to_lower(Species_name),       # 2) capitalise only the very first letter
      "^.",                             #    (the first character in the string)
      ~ str_to_upper(.x))                #    using a lambda replacement
  )


# 2. Trimming spaces
GC <- GC %>%
  mutate(Site = str_trim(Site),
         Species_name = str_trim(Species_name),
         Fencing = str_trim(Fencing),
         Treatment = str_trim(Treatment))
         

# Checking for consistency
unique_sites <- unique(GC$Site)
unique_species <- unique(GC$Species_name)
unique_fencing <- unique(GC$Fencing)
unique_treatemnt <- unique(GC$Treatment)

print("Unique values in 'Site' after standardization:")
print(unique_sites)

print("Unique values in 'Species_name' after standardization:")
print(unique_species)

# Summary of the cleaned data
print("Cleaned data:")
print(GC)

###### saving cleaned data
  #write_csv(GC, "DATA/March2025/GrassesCombinedCleaned.csv")

########################################################################################################
########################################################################################################
########################################################################################################

#Load data (DPM HEIGHT)
Grassnew <- read_csv("DATA/March2025/GrassesCombinedCleaned.csv")

summary_Gr <- Grassnew %>%
  filter(!is.na(DPM_Height),
         Year %in% c(2024, 2025)) %>% 
group_by(Site, Plot, Subplot, Treatment, Fencing, Year) %>%
  summarise(mean_DPM_Height = mean(DPM_Height, na.rm = TRUE)) %>%
  ungroup() 

Gr1 <- lmer(DPM_Height ~ Treatment * Fencing + (1 | Site),# random intercepts 
            data = Grassnew, REML = FALSE                       # use ML for comparing models
             )

#Gr2 <- lmer(DPM_Height ~ Treatment * Fencing * Year +
    #         (Treatment | Site),
     #      data = Grassnew, REML = FALSE)
  #summary(Gr1)


### marginal means of fencing, averaging over Treatment and Year
Gemm_fence <- emmeans(Gr1, ~ Fencing)          # or add , weights = "equal" if you prefer equal‑sized cells
Gemm_fence                                  # shows the two means

# test the difference
contrast(Gemm_fence, method = "revpairwise") # Fenced – Unfenced

####  Fencing effect within each treatment ; Testing effect of fencing on each treatment compared to control
  
Gemm_tf <- emmeans(Gr1, ~ Fencing | Treatment)   # fencing means conditioned on each treatment
pairs(Gemm_tf, adjust = "sidak")                # or "none", "bonferroni", "tukey", …


#Visualisation
 ggplot(Grassnew, aes(x = Fencing, y = DPM_Height, fill = Fencing)) + facet_wrap(~Treatment)+
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = " Grass DPM height (cm)") +
  theme_beautiful() +
  theme(legend.position = "none")

#ggsave
  #ggsave(G,filename ="Plots/Grass height Box plot.png",
   #    width = 16, height = 14, units = "cm")

##############################################################################
################################################# GRASS SPECIES RICHNESS

GrassComp <- read_csv("DATA/March2025/GrassesCombinedCleaned.csv")

# 1. Prepare data: richness per Site x Treatment x Year
Grass_year <- GrassComp %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025)) %>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Year) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop")

# Quick look
summary(Grass_year)



#######  Δ‑change (2025 – 2024) analysis --------------------------------------
# Pivot wider to compute site‑level change
grass_delta <- Grass_year %>%
  pivot_wider(names_from = Year, values_from = spp_richness, names_prefix = "Y") %>%
  mutate(delta = Y2025 - Y2024)

# Fit linear mixed model for delta (can go negative, so Gaussian assumption)
gmod_delta <- lmer(delta ~ Treatment + (1|Site), data = grass_delta)
summary(gmod_delta)


# Post‑hoc comparisons on delta
emm_delta <- emmeans(gmod_delta, ~ Treatment)
contrast(emm_delta, method = "pairwise")

# 5. Box plot of Δ‑change --------------------------------------------------
GrDELTA <- ggplot(grass_delta, aes(x = Treatment, y = delta, fill = Treatment)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Grass species richness") +
  theme_beautiful() +
  theme(legend.position = "none")

# ggsave
ggsave(GrDELTA,filename ="Plots/Delta Grass species richness Box plot.png",
       width = 16, height = 14, units = "cm")


### Grass spp richness according to fencing
# sort data
Grass_yearF <- GrassComp %>%
  filter(!is.na(Species_name),
         Year %in% c(2024, 2025)) %>%   # keep only pre/post years
  group_by(Site, Plot, Subplot, Treatment, Year, Fencing) %>%
  summarise(spp_richness = n_distinct(Species_name), .groups = "drop")

# Pivot wider to compute site‑level change
grass_deltaF <- Grass_yearF %>%
  pivot_wider(names_from = Year, values_from = spp_richness, names_prefix = "Y") %>%
  mutate(delta = Y2025 - Y2024)

# Fit linear mixed model for delta (can go negative, so Gaussian assumption)
gmod_deltaF <- lmer(delta ~ Treatment + (1|Site), data = grass_deltaF)
summary(gmod_deltaF)

# Treatment interaction with FENCING
gmod_deltaF <- lmer(delta ~ Treatment * Fencing +(1|Site), data = grass_deltaF)
summary(gmod_deltaF)

# Post‑hoc comparisons on delta
emm_deltaF <- emmeans(gmod_deltaF, ~ Treatment)
contrast(emm_deltaF, method = "pairwise")


#Visualisation spp richness according to fencing
GF <- ggplot(grass_deltaF, aes(x = Fencing, y = delta, fill = Fencing)) + facet_wrap(~Treatment)+
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  #geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Δ Grass species richness") +
  theme_beautiful() +
  theme(legend.position = "none")

#ggsave
ggsave(GF,filename ="Plots/Delta FENCE Grass species richness Box plot.png",
       width = 16, height = 14, units = "cm")




##############################################################################
######################### GRASS BIOMASS GRASS BIOMASS  USING TLS TLS TLS 

#LOAD DATA
WGtlsdata <- read_csv(here("DATA/DPM.csv"))


################## DPM HEIGHT ~ OVEN DRIED WEIGHT. NO SQUARE ROOT
# 1. Convert weight from grams to kg/ha
#    Area of 34cm diameter disc = π * (0.17 m)^2 = 0.0908 m²
frame_area <- pi * (0.17^2)  # = 0.0908 m²
WGtlsdata$Biomass_kg_ha <- WGtlsdata$Weight * 10 / frame_area


# 2.  Remove rows with missing Height or Weight_kg_ha.
# ------------------------------------------------------------------
df_tls <- WGtlsdata %>%
  filter(!is.na(DPH_Height), !is.na(Biomass_kg_ha))
stopifnot(nrow(df_tls) >= 3)

# 3.  Robust TLS helper
# ------------------------------------------------------------------
tls_fit <- function(x, y, through_origin = FALSE) {
  df <- na.omit(data.frame(x, y))
  if (nrow(df) < 2) stop("Need at least two points for TLS")
  
  # build the matrix for SVD
  if (through_origin) {
    A <- cbind(df$x, df$y)
  } else {
    A <- cbind(df$x - mean(df$x), df$y - mean(df$y))
  }
  
  V <- svd(A)$v
  if (ncol(V) < 2) stop("TLS failed: insufficient rank (x or y constant)")
  
  v2    <- V[, ncol(V)]            # always take the last right‑singular vector
  slope <- -v2[1] / v2[2]
  intercept <- if (through_origin) 0 else mean(df$y) - slope * mean(df$x)
  
  list(intercept = intercept, slope = slope)
}

# 4.  Fit the two TLS models
# ------------------------------------------------------------------
tls_free <- tls_fit(df_tls$DPH_Height, df_tls$Biomass_kg_ha)                # free intercept
tls_zero <- tls_fit(df_tls$DPH_Height, df_tls$Biomass_kg_ha, through_origin = TRUE)  # passes origin

n_obs    <- nrow(df_tls)
r2_proxy <- cor(df_tls$DPH_Height, df_tls$Biomass_kg_ha)^2                  # correlation² as proxy

# 4a.  Build annotation strings (include n)
# ------------------------------------------------------------------
eq_tls1 <- sprintf("y = %.2f + %.2fx\nTLS R² ≈ %.3f, n = %d", 
                   tls_free$intercept, tls_free$slope, r2_proxy, n_obs)

eq_tls0 <- sprintf("y = %.2fx\nTLS R² ≈ %.3f, n = %d", 
                   tls_zero$slope, r2_proxy, n_obs)

# 5.  Plot
# ------------------------------------------------------------------

TLSBIOM <- ggplot(df_tls, aes(x = DPH_Height, y = Biomass_kg_ha)) +
  geom_point() +
  geom_abline(intercept = tls_free$intercept, slope = tls_free$slope, colour = "red",  size = 1) +
  geom_abline(intercept = 0,                 slope = tls_zero$slope, colour = "blue", size = 1) +
  annotate("text",
           x = min(df_tls$DPH_Height, na.rm = TRUE),
           y = max(df_tls$Biomass_kg_ha, na.rm = TRUE),
           label = eq_tls1, hjust = 0, size = 3, colour = "red") +
  annotate("text",
           x = min(df_tls$DPH_Height, na.rm = TRUE),
           y = 0.80 * max(df_tls$Biomass_kg_ha, na.rm = TRUE),
           label = eq_tls0, hjust = 0, size = 3, colour = "blue") +
  labs(x = "DPM Height (cm)", y = "Standing grass biomass (kg/ha)") +
  theme_classic()

# ggsave
 #ggsave(TLSBIOM,filename ="Plots/TLS Biomass & intercept.png",
 #      width = 16, height = 14, units = "cm")



############################################## TLS WITH LOG TRANSFORMED VARIABLES

WGtdata <- read_csv(here("DATA/DPM.csv"))


################## DPM HEIGHT ~ OVEN DRIED WEIGHT. NO SQUARE ROOT
# 1. Convert weight from grams to kg/ha
#    Area of 34cm diameter disc = π * (0.17 m)^2 = 0.0908 m²
frame_area <- pi * (0.17^2)  # = 0.0908 m²
WGtdata$Biomass_kg_ha <- WGtdata$Weight * 10 / frame_area


# 2.  Remove rows with missing Height or Weight_kg_ha.
# ------------------------------------------------------------------
df_tlss <- WGtdata %>%
  filter(!is.na(DPH_Height), !is.na(Biomass_kg_ha), DPH_Height > 0, Biomass_kg_ha > 0) %>%
  mutate(
    log_DPH_Height       = log(DPH_Height),           # natural log
    log_Biomass_kg_ha = log(Biomass_kg_ha)
  )
stopifnot(nrow(df_tlss) >= 3)

# 3.  Robust TLS helper
# ------------------------------------------------------------------
tlss_fit <- function(x, y, through_origin = FALSE) {
  df <- na.omit(data.frame(x, y))
  if (nrow(df) < 2) stop("Need at least two points for TLS")
  
  # build the matrix for SVD
  if (through_origin) {
    A <- cbind(df$x, df$y)
  } else {
    A <- cbind(df$x - mean(df$x), df$y - mean(df$y))
  }
  
  V <- svd(A)$v
  if (ncol(V) < 2) stop("TLS failed: insufficient rank (x or y constant)")
  
  v2    <- V[, ncol(V)]            # always take the last right‑singular vector
  slope <- -v2[1] / v2[2]
  intercept <- if (through_origin) 0 else mean(df$y) - slope * mean(df$x)
  
  list(intercept = intercept, slope = slope)
}


#Log-transform both variables (natural log)
# 4.  Fit TLS models on the *log‑scale* -----------------------------
# ------------------------------------------------------------------
tlss_free  <- tlss_fit(df_tlss$log_DPH_Height, df_tlss$log_Biomass_kg_ha)                     # free intercept
tlss_zero  <- tlss_fit(df_tlss$log_DPH_Height, df_tlss$log_Biomass_kg_ha, through_origin = TRUE) # through origin

n_obs     <- nrow(df_tlss)
r2_proxy  <- cor(df_tlss$log_DPH_Height, df_tlss$log_Biomass_kg_ha)^2                        # proxy R² on log scale

# 4a.  Build annotation strings (include n) -------------------------
# ------------------------------------------------------------------
eq_tls1 <- sprintf("ln(y) = %.2f + %.2f·ln(x)\nTls R² ≈ %.3f, n = %d", 
                   tlss_free$intercept, tlss_free$slope, r2_proxy, n_obs)

eq_tls0 <- sprintf("ln(y) = %.2f·ln(x)\nTls R² ≈ %.3f, n = %d", 
                   tlss_zero$slope, r2_proxy, n_obs)

##5.  Plot on log‑log scale ----------------------------------------
  # ------------------------------------------------------------------

tlsf <- ggplot(df_tlss, aes(x = log_DPH_Height, y = log_Biomass_kg_ha)) +
  geom_point() +
  geom_abline(intercept = tlss_free$intercept, slope = tlss_free$slope, colour = "red",  size = 1) +
  geom_abline(intercept = 0,                    slope = tlss_zero$slope, colour = "blue", size = 1) +
  annotate("text",
           x = min(df_tlss$log_DPH_Height, na.rm = TRUE),
           y = max(df_tlss$log_Biomass_kg_ha, na.rm = TRUE),
           label = eq_tls1, hjust = 0, size = 3, colour = "red") +
  annotate("text",
           x = min(df_tlss$log_DPH_Height, na.rm = TRUE),
           y = 0.85 * max(df_tlss$log_Biomass_kg_ha, na.rm = TRUE),
           label = eq_tls0, hjust = 0, size = 3, colour = "blue") +
  labs(x = "ln(DPM height (cm)", y = "ln(Standing grass biomass (kg/ha)") +
  theme_beautiful()


# ggsave
 # ggsave(tlsf,filename ="Plots/TLS LOG Biomass.png",
   #    width = 16, height = 14, units = "cm")



################################################
################################################# TLS WITH SQUARE ROOTED VARIABLES

WGsqtdata <- read_csv(here("DATA/DPM.csv"))


################## DPM HEIGHT ~ OVEN DRIED WEIGHT. NO SQUARE ROOT
# 1. Convert weight from grams to kg/ha
#    Area of 34cm diameter disc = π * (0.17 m)^2 = 0.0908 m²
frame_area <- pi * (0.17^2)  # = 0.0908 m²
WGsqtdata$Biomass_kg_ha <- WGsqtdata$Weight * 10 / frame_area


# 2.  Remove rows with missing Height or Weight_kg_ha.
# ------------------------------------------------------------------
df_tlsq <- WGsqtdata %>%
  filter(!is.na(DPH_Height), !is.na(Biomass_kg_ha), DPH_Height > 0, Biomass_kg_ha > 0) %>%
  mutate(
    sqrt_DPH_Height       = sqrt(DPH_Height),           
    sqrt_Biomass_kg_ha = sqrt(Biomass_kg_ha) 
  )
stopifnot(nrow(df_tlsq) >= 3)

# 3.  Robust TLS helper
# ------------------------------------------------------------------
tlsq_fit <- function(x, y, through_origin = FALSE) {
  df <- na.omit(data.frame(x, y))
  if (nrow(df) < 2) stop("Need at least two points for TLS")
  
  # build the matrix for SVD
  if (through_origin) {
    A <- cbind(df$x, df$y)
  } else {
    A <- cbind(df$x - mean(df$x), df$y - mean(df$y))
  }
  
  V <- svd(A)$v
  if (ncol(V) < 2) stop("TLS failed: insufficient rank (x or y constant)")
  
  v2    <- V[, ncol(V)]            # always take the last right‑singular vector
  slope <- -v2[1] / v2[2]
  intercept <- if (through_origin) 0 else mean(df$y) - slope * mean(df$x)
  
  list(intercept = intercept, slope = slope)
}


#Log-transform both variables (natural log)
# 4.  Fit TLS models on the *log‑scale* -----------------------------
# ------------------------------------------------------------------
tlsq_free  <- tlsq_fit(df_tlsq$sqrt_DPH_Height, df_tlsq$sqrt_Biomass_kg_ha)                     # free intercept
tlsq_zero  <- tlsq_fit(df_tlsq$sqrt_DPH_Height, df_tlsq$sqrt_Biomass_kg_ha, through_origin = TRUE) # through origin

n_obs     <- nrow(df_tlsq)
r2_proxy  <- cor(df_tlsq$sqrt_DPH_Height, df_tlsq$sqrt_Biomass_kg_ha)^2                        # proxy R² on log scale

# 4a.  Build annotation strings (include n) -------------------------
# ------------------------------------------------------------------
eq_tlsq1 <- sprintf("y = %.2f + %.2f·sqrt(x)\n R² ≈ %.3f, n = %d", 
                   tlsq_free$intercept, tlsq_free$slope, r2_proxy, n_obs)

eq_tlsq0 <- sprintf("y = %.2f·sqrt(x)\n R² ≈ %.3f, n = %d", 
                   tlsq_zero$slope, r2_proxy, n_obs)

##5.  Plot on log‑log scale ----------------------------------------
# ------------------------------------------------------------------

tlsq1 <- ggplot(df_tlsq, aes(x = sqrt_DPH_Height, y = sqrt_Biomass_kg_ha)) +
  geom_point() +
  geom_abline(intercept = tlsq_free$intercept, slope = tlsq_free$slope, colour = "red",  size = 1) +
  geom_abline(intercept = 0,                    slope = tlsq_zero$slope, colour = "blue", size = 1) +
  annotate("text",
           x = min(df_tlsq$sqrt_DPH_Height, na.rm = TRUE),
           y = max(df_tlsq$sqrt_Biomass_kg_ha, na.rm = TRUE),
           label = eq_tlsq1, hjust = 0, size = 3, colour = "red") +
  annotate("text",
           x = min(df_tlsq$sqrt_DPH_Height, na.rm = TRUE),
           y = 0.75 * max(df_tlsq$sqrt_Biomass_kg_ha, na.rm = TRUE),
           label = eq_tlsq0, hjust = 0, size = 3, colour = "blue") +
  labs(x = "Sqrt DPM height (cm)", y = " Sqrt Standing grass biomass (kg/ha)") +
  theme_beautiful()


# ggsave
#ggsave(tlsq1,filename ="Plots/TLS SQRT Biomass & intercept.png",
 #     width = 16, height = 14, units = "cm")

