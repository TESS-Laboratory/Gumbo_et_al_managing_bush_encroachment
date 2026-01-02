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
library(MASS)
library(RobustLinearReg)
library(cowplot)   # for draw_* helpers
library(png)


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


##############################################################################
######################### GRASS BIOMASS GRASS BIOMASS  USING TLS TLS TLS 

#LOAD DATA for TLS BIOMASS and intercept line - UNTRANSFORMED VARIABLES
WGtlsdata <- read_csv("DATA/DPM.csv")


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
eq_tls1 <- sprintf("y = %.2f + %.2fx\nn = %d", 
                   tls_free$intercept, tls_free$slope, as.integer (n_obs))

eq_tls0 <- sprintf("y = %.2f + %.2fx\nn = %d", 
                   tls_zero$intercept, tls_zero$slope, as.integer(n_obs))

# 5.  Plot
# ------------------------------------------------------------------

ggplot(df_tls, aes(x = DPH_Height, y = Biomass_kg_ha)) +
  geom_point() +
  geom_abline(intercept = tls_free$intercept, slope = tls_free$slope, colour = "red",  size = 1) +
  geom_abline(intercept = 0,                slope = tls_zero$slope, colour = "blue", size = 1) +
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


############################################## TLS WITH LOG TRANSFORMED VARIABLES

WGtdata <- read_csv("DATA/DPM.csv")


################## DPM HEIGHT ~ OVEN DRIED WEIGHT. 
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

eq_tls1 <- sprintf("ln(y) = %.2f + %.2f·ln(x)\nn = %d", 
                   tlss_free$intercept, tlss_free$slope, as.integer (n_obs))


eq_tls0 <- sprintf("ln(y) = %.2f + %.2f·ln(x)\nn = %d", 
                   tlss_zero$intercept, tlss_zero$slope, as.integer(n_obs))

##5.  Plot on log‑log scale ----------------------------------------
# ------------------------------------------------------------------

 ggplot(df_tlss, aes(x = log_DPH_Height, y = log_Biomass_kg_ha)) +
  geom_point() +
  geom_abline(intercept = tlss_free$intercept, slope = tlss_free$slope, colour = "red",  size = 1) +
  geom_abline(intercept = 0,                    slope = tlss_zero$slope, colour = "blue", size = 1) +
  annotate("text",
           x = min(df_tlss$log_DPH_Height, na.rm = TRUE),
           y = max(df_tlss$log_Biomass_kg_ha, na.rm = TRUE),
           label = eq_tls1, hjust = 0, size = 4, colour = "red") +
  annotate("text",
           x = min(df_tlss$log_DPH_Height, na.rm = TRUE),
           y = 0.85 * max(df_tlss$log_Biomass_kg_ha, na.rm = TRUE),
           label = eq_tls0, hjust = 0, size = 4, colour = "blue") +
  labs(x = "ln DPM height (cm)", y = "ln Standing grass biomass (kg/ha)") +
  theme_beautiful()



################################################
################################################### TLS WITH SQUARE ROOTED VARIABLES

WGsqtdata <- read_csv("DATA/DPM.csv")


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


#Sqrt-transform both variables (sqrt)
# 4.  Fit TLS models on the *sqrt‑scale* -----------------------------
# ------------------------------------------------------------------
tlsq_free  <- tlsq_fit(df_tlsq$sqrt_DPH_Height, df_tlsq$sqrt_Biomass_kg_ha)                     # free intercept
tlsq_zero  <- tlsq_fit(df_tlsq$sqrt_DPH_Height, df_tlsq$sqrt_Biomass_kg_ha, through_origin = TRUE) # through origin

n_obs     <- nrow(df_tlsq)
r2_proxy  <- cor(df_tlsq$sqrt_DPH_Height, df_tlsq$sqrt_Biomass_kg_ha)^2                        # proxy R² on log scale

# 4a.  Build annotation strings (include n) 

eq_tlsq1 <- sprintf("sqrt (y) = %.2f + %.2f·sqrt(x)\nn = %d", 
                    tlsq_free$intercept, tlsq_free$slope, as.integer (n_obs))


eq_tlsq0 <- sprintf("sqrt (y) = %.2f + %.2f·sqrt(x)\nn = %d", 
                    tlsq_zero$intercept, tlsq_zero$slope, as.integer(n_obs))

##5.  Plot on sqrt scale ----------------------------------------

ggplot(df_tlsq, aes(x = sqrt_DPH_Height, y = sqrt_Biomass_kg_ha)) +
  geom_point() +
  geom_abline(intercept = tlsq_free$intercept, slope = tlsq_free$slope, colour = "red",  size = 1) +
  geom_abline(intercept = 0,                    slope = tlsq_zero$slope, colour = "blue", size = 1) +
  annotate("text",
           x = min(df_tlsq$sqrt_DPH_Height, na.rm = TRUE),
           y = max(df_tlsq$sqrt_Biomass_kg_ha, na.rm = TRUE),
           label = eq_tlsq1, hjust = 0, size = 4, colour = "red") +
  annotate("text",
           x = min(df_tlsq$sqrt_DPH_Height, na.rm = TRUE),
           y = 0.75 * max(df_tlsq$sqrt_Biomass_kg_ha, na.rm = TRUE),
           label = eq_tlsq0, hjust = 0, size = 4, colour = "blue") +
  labs(x = "Sqrt DPM height (cm)", y = " Sqrt Standing grass biomass (kg/ha)") +
  theme_beautiful()



# METHOD 1: Direct extraction of model coefficients
if (exists("tlsq_free")) {
  cat("YOUR MODEL COEFFICIENTS:\n")
  cat("=======================\n")
  cat(sprintf("Intercept: %.4f\n", tlsq_free$intercept))
  cat(sprintf("Slope: %.4f\n", tlsq_free$slope))
  cat(sprintf("Model: sqrt(Biomass) = %.4f + %.4f × sqrt(Height)\n", 
              tlsq_free$intercept, tlsq_free$slope))
}

# Quick check
cat("Checking your two models:\n")
cat("=========================\n")
if(exists("tlsq_free")) {
  cat("Red model coefficients:\n")
  cat(sprintf("  Intercept: %.4f\n", tlsq_free$intercept))
  cat(sprintf("  Slope: %.4f\n", tlsq_free$slope))
}
if(exists("tlsq_zero")) {
  cat("\nBlue model coefficients:\n")
  cat(sprintf("  Intercept: 0 (forced)\n"))
  cat(sprintf("  Slope: %.4f\n", tlsq_zero$slope))
}


