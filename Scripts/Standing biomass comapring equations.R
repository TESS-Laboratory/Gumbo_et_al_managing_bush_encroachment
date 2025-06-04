library(tidyverse)
library(lmerTest)
library(ggplot2)
library(Matrix)
library(lme4)
library(emmeans)
library(robustbase)
library(sjPlot)


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
        linetype = "blank")
    )
  }


## REDOING BIOMASS MODELS USING ACTUAL DATA POINTS

WGdata <- read_csv("DATA/DPM.csv")

################## DPM HEIGHT ~ OVEN DRIED WEIGHT. NO SQUARE ROOT
# 2. Convert weight from grams to kg/ha
#    Area of 34cm diameter disc = π * (0.17 m)^2 = 0.0908 m²
frame_area <- pi * (0.17^2)  # = 0.0908 m²
WGdata$Biomass_kg_ha <- WGdata$Weight * 10 / frame_area

# 3. Linear regression: Biomass ~ DPH
modelWG <- lm(Biomass_kg_ha ~ DPH_Height, data = WGdata)
#summary(modelWG)
 tab_model(modelWG)

 ###adding annotation to the plot
 # Fit the model
 modelWG <- lm(Biomass_kg_ha ~ DPH_Height, data = WGdata)
 
 # Extract coefficients
    coefs <- coef(modelWG)
   intercept <- round(coefs[1], 2)
    slope <- round(coefs[2], 2)
 
 # Create equation string for annotation
 # Build the equation string (biomass = intercept + slope * x)
    eq <- paste0("Biomass== ", intercept, " + ", slope, " %*% Dpm")
 
 
 # Plot with regression and equation
 ggplot(WGdata, aes(x = DPH_Height, y = Biomass_kg_ha)) +
   geom_point() +
   geom_smooth(method = "lm", se = TRUE, color = "red") +
   annotate("text", x = min(WGdata$DPH_Height, na.rm = TRUE), 
            y = max(WGdata$Biomass_kg_ha, na.rm = TRUE), 
          label = eq, parse = TRUE, hjust = 0, size = 3, color = "red") +
   labs(
     x = "Dpm height (cm)",
     y = "Standing grass biomass (kg/ha)"
   ) +
   theme_beautiful()
 

 
 ##### SQAURE ROOT OF DPH * BIOMASS MODEL
# WGdata <- WGdata %>% 
      #mutate(sqrt_dpm = sqrt(DPH_Height))
 
 #modelWG2 <- lm(Biomass_kg_ha ~ sqrt_dpm, data = WGdata)
   #summary(modelWG)
   #tab_model(modelWG2)
 
# Create scatter plot with regression line
ggplot(UGdata, aes(x = DPH_Height, y = Biomass_kg_ha)) +
  geom_point() +  # scatter plot
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # regression line
  labs(
    x = "DPM Height (cm)",
    y = "Standing Biomass (kg/ha)") +
  theme_beautiful()

### USING THE TROLLOPE EQUATION TO COMPARE IT WITH MY MODEL
# Step 1: Prepare WGdata

WGdata$sqrtDPH_Height <- sqrt(WGdata$DPH_Height)

# Step 2: Create theoretical model predictions using the equation
WGdata$TR_model <- -3019 + 2260 * WGdata$sqrtDPH_Height

# Step 3: Theoretical model: y = -3019 + 2260 * sqrt(DP)
TR_model_data <- data.frame(
  sqrtDPH_Height = WGdata$sqrtDPH_Height,
  Biomass_kg_ha = -3019 + 2260 * WGdata$sqrtDPH_Height,
  Source = "Trollope and Potgieter 1986")

# Step 4: Add source to WGdata
WG_actual_data <- data.frame(
  sqrtDPH_Height = WGdata$sqrtDPH_Height,
  Biomass_kg_ha = WGdata$Biomass_kg_ha,
  Source = "SHR")

# Step 5: Combine both for plotting
combined <- rbind(WG_actual_data, TR_model_data)

# Step 6: Plot with legend
ggplot(combined, aes(x = sqrtDPH_Height, y = Biomass_kg_ha, color = Source)) +
  geom_point(data = subset(combined, Source == "SHR"), size = 2) +
  geom_smooth(data = subset(combined, Source == "SHR"),
              method = "lm", se = FALSE) +
  geom_line(data = subset(combined, Source == "Trollope and Potgieter 1986"),
            linetype = "dashed", size = 1) +
  labs(
    x = "Square root DPM heigh (cm)",
    y = "Standing Biomass (kg/ha)",
    color = "Model"
  ) +
  theme_beautiful() + theme(
    legend.position = c(0.1, 1),
    legend.justification = c(0, 1),
    legend.box.just = "left",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = "gray90"))


######################## COMPARE EQUATIONS FOR UNTRANSFORMED DPM HEIGHTS #####

UGdata <- read_csv("DATA/DPM.csv")
# 2. Convert weight from grams to kg/ha
#    Area of 34cm diameter disc = π * (0.17 m)^2 = 0.0908 m²
frame_area <- pi * (0.17^2)  # = 0.0908 m²
UGdata$Biomass_kg_ha <- UGdata$Weight * 10 / frame_area

# # Step 2: Add WG Data and TR Model as long-format data
UG_actual_data <- data.frame(
  DPH_Height = UGdata$DPH_Height,
  Biomass_kg_ha = UGdata$Biomass_kg_ha,
  Source = "SHR",
  stringsAsFactors = FALSE)


# # Step 3: Create TR model data using sqrt(DPH)
TR_model_data <- data.frame(
  DPH_Height = UGdata$DPH_Height,
  Biomass_kg_ha = -3019 + 2260 * sqrt(UGdata$DPH_Height),
  Source = "Trollope and Potgieter 1986",
  stringsAsFactors = FALSE)

# Step 4: Combine datasets
combined <- rbind(UG_actual_data, TR_model_data)

# Step 5: Plot both datasets without filtering (no subset!)
ggplot(combined, aes(x = DPH_Height, y = Biomass_kg_ha, color = Source)) +
  geom_point(data = UG_actual_data, size = 2) +  # scatter for UG
  geom_smooth(data = UG_actual_data, method = "lm", se = FALSE) +  # fit for UG
  geom_line(data = TR_model_data, linetype = "dashed", size = 1) +  # line for TR
  labs(
    x = "DPM height (cm)",
    y = "Standing Biomass (kg/ha)",
    color = "Model"
  ) +
  theme_beautiful() + 
  theme(
    legend.position = c(0.1, 1),
    legend.justification = c(0, 1),
    legend.box.just = "left",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = "gray90"))


##################
##################################################################################
####### CHECKING THE MODEL THAT WAS USED BY SHR ON THEIR DPM

SHRmodel <- read_csv("DATA/March2025/SHR_model.csv")


# 3. Linear regression: Biomass ~ DPH
modelSHR <- lm(Biomass ~ Dpm, data = SHRmodel)
#summary(modelSHR)
 tab_model(modelSHR)

# Create scatter plot with regression line
ggplot(SHRmodel, aes(x = Dpm, y = Biomass)) +
  geom_point() +  # scatter plot
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # regression line
  labs(
    x = "DPM Height (cm)",
    y = "Standing Biomass (kg/ha)") + theme_classic()
  #+ theme_beautiful()

######COMPARING MODELS FOR 2024 AND 2025 ###############################

df <- read_csv("DATA/March2025/SHR_modelYear.csv")

# Ensure 'year' is a factor so ggplot treats it as a group
df$Year <- as.factor(df$Year)

# Create the plot
ggplot(df, aes(x = Dpm, y = Biomass, color = Year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "DPM height (cm)",
    y = "Standing Biomass (kg/ha)",
    color = "Year"
  ) +
  theme_beautiful() + theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.box.just = "left")

#####################################################################################
###################### FITTING THE CALCULATED OVEN DRIED BIOMASS  TO THE TWO PREEXISTING MODELS ######

df <- read_csv("DATA/March2025/SHR_modelYear.csv")

# Read and prepare data

df$Year <- as.factor(df$Year)
df$Dpm <- as.numeric(as.character(df$Dpm))
df <- df[is.finite(df$Dpm), ]

## Create custom line data (PRT)
dpm_vals <- seq(dpm_range[1], dpm_range[2], length.out = 100)
custom_line <- data.frame(
  Dpm = dpm_vals,
  Biomass = 107.06 + 171.69 * dpm_vals,
  Year = "SHR")


# Combine data to control legend together
df_combined <- rbind(
  df[, c("Dpm", "Biomass", "Year")],
  custom_line)

# Plot all at once using combined data
ggplot(df_combined, aes(x = Dpm, y = Biomass, color = Year)) +
  geom_point(data = df) +  # Only original data points
  geom_smooth(data = df, method = "lm", se = FALSE) +  # Regression lines for 2009 & 2013
  geom_line(data = custom_line, linetype = "solid", size = 1) +  # Custom model
  labs(
    x = "DPM Height (cm)",
    y = "Biomass (kg/ha)",
    color = "Model"
  ) +
  theme_beautiful() + 
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.box.just = "left")

#ggsave(DF,filename ="Plots/SHR BiomassComparisons.png",
 #      width = 16, height = 14, units = "cm" )



########################## ANNOTATING EQUATIONS for each model #################

df$Year <- as.factor(df$Year)
df$Dpm <- as.numeric(as.character(df$Dpm))
df <- df[is.finite(df$Dpm), ]

# Define the range for Dpm values (based on your actual data)
dpm_range <- range(df$Dpm, na.rm = TRUE)

# Create a sequence of Dpm values across the range
dpm_vals <- seq(dpm_range[1], dpm_range[2], length.out = 100)

# Define the custom line (SHR model)
custom_line <- data.frame(
  Dpm = dpm_vals,
  Biomass = 107.06 + 171.69 * dpm_vals,
  Year = "SHR")

df_combined <- rbind(
  df[, c("Dpm", "Biomass", "Year")],
  custom_line)

# Fit models for 2014 and 2015
model_2024 <- lm(Biomass ~ Dpm, data = subset(df, Year == "2024"))
model_2025 <- lm(Biomass ~ Dpm, data = subset(df, Year == "2025"))
#tab_model(model_2024)
#tab_model(model_2025)


# Extract and round coefficients
coef_2024 <- round(coef(model_2024), 2)
coef_2025 <- round(coef(model_2025), 2)
coef_SHR  <- c(171.69, 107.06)  # slope, intercept as per your earlier custom line

# Format equations as: biomass = slope + intercept * dpm
eq_2024 <- paste0("biomass = ", coef_2024[2], " + ", coef_2024[1], " * Dpm")
eq_2025 <- paste0("biomass = ", coef_2025[2], " + ", coef_2025[1], " * Dpm")
eq_SHR  <- paste0("biomass = ", coef_SHR[1], " + ", coef_SHR[2], " * Dpm")

# Plot
ggplot(df_combined, aes(x = Dpm, y = Biomass, color = Year)) +
  geom_point(data = df) +
  geom_smooth(data = df, method = "lm", se = FALSE) +
  geom_line(data = custom_line, linetype = "solid", size = 1) +
  scale_color_manual(values = c("2024" = "green", "2025" = "purple", "Trollope" = "blue" ,"SHR" = "red")) +
  labs(x = "DPM height (cm)", y = "Standing grass biomass (kg/ha)", color = "Model") +
  
  # Annotations to far left, stacked vertically
  annotate("text", x = 25, y = 2800, label = eq_2024, hjust = 0, color = "green", size = 4) +
  annotate("text", x = 25, y = 2200, label = eq_2025, hjust = 0, color = "purple", size = 4) +
  annotate("text", x = 25, y = 1600, label = eq_SHR, hjust = 0, color = "red", size = 4) +
  theme_beautiful()+ 
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 0.8),
    legend.box.just = "left")


### FOUR MODELS INCLUDING ANNOTATIONS
#df$Year <- as.factor(df$Year)
df$Dpm <- as.numeric(as.character(df$Dpm))
df <- df[is.finite(df$Dpm), ]

# Define the range for Dpm values (based on your actual data)
dpm_range <- range(df$Dpm, na.rm = TRUE)

# Create a sequence of Dpm values across the range
dpm_vals <- seq(dpm_range[1], dpm_range[2], length.out = 100)

# Define the custom line (SHR model)
custom_line <- data.frame(
  Dpm = dpm_vals,
  Biomass = 107.06 + 171.69 * dpm_vals,
  Year = "SHR")

# Ddefine Trollope line
trollope_line <- data.frame(
  Dpm = dpm_vals,
  Biomass = -3019 + 2260 * sqrt(dpm_vals),
  Year = "Trollope and Potgieter 1986")

#df_combined <- rbind(
# df[, c("Dpm", "Biomass", "Year")],
#custom_line, trollope_line)

df_combined <- rbind(
  df_combined,
  trollope_line)

# Fit models for 2014 and 2015
model_2024 <- lm(Biomass ~ Dpm, data = subset(df, Year == "2024"))
model_2025 <- lm(Biomass ~ Dpm, data = subset(df, Year == "2025"))
#tab_model(model_2024)
#tab_model(model_2025)


# Extract and round coefficients
coef_2024 <- round(coef(model_2024), 2)
coef_2025 <- round(coef(model_2025), 2)
coef_SHR  <- c(171.69, 107.06)  # slope, intercept as per your earlier custom line
coef_Trollope <- c(-3019, 2260)

# Format equations as: biomass = slope + intercept * dpm
eq_2024 <- paste0("biomass = ", coef_2024[2], " + ", coef_2024[1], " * Dpm")
eq_2025 <- paste0("biomass = ", coef_2025[2], " + ", coef_2025[1], " * Dpm")
eq_SHR  <- paste0("biomass = ", coef_SHR[1], " + ", coef_SHR[2], " * Dpm")
eq_Trollope <- paste0("Biomass == ", round(coef_Trollope[1], 2), 
                      " + ", round(coef_Trollope[2], 2), " * sqrt(Dpm)")

##PLOT AND ANNOTATION
AAN <- ggplot(df_combined, aes(x = Dpm, y = Biomass, color = Year)) +
  geom_point(data = df) +
  geom_smooth(data = df, method = "lm", se = FALSE) +
  geom_line(data = custom_line, linetype = "solid", size = 1) +
  geom_line(data = trollope_line, aes(x = Dpm, y = Biomass, color = "Trollope"),
            linetype = "dashed", size = 1) +  # <--- ADD THIS
  
  scale_color_manual(values = c(
    "2024" = "green",
    "2025" = "purple",
    "Trollope" = "blue",
    "SHR" = "red"
  )) +
  
  labs(x = "DPM height (cm)", y = "Standing grass biomass (kg/ha)", color = "Model") +
  
  # Annotations (including Trollope)
  annotate("text", x = 25, y = 2800, label = eq_2024, hjust = 0, color = "green", size = 4) +
  annotate("text", x = 25, y = 2000, label = eq_2025, hjust = 0, color = "purple", size = 4) +
  annotate("text", x = 25, y = 1200, label = eq_SHR, hjust = 0, color = "red", size = 4) +
  annotate("text", x = 25, y = 600, label = eq_Trollope, hjust = 0, color = "blue", size = 4) +
  
  theme_beautiful()+ 
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 0.8),
    legend.box.just = "left")


ggsave(AAN,filename ="Plots/Biomass-4MODELS& ANNOTATION.png",
       width = 16, height = 14, units = "cm" )







###############################################################################
###### FIT 4 MODELS IN ONE PLOT

####### Ensure df is clean and structured properly
df$Year <- as.factor(df$Year)
df$Dpm <- as.numeric(as.character(df$Dpm))
df$Biomass <- as.numeric(as.character(df$Biomass))
#df$DPH_Height <- as.numeric(as.character(df$DPH_Height))
df <- df[is.finite(df$Dpm), ]

# --- Model 1 & 2: Linear regressions for 2009 and 2013 (in df)

# --- Model 3: PRT model (y = 17.06 + 17.69 * dpm)
dpm_vals <- seq(min(df$Dpm), max(df$Dpm), length.out = 100)
prt_model <- data.frame(
  x = dpm_vals,
  y = 107.06 + 171.69 * dpm_vals,
  year = "SHR")

# --- Model 4: Tr model (y = -3019 + 2260 * sqrt(DPH_Height))
dph_vals <- seq(min(df$Dpm), max(df$Dpm), length.out = 100)
tr_model <- data.frame(
  x = dph_vals,
  y = -3019 + 2260 * sqrt(dph_vals),
  year = "Trollope and Potgieter 1986")

# --- Main df: Rename to match format
df_main <- df[, c("Dpm", "Biomass", "Year")]
colnames(df_main) <- c("x", "y", "year")

# --- Combine everything
combined_df <- rbind(df_main, prt_model, tr_model)

# --- Plot
DF2 <- ggplot(combined_df, aes(x = x, y = y, color = year)) +
  geom_point(data = df_main) +
  geom_smooth(data = df_main, method = "lm", se = FALSE) +
  geom_line(data = prt_model, linetype = "solid", size = 1) +
  geom_line(data = tr_model, linetype = "solid", size = 1.1) +
  labs(
    x = "DPM height (cm)",
    y = "Standing Biomass (kg/ha) ",
    color = "Model"
  ) +
  theme_beautiful() + 
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 0.8),
    legend.box.just = "left")

ggsave(DF2,filename ="Plots/2SHR Biomass-4MODELS.png",
             width = 16, height = 14, units = "cm" )


########################################################################
############# USING SQUARE ROOT FOR ALL THE MODELS  #########

# Ensure data is numeric and clean
df$Year <- as.factor(df$Year)
df$Dpm <- as.numeric(as.character(df$Dpm))
df$Biomass <- as.numeric(as.character(df$Biomass))
df <- df[is.finite(df$Dpm) & is.finite(df$Biomass), ]


# Square root transform
df$sqrt_dpm <- sqrt(df$Dpm)

# --- PRT model (17.06 + 17.69 * sqrt(dpm))
dpm_vals <- seq(min(df$Dpm), max(df$Dpm), length.out = 100)
   #dpm_vals <- seq(dpm_range[1], dpm_range[2], length.out = 100)
sqrt_dpm_vals <- sqrt(dpm_vals)
prt_model <- data.frame(
  x = sqrt_dpm_vals,
  y = 107.06 + 171.69 * sqrt_dpm_vals,
  year = "SHR")



# --- Tr model (-3019 + 2260 * sqrt(dpm))
tr_model <- data.frame(
  x = sqrt_dpm_vals,
  y = -3019 + 2260 * sqrt_dpm_vals,
  year = "Trollope")

# --- Main data for 2009 & 2013
main_data <- data.frame(
  x = df$sqrt_dpm,
  y = df$Biomass,
  year = df$Year)

# --- Combine everything
combined_df <- rbind(main_data, prt_model, tr_model)

# --- Plot all four models
ggplot(combined_df, aes(x = x, y = y, color = year)) +
  geom_point(data = main_data) +
  geom_smooth(data = main_data, method = "lm", se = FALSE) +
  geom_line(data = prt_model, linetype = "dashed", size = 1) +
  geom_line(data = tr_model, linetype = "dotted", size = 1.1) +
  labs(
    title = "Biomass vs sqrt(DPM) - Four Models",
    x = "sqrt(DPM)",
    y = "Biomass",
    color = "Model"
  ) +
  theme_minimal() ## NB THIS ONE DIDNT GIVE THE DESIRED RESULTS FURTHER ANALYSIS E.G LOG TRANSFORMING TO BE DONE

################################################################# LOG TRANSFORMING
# Clean the dataset
df$Year <- as.factor(df$Year)
df$Dpm <- as.numeric(as.character(df$Dpm))
df$Biomass <- as.numeric(as.character(df$Biomass))
df <- df[is.finite(df$Dpm) & is.finite(df$Biomass), ]

# Create transformed variables
df$sqrt_dpm <- sqrt(df$Dpm)
df$log_Biomass <- log(df$Biomass)

# --- Model 1 & 2: 2009 and 2013 (fitted from data)
main_data <- data.frame(
  x = df$sqrt_dpm,
  y = df$log_Biomass,
  year = df$Year)

# --- Model 3: PRT model (log-transformed)
# log transformation of biomass
dpm_vals <- seq(min(df$Dpm), max(df$Dpm), length.out = 100)
sqrt_dpm_vals <- sqrt(dpm_vals)
prt_biomass <- 107.06 + 171.69 * dpm_vals
prt_model <- data.frame(
  x = sqrt_dpm_vals,
  y = log(prt_biomass),
  year = "SHR")

# --- Model 4: Tr model (log-transformed)
tr_biomass <- -3019 + 2260 * sqrt_dpm_vals
tr_model <- data.frame(
  x = sqrt_dpm_vals,
  y = log(tr_biomass),
  year = "Trollope")

# Filter out any invalid log values (e.g., negative biomass)
  prt_model <- prt_model[is.finite(prt_model$y), ]
  tr_model <- tr_model[is.finite(tr_model$y), ]

# --- Combine all
combined_df <- rbind(main_data, prt_model, tr_model)

# --- Plot all 4 models
ggplot(combined_df, aes(x = x, y = y, color = year)) +
  geom_point(data = main_data, alpha = 0.5) +
  geom_smooth(data = main_data, method = "lm", se = FALSE) +
  geom_line(data = prt_model, linetype = "dashed", size = 1) +
  geom_line(data = tr_model, linetype = "dotted", size = 1.1) +
  labs(
    title = "Log(Biomass) vs sqrt(DPM) for All Models",
    x = "sqrt(DPM)",
    y = "log(Biomass)",
    color = "Model"
  ) +
  theme_minimal()


################################################################
###################   NEW WAY OF USING SQUARE ROOT IN ALL THE MODELS

df3 <- read_csv("DATA/March2025/SHR_modelYear.csv")
# Prepare data
df3 <- df3 %>% 
  mutate(
    sqrt_dpm = sqrt(Dpm),
    year = as.factor(Year))

# Fit models for 2009 and 2013
model_2024 <- lm(Biomass ~ sqrt_dpm, data = df3 %>% filter(year == "2024"))
tab_model(model_2024)
model_2025 <- lm(Biomass ~ sqrt_dpm, data = df3 %>% filter(year == "2025"))
tab_model(model_2025)

# Generate prediction range
dpm_range <- seq(min(df3$Dpm, na.rm = TRUE), max(df3$Dpm, na.rm = TRUE), length.out = 100)
sqrt_dpm_range <- sqrt(dpm_range)

# Create prediction data frames
pred_2024 <- data.frame(sqrt_dpm = sqrt_dpm_range)
pred_2024$Biomass <- predict(model_2024, newdata = pred_2024)
pred_2024$model <- "2024"


pred_2025 <- data.frame(sqrt_dpm = sqrt_dpm_range)
pred_2025$biomass <- predict(model_2025, newdata = pred_2025)
pred_2025$model <- "2025"


# SHR model (uses raw dpm)
SHR_model <- data.frame(
  sqrt_dpm = sqrt_dpm_range,
  biomass = -2990 + 1574.02 * sqrt_dpm_range,
  model = "SHR")

# Tr model
tr_model <- data.frame(
  sqrt_dpm = sqrt_dpm_range,
  biomass = -3019 + 2260 * sqrt_dpm_range,
  model = "Trollope and Potgieter 1986")

# Combine all models
all_models <- bind_rows(pred_2024, pred_2025, SHR_model, tr_model)

# Plot
#DFf3 <- 
  ggplot() +
  geom_point(data = df, aes(x = sqrt_dpm, y = Biomass, color = Year), alpha = 0.5) +
  geom_line(data = all_models, aes(x = sqrt_dpm, y = biomass, color = model), size = 1.2) +
  labs(
    x = "Square root DPM (cm)",
    y = "Standing Biomass (kg/ha)",
    color = "Model"
  ) +
  theme_beautiful() +
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 0.8),
    legend.box.just = "left")

#ggsave(DFf3,filename ="Plots/SQRT DPM heights 4 models.png",
       #width = 16, height = 14, units = "cm" )
  
 #####################################################################
  
  #### trying another way to establish a plot using already existing models
  
  # DPM height from 1 to 90 cm
  dpm <- seq(1, 90, by = 1)
  sqrt_dpm <- sqrt(dpm)
  
  # Compute biomass for each model
  biomass_a <- -730.42 + 874.59 * sqrt_dpm
  biomass_b <- -1013.69 + 971.25 * sqrt_dpm
  biomass_c <- -2990.95 + 1574.02 * sqrt_dpm
  biomass_d <- -3019 + 2260 * sqrt_dpm
  
  # Combine into a tidy data frame
  tf <- data.frame(
    dpm = rep(dpm, 4),
    biomass = c(biomass_a, biomass_b, biomass_c, biomass_d),
    model = factor(rep(c("2024", "2025", "SHR", "Trollope and Potgieter,1986"), each = length(dpm)))
  )
  
  # Plot
   #ff3 <- 
    ggplot(tf, aes(x = dpm, y = biomass, color = model)) +
    geom_line(size = 1.2) +
    labs(
      x = "DPM Height (cm)",
      y = "Standing Biomass (kg)"
    ) +
    theme_beautiful() +
    scale_color_manual(values = c("2024" = "blue", "2025" = "green", 
                                  "SHR" = "orange", "Trollope and Potgieter,1986" = "red"))+
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 0.8),
    legend.box.just = "left")
 
  #ggsave(ff3,filename ="Plots/4 MODELS ONLY.png",
   #width = 16, height = 14, units = "cm" )
    
###########################################################

    
    library(tidyverse)
    library(lmerTest)
    library(ggplot2)
    library(dplyr)
    library(Matrix)
    library(lme4)
    library(emmeans)
    library(here) ## MOST IMPORTANT NOT TO FORGET THIS ONE
    library(robustbase)
    library(sjPlot)
    
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
    
    
    df3 <- read_csv(here("DATA/March2025/SHR_modelYear.csv"))
    
    #Prepare data 
    df3 <- df3 %>% 
      mutate(
        sqrt_dpm = sqrt(Dpm),
        year = as.factor(Year))
    # Fit models for 2024 and 2025
    model_2024 <- lm(Biomass ~ sqrt_dpm, data = df3 %>% filter(year == "2024"))
    #tab_model(model_2024)
    model_2025 <- lm(Biomass ~ sqrt_dpm, data = df3 %>% filter(year == "2025"))
    #tab_model(model_2025)
    
    # Generate prediction range
    dpm_range <- seq(min(df3$Dpm, na.rm = TRUE), max(df3$Dpm, na.rm = TRUE), length.out = 100)
    sqrt_dpm_range <- sqrt(dpm_range)  
    
    # Create prediction data frames
    pred_2024 <- data.frame(sqrt_dpm = sqrt_dpm_range)
    pred_2024$Biomass <- predict(model_2024, newdata = pred_2024)
    pred_2024$model <- "2024"
    
    
    pred_2025 <- data.frame(sqrt_dpm = sqrt_dpm_range)
    pred_2025$biomass <- predict(model_2025, newdata = pred_2025)
    pred_2025$model <- "2025"
    
    
    # SHR model (uses raw dpm)
    SHR_model <- data.frame(
      sqrt_dpm = sqrt_dpm_range,
      biomass = -2990 + 1574.02 * sqrt_dpm_range,
      model = "SHR")
    
    # Tr model
    tr_model <- data.frame(
      sqrt_dpm = sqrt_dpm_range,
      biomass = -3019 + 2260 * sqrt_dpm_range,
      model = "Trollope and Potgieter 1986")
    # Combine all models
    all_models <- bind_rows(pred_2024, pred_2025, SHR_model, tr_model)
    
    # Plot
    ggplot() + geom_point(data = df3, aes(x = sqrt_Dpm, y = Biomass, color = Year), alpha = 0.5) +
      geom_line(data = all_models, aes(x = sqrt_dpm, y = biomass, color = model), size = 1.2) +
      labs(
        x = "Square root DPM (cm)",
        y = "Standing Biomass (kg/ha)",
        color = "Model"
      ) +
      theme_beautiful()
    
    )
###################################################################################


## USING POWER FUNCTION TO THE FOUR MODELS
library(janitor)

# --- Step 1: Read and clean data
#df <- read_csv("DATA/March2025/SHR_modelYear.csv")
df <- read_csv("DATA/March2025/SHR_modelYear.csv", na = c("", "NA"))
df <- clean_names(df)

raw <- read.csv("DATA/March2025/SHR_modelYear.csv", header = FALSE, stringsAsFactors = FALSE)

vNms <- as.character(unlist(raw[1, ]))
vNms[is.na(vNms) | vNms == ""] <- paste0("V", seq_along(vNms))[is.na(vNms) | vNms == ""]
vNms <- make.names(vNms, unique = TRUE)

df <- raw[-1, ]
names(df) <- vNms



df$year <- as.factor(df$year)
df$dpm <- as.numeric(as.character(df$dpm))
df$biomass <- as.numeric(as.character(df$biomass))
df <- df[is.finite(df$dpm) & is.finite(df$biomass), ]
df$sqrt_dpm <- sqrt(df$dpm)



# Sequence of Dpm values for prediction
dpm_vals <- seq(0, max(df$dpm), length.out = 100)
sqrt_dpm_vals <- sqrt(dpm_vals)

# --- Step 2: Hardcoded Models (REW and Tro)
rew_model <- data.frame(
  x = sqrt_dpm_vals,
  y = -2990.95+1574.02* sqrt_dpm_vals,
  year = "SHR")

tro_model <- data.frame(
  x = sqrt_dpm_vals,
  y = -3019+2260* sqrt_dpm_vals,
  year = "Trollope")

# --- Step 3: Fit two models from data
# Example: subset data by Year, Treatment, etc., if needed
# Hypothetical split for models 3 and 4 (adjust as needed)
df_hyp1 <- subset(df, Year == "2023")
df_hyp2 <- subset(df, Year == "2024")


# Model 3: Fit power model with no intercept
fit_hyp1 <- nls(biomass ~ a * sqrt_dpm, data = df_hyp1, start = list(a = 1))
coef_hyp1 <- coef(fit_hyp1)["a"]
hyp1_model <- data.frame(
  x = sqrt_dpm_vals,
  y = coef_hyp1 * sqrt_dpm_vals,
  year = "Hyp1")

# Model 4: Fit another model similarly
fit_hyp2 <- nls(Biomass ~ a * sqrt_dpm, data = df_hyp2, start = list(a = 1))
coef_hyp2 <- coef(fit_hyp2)["a"]
hyp2_model <- data.frame(
  x = sqrt_dpm_vals,
  y = coef_hyp2 * sqrt_dpm_vals,
  year = "Hyp2"
)

# --- Step 4: Combine and Plot
model_df <- rbind(rew_model, tro_model, hyp1_model, hyp2_model)

ggplot(df, aes(x = sqrt_dpm, y = Biomass)) +
  geom_point(alpha = 0.4) +
  geom_line(data = model_df, aes(x = x, y = y, color = year), size = 1.2) +
  labs(
    title = "Power Model Fits (sqrt(DPM) vs Biomass)",
    x = "sqrt(DPM)",
    y = "Biomass",
    color = "Model"
  ) +
  theme_minimal()



    