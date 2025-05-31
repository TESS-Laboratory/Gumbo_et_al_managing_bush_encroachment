library(tidyverse)
library(lmerTest)
library(ggplot2)
library(dplyr)
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
        linetype = "blank"
      )
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
summary(modelWG)
 tab_model(modelWG)

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


################## TRYING THE OTHER EQUATIONS BY ZAMBATIS
# Step 1: Define the model function
 #Revised biomass_model() Function with Debug:

biomass_model <- function(x) {
  biomass <- numeric(length(x))
  
  for (i in seq_along(x)) {
    if (x[i] <= 26) {
      part1 <- 31.7176
      part2 <- 0.3218^(1 / x[i])
      part3 <- 0.2834
      result <- (part1 * part2 * part3)^2
    } else {
      part1 <- 17.3543
      part2 <- 0.9893^x[i]
      part3 <- 0.5413
      result <- (part1 * part2 * part3)^2
    }
    biomass[i] <- result
  }
  
  return(biomass)
}


# Step 3: Apply model to DPH values in WGdata (for plotting only)
TR_curve_data <- data.frame(
  DPH_Height = WGdata$DPH_Height,
  Biomass_kg_ha = biomass_model(WGdata$DPH_Height),
  Source = "TR Model")

# Step 4: Actual observed WG data
WG_actual_data <- data.frame(
  DPH_Height = WGdata$DPH_Height,
  Biomass_kg_ha = WGdata$Biomass_kg_ha,
  Source = "SHR")

# Step 5: Combine for plotting
combined <- rbind(WG_actual_data, TR_curve_data)

# Step 6: Plot
ggplot(combined, aes(x = DPH_Height, y = Biomass_kg_ha, color = Source)) +
  geom_point(data = WG_actual_data, size = 2) +
  geom_smooth(data = WG_actual_data, method = "lm", se = FALSE) +
  geom_line(data = TR_curve_data, linetype = "dashed", size = 1.1) +
  labs(
    x = "DPH (cm)",
    y = "Biomass",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.box.just = "left")

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
  #theme_beautiful()

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

# Ensure 'year' is treated as a factor
df$Year <- as.factor(df$Year)

# Convert to numeric (in case it's a factor or character)
df$Dpm <- as.numeric(as.character(df$Dpm))

# Remove rows with missing or non-finite values
df <- df[is.finite(df$Dpm), ]

##
range(df$Dpm, na.rm = TRUE)


# Create a new data frame for the custom model line
# We'll use the same range of dpm as in your dataset
dpm_range <- range(df$Dpm)
custom_line <- data.frame(
  dpm = seq(dpm_range[1], dpm_range[2], length.out = 100)
)
custom_line$biomass <- 107.06 + 171.69 * custom_line$dpm
custom_line$label <- "SHR"  # Label for legend


# establish a plot

ggplot(df, aes(x = Dpm, y = Biomass, color = Year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Fitted lines for each year
  geom_line(data = custom_line, aes(x = dpm, y = biomass),
            color = "black", linetype = "solid", size = 1) +  # Custom model
  labs(
    x = "DPM Height (cm)",
    y = "Biomass (kg/ha)",
    color = "Year"
  ) +
  theme_beautiful() + 
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.box.just = "left")
