library(tidyverse)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(Matrix)
library(lme4)
library(emmeans)

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



#####COMPARING TWO DIFFERENT EQUATIONS USED FOR BIOMASS ESTIMATION

# Load the dataset

TGdata <- read_csv("DATA/March2025/dpmHEIGHT.csv")


# Apply square root transformation and compute biomass estimates
TGdata <- TGdata %>%
  mutate(
    DH_sqrt = sqrt(DH),
    Eq1 = -3019 + 2260 * DH_sqrt,
    Eq2 = -2928.7 + 1571.3 * DH_sqrt
  )

# View the result
head(data)

# Create the plot with two different fit lines
ggplot(TGdata, aes(x = DH_sqrt)) +
  geom_point(aes(y = biomass_eq1), color = "blue", alpha = 0.5) +
  geom_point(aes(y = biomass_eq2), color = "red", alpha = 0.5) +
  geom_smooth(aes(y = biomass_eq1), method = "lm", color = "blue", se = FALSE) +
  geom_smooth(aes(y = biomass_eq2), method = "lm", color = "red", se = FALSE) +
  labs(
    x = "DPM height (cm)",
    y = "Biomass (kg/ha)",
    color = "Equation"
  ) +
  theme_beautiful()


# Reshape data for ggplot (long format)
data_long <- TGdata %>%
  pivot_longer(cols = c(biomass_eq1, biomass_eq2),
               names_to = "Equation",
               values_to = "Biomass")

# Create plot with linear regression lines
ggplot(data_long, aes(x = DH_sqrt, y = Biomass, color = Equation)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  labs(
    title = "Linear Regression of Biomass Estimates",
    x = "Square Root of DPH",
    y = "Estimated Biomass",
    color = "Equation"
  ) +
  theme_beautiful()


# Plot with solid regression lines and points
ggplot(data_long, aes(x = DH_sqrt, y = Biomass, color = Equation)) +
  geom_point(alpha = 0.5, size = 2) +  # show data points
  geom_smooth(method = "lm", se = FALSE, size = 1.2, linetype = "solid") +  # solid regression lines
  labs(
    x = "Square Root of DPH",
    y = "Estimated Biomass",
    color = "Equation"
  ) +
  theme_minimal()



###################################### ANOTHER ATTEMPT TO PRODUCE A PLOT 
# Reshape to long format for plotting

# Reshape to long format for plotting
data_long <- TGdata %>%
  pivot_longer(cols = c(biomass_eq1, biomass_eq2),
               names_to = "Model",
               values_to = "Biomass")

# Create the plot with different line types and colors
TG<-ggplot(data_long, aes(x = DH_sqrt, y = Biomass, color = Model)) +
  geom_point(aes(shape = Model), size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = Model), size = 1.2) +
  scale_color_manual(values = c("biomass_eq1" = "#440154FF", "biomass_eq2" = "#21908CFF")) +
  scale_linetype_manual(values = c("biomass_eq1" = "solid", "biomass_eq2" = "dashed")) +
  scale_shape_manual(values = c("biomass_eq1" = 16, "biomass_eq2" = 17)) +
  labs(
    x = "DPM height (cm)",
    y = "Biomass (kg/ha)",
    color = "Model",
    linetype = "Model",
    shape = "Model"
  ) +
  theme_beautiful() +
  theme(legend.position = "right")

# Saving as png
ggsave(TG,filename ="C:/workspace/gumbo_dev/Plots/EBiomass.png",
       width = 16, height = 14, units = "cm" )

#####################################################################
############## LOG TRANSFORMED 

LGdata <- read_csv("C:/workspace/gumbo_dev/DATA/March2025/dpmHEIGHT.csv")
# Add biomass estimates and log-transformed values
LGdata <- LGdata %>%
  mutate(
    DH_sqrt = sqrt(DH),
    biomass_eq1 = -3019 + 2260 * DH_sqrt,
    biomass_eq2 = -2928.7 + 1571.3 * DH_sqrt
  ) %>%
  filter(biomass_eq1 > 0, biomass_eq2 > 0, DH > 0) %>%  # remove negative/zero values before log
  mutate(
    log_DH = log(DH),
    log_biomass_eq1 = log(biomass_eq1),
    log_biomass_eq2 = log(biomass_eq2)
  )

# Reshape data for plotting
data_long <- LGdata %>%
  pivot_longer(cols = c(log_biomass_eq1, log_biomass_eq2),
               names_to = "Model",
               values_to = "LogBiomass")

# Plot log-log relationship
ggplot(data_long, aes(x = log_DH, y = LogBiomass, color = Model)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  labs(
    title = "Log-Transformed Biomass vs. DPH",
    x = "log(DPH)",
    y = "log(Biomass)",
    color = "Model"
  ) +
  theme_minimal()

###############################################################################
## REDOING BIOMASS MODELS USING ACTUAL DATA POINTS

WGdata <- read_csv("DATA/DPM .csv")

################## DPM HEIGHT ~ OVEN DRIED WEIGHT. NO SQUARE ROOT
# 2. Convert weight from grams to kg/ha
#    Area of 34cm diameter disc = π * (0.17 m)^2 = 0.0908 m²
frame_area <- pi * (0.17^2)  # = 0.0908 m²
WGdata$Biomass_kg_ha <- WGdata$Weight * 10 / frame_area

# 3. Linear regression: Biomass ~ DPH
modelWG <- lm(Biomass_kg_ha ~ DPH_Height, data = WGdata)
summary(modelWG)

# Create scatter plot with regression line
WG <-ggplot(WGdata, aes(x = DPH_Height, y = Biomass_kg_ha)) +
  geom_point() +  # scatter plot
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # regression line
  labs(
       x = "DPM Height (cm)",
       y = "Standing Biomass (kg/ha)") +
  theme_beautiful()


# Saving as png
ggsave(WG,filename ="Plots/Correct Biomass.png",
       width = 16, height = 14, units = "cm" )



###ADDING TROLLOPE DATA SET AND COMBINING THE PLOTS
# Load necessary library
library(ggplot2)

# Step 1: Create the second dataset (TRdata)
TRdata <- data.frame(
  DPH_Height = c(2, 3, 4, 5, 6, 8, 10, 15, 20, 25, 30),
  Biomass_kg_ha = c(177, 895, 1501, 2035, 2517, 3373, 4128, 5734, 7088, 8281, 9360),
  Source = "Trollope 1986"
)

# Step 2: Add a column to WGdata
WGdata$Source <- "SHR"

# Step 3: Standardize and combine
combined_data <- rbind(
  WGdata[, c("DPH_Height", "Biomass_kg_ha", "Source")],
  TRdata
)

# Step 4: Plot both with regression lines
TWG <- ggplot(combined_data, aes(x = DPH_Height, y = Biomass_kg_ha, color = Source)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
       x = "DPM height (cm)",
       y = "Standing Biomass (kg/ha)",
       color = "Model") +
  theme_beautiful()+ theme(legend.position = "right")

ggsave(TWG,filename ="Plots/TRSHR Biomass.png",
       width = 16, height = 14, units = "cm" )


################## NOW I AM USING SQUARE ROOTS FOR DPM HEIGHT#################


# --- Step 1: Create TR dataset ---
TRdata <- data.frame(
  DPH_Height = c(2, 3, 4, 5, 6, 8, 10, 15, 20, 25, 30),
  Biomass_kg_ha = c(177, 895, 1501, 2035, 2517, 3373, 4128, 5734, 7088, 8281, 9360),
  Source = "Trollope 1986"
)

# --- Step 2: Prepare WGdata ---
# Ensure WGdata has matching column names
colnames(WGdata)[1:2] <- c("DPH_height", "Biomass_kg_ha")  # if not already set
WGdata$Source <- "SHR"

# --- Step 3: Combine datasets ---

combined_data <- rbind(
  WGdata[, c("DPH_Height", "Biomass_kg_ha", "Source")],
  TRdata
)

# --- Step 4: Add square root of DP ---
combined_data$sqrtDPH_Height <- sqrt(combined_data$DPH_Height)

# --- Step 5: Plot with square root transformation ---
SQRT <- ggplot(combined_data, aes(x = sqrtDPH_Height, y = Biomass_kg_ha, color = Source)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Square root DPM height (cm)",
    y = "Standing Biomass (kg/ha)",
    color = "Model"
  ) +
  theme_minimal()

ggsave(SQRT,filename ="Plots/SQT DPM Biomass.png",
       width = 16, height = 14, units = "cm" )

############ USING TROLLOPE EQUATION TO DEVELOP A MODEL AND COMPARE IT WITH SHR

# Load ggplot2
library(ggplot2)

# Step 1: Prepare WGdata

WGdata$sqrtDPH_Height <- sqrt(WGdata$DPH_Height)

# Step 2: Create theoretical model predictions using the equation
WGdata$TR_model <- -3019 + 2260 * WGdata$sqrtDPH_Height

# Step 3: Plot actual data with regression line + theoretical line
ggplot(WGdata, aes(x = sqrtDPH_Height)) +
  geom_point(aes(y = Biomass_kg_ha), color = "blue", size = 2) +  # scatter points
  geom_smooth(aes(y = Biomass_kg_ha), method = "lm", se = FALSE, color = "blue") +  # WG regression
  geom_line(aes(y = TR_model), color = "red", linetype = "dashed", size = 1) +  # TR model line
  labs(
    x = "Square root DPM height (cm)",
    y = "Standing Biomass (kg/ha)",
    caption = "Blue = SHR | Red dashed = Trollope1986"
  ) +
  theme_minimal()

######### IMPROVING THE  SCRRIPT SO THAT THE LEGEND COMES OUT CLEARLY FOR DATA VISUALISATION
# Step 2: Theoretical model: y = -3019 + 2260 * sqrt(DP)
TR_model_data <- data.frame(
  sqrtDPH_Height = WGdata$sqrtDPH_Height,
  Biomass_kg_ha = -3019 + 2260 * WGdata$sqrtDPH_Height,
  Source = "Trollope1986"
)

# Step 3: Add source to WGdata
WG_actual_data <- data.frame(
  sqrtDPH_Height = WGdata$sqrtDPH_Height,
  Biomass_kg_ha = WGdata$Biomass_kg_ha,
  Source = "SHR"
)

# Step 4: Combine both for plotting
combined <- rbind(WG_actual_data, TR_model_data)

# Step 5: Plot with legend
SD <- ggplot(combined, aes(x = sqrtDPH_Height, y = Biomass_kg_ha, color = Source)) +
  geom_point(data = subset(combined, Source == "SHR"), size = 2) +
  geom_smooth(data = subset(combined, Source == "SHR"),
              method = "lm", se = FALSE) +
  geom_line(data = subset(combined, Source == "Trollope1986"),
            linetype = "dashed", size = 1) +
  labs(
    x = "Square root DPM heigh (cm)",
    y = "Standing Biomass (kg/ha)",
    color = "Model"
  ) +
  theme_beautiful() + theme(legend.position = "right")


ggsave(SD,filename ="Plots/PROPER SQRT DPM Biomass.png",
       width = 16, height = 14, units = "cm" )
