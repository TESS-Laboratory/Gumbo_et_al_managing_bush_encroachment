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
##library(ggpmisc)

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

WGdata <- read_csv("DATA/DPM.csv")



# 1. Calculate frame area (m²)
frame_area <- pi * (0.17^2)  # = 0.0908 m²

# 2. Convert grams to kg/ha
WGdata$Biomass_kg_ha <- WGdata$Weight * 10 / frame_area

# 3. Square root transformation of DPH
WGdata$sqrtDPH_Height <- sqrt(WGdata$DPH_Height)

# 4. Fit power model through the origin: Biomass ~ sqrt(DPH)
power_model <- nls(Biomass_kg_ha ~ a * sqrtDPH_Height,
                   data = WGdata,
                   start = list(a = 1))

# 5. Get coefficient
a_coef <- coef(power_model)["a"]

# 6. Predict values for plotting
x_vals <- seq(0, max(WGdata$DPH_Height, na.rm = TRUE), length.out = 100)
sqrt_x <- sqrt(x_vals)

# --- Your fitted power model
model1_df <- data.frame(
  DPH_Height = x_vals,
  Biomass = a_coef * sqrt_x,
  Model = "SHR")

# --- Trollope model
model2_df <- data.frame(
  DPH_Height = x_vals,
  Biomass = -3019 + 2260 * sqrt_x,
  Model = "Trollope Model")

# Combine both models
combined_models <- rbind(model1_df, model2_df)

# 7. Plot
ggplot(WGdata, aes(x = DPH_Height, y = Biomass_kg_ha)) +
  geom_point(alpha = 0.4) +
  geom_line(data = combined_models, aes(x = DPH_Height, y = Biomass, color = Model), size = 1.2) +
  scale_color_manual(
    values = c("SHR" = "red", "Trollope Model" = "yellow")
  ) +
  labs(
    title = "Biomass vs DPH: Your Model vs Trollope",
    x = "DPH Height (cm)",
    y = "Biomass (kg/ha)",
    color = "Model"
  ) +
  theme_minimal()


################### REVISION OF THE MODELS TO CONSTRAIN THE MODELS TO ZERO

# Fit your own power model through origin
SHR_model <- nls(Biomass_kg_ha ~ a * sqrtDPH_Height,
                  data = WGdata,
                  start = list(a = 1))
a1 <- coef(SHR_model)["a"]



# Fit "Trollope-like" model through origin using same data
trollope_model <- nls(Biomass_kg_ha ~ a * sqrtDPH_Height,
                      data = WGdata,
                      start = list(a = 2000))  # Trollope's slope is about 2260
a2 <- coef(trollope_model)["a"]

# Predict over common range
x_vals <- seq(0, max(WGdata$DPH_Height), length.out = 100)
sqrt_x <- sqrt(x_vals)

model1_df <- data.frame(DPH_Height = x_vals,
                        Biomass = a1 * sqrt_x,
                        Model = "SHR")


model2_df <- data.frame(DPH_Height = x_vals,
                        Biomass = a2 * sqrt_x,
                        Model = "Trollope")

plot_df <- rbind(model1_df, model2_df)

###Final Plot with Color:

ggplot(WGdata, aes(x = DPH_Height, y = Biomass_kg_ha)) +
  geom_point(alpha = 0.5) +
  geom_line(data = plot_df, aes(x = DPH_Height, y = Biomass, color = Model), size = 1) +
  scale_color_manual(values = c("SHR" = "blue", "Trollope" = "green")) +
  labs(x = "DPH Height (cm)", y = "Biomass (kg/ha)", color = "Model") +
  theme_minimal()




#######attempt 2 to use power function on sqrt dpm

library(ggplot2)

# 1. Frame area and conversion
frame_area <- pi * (0.17^2)
WGdata$Biomass_kg_ha <- WGdata$Weight * 10 / frame_area
WGdata$sqrtDPH_Height <- sqrt(WGdata$DPH_Height)

# 2. Fit model 1 through origin
model1 <- nls(Biomass_kg_ha ~ a * sqrtDPH_Height,
              data = WGdata,
              start = list(a = 1))
a1 <- coef(model1)

# 3. Generate prediction line
x_vals <- seq(0, max(WGdata$DPH_Height, na.rm = TRUE), length.out = 100)
sqrt_x <- sqrt(x_vals)
model1_df <- data.frame(
  DPH_Height = x_vals,
  Biomass = a1 * sqrt_x,
  Model = "Model 1"
)

# 4. Optional: Add a second model (e.g., Trollope)
model2_df <- data.frame(
  DPH_Height = x_vals,
  Biomass = -3019 + 2260 * sqrt_x,
  Model = "Trollope"
)

model2 <- nls(Biomass_kg_ha ~ a * sqrtDPH_Height,
                      data = WGdata,
                      start = list(a = 2000))  # Trollope's slope is about 2260
a2 <- coef(model2)["a"]


# 5. Combine model lines
model_lines <- rbind(model1_df, model2_df)
model_lines$Model <- factor(model_lines$Model)  # Force clean factor

# 6. Final plot
ggplot(WGdata, aes(x = DPH_Height, y = Biomass_kg_ha)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_line(data = model_lines, aes(x = DPH_Height, y = Biomass, color = Model), size = 1.2) +
  scale_color_manual(values = c("Model 1" = "blue", "Trollope" = "red")) +
  labs(x = "DPH Height (cm)", y = "Biomass (kg/ha)", color = "Model") +
  theme_minimal()






######################## this code gave better results further analysis being done to sqrt DPH for model 1
WGdata <- WGdata[WGdata$DPH_Height > 1 & WGdata$Biomass_kg_ha > 0, ]

#POWER MODEL
power_mod <- nls(Biomass_kg_ha ~ a * DPH_Height^b,
                 data = WGdata,
                 start = list(a = 200, b = 1),
                 control = nls.control(maxiter = 100, warnOnly = TRUE))

#Fit the Trollope-style constrained model
trollope_mod <- nls(Biomass_kg_ha ~ a * sqrt(DPH_Height),
                    data = WGdata,
                    start = list(a = 100),
                    control = nls.control(maxiter = 100, warnOnly = TRUE))

##Plotting Both Models
# Create prediction data
x_vals <- seq(min(WGdata$DPH_Height), max(WGdata$DPH_Height), length.out = 100)
pred_df <- data.frame(DPH_Height = x_vals)

pred_df$Power <- predict(power_mod, newdata = pred_df)
pred_df$Trollope <- predict(trollope_mod, newdata = pred_df)

# Plot

ggplot(WGdata, aes(x = DPH_Height, y = Biomass_kg_ha)) +
  geom_point() +
  geom_line(data = pred_df, aes(x = DPH_Height, y = Power, color = "Power Model"), size = 1) +
  geom_line(data = pred_df, aes(x = DPH_Height, y = Trollope, color = "Trollope Model"), linetype = "dashed", size = 1) +
  labs(x = "DPH Height (cm)", y = "Biomass (kg/ha)", color = "Model") +
  scale_color_manual(values = c("Power Model" = "green", "Trollope Model" = "blue")) +
  theme_minimal()

##################################################
#Clean your data first

WGdata <- WGdata[is.finite(WGdata$DPH_Height) & is.finite(WGdata$Biomass_kg_ha), ]
WGdata <- WGdata[WGdata$DPH_Height > 0 & WGdata$Biomass_kg_ha > 0, ]

# Create sqrt(DPH) variable
WGdata$sqrtDPH <- sqrt(WGdata$DPH_Height)

#2. Fit both models constrained through the origin
#Power Model: We'll constrain it as:
#Biomass = a * sqrt(DPH) ^ b

power_mod <- nls(Biomass_kg_ha ~ a * sqrtDPH^b,
                 data = WGdata,
                 start = list(a = 100, b = 1),
                 control = nls.control(maxiter = 200, warnOnly = TRUE))

#Trollope-style model (linear in sqrtDPH, no intercept):
trollope_mod <- nls(Biomass_kg_ha ~ a * sqrtDPH,
                    data = WGdata,
                    start = list(a = 100),
                    control = nls.control(maxiter = 200, warnOnly = TRUE))

##3.Plot both models together

# Create prediction data
x_vals <- seq(min(WGdata$DPH_Height), max(WGdata$DPH_Height), length.out = 100)
pred_df <- data.frame(DPH_Height = x_vals,
                      sqrtDPH = sqrt(x_vals))

# Add predictions
pred_df$Power <- predict(power_mod, newdata = pred_df)
pred_df$Trollope <- predict(trollope_mod, newdata = pred_df)

# Plot
ggplot(WGdata, aes(x = DPH_Height, y = Biomass_kg_ha)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +
  geom_line(data = pred_df, aes(x = DPH_Height, y = Power, color = "Power Model"), size = 1.2) +
  geom_line(data = pred_df, aes(x = DPH_Height, y = Trollope, color = "Trollope Model"), linetype = "dashed", size = 1.2) +
  labs(
    title = "Biomass vs DPH Height using Power & Trollope Models",
    x = "DPH Height (cm)",
    y = "Biomass (kg/ha)",
    color = "Model"
  ) +
  scale_color_manual(values = c("Power Model" = "green", "Trollope Model" = "blue")) +
  theme_minimal()



###################################################################################################################
##### ANOTHER ATTEMPT TO USE POWER FUNCTION 

#Fit the models (as before)
WGdata$sqrtDPH <- sqrt(WGdata$DPH_Height)


# Power model: y = a * sqrtDPH^b
power_mod <- nls(Biomass_kg_ha ~ a * sqrtDPH^b,
                 data = WGdata,
                 start = list(a = 100, b = 1),
                 control = nls.control(maxiter = 200, warnOnly = TRUE))

# Trollope model: y = a * sqrtDPH (constrained to pass through 0)
trollope_mod <- nls(Biomass_kg_ha ~ a * sqrtDPH,
                    data = WGdata,
                    start = list(a = 100),
                    control = nls.control(maxiter = 200, warnOnly = TRUE))

## Extract model coefficients for labels
# Get parameters
coef_power <- coef(power_mod)
coef_trollope <- coef(trollope_mod)

# Create equation labels
eq_power <- sprintf("Power: y == %.2f %.2f^sqrt(x)", coef_power["a"], coef_power["b"])
eq_power_pretty <- sprintf("y == %.2f * sqrt(x)^%.2f", coef_power["a"], coef_power["b"])

eq_trollope <- sprintf("Trollope: y == %.2f * sqrt(x)", coef_trollope["a"])
eq_trollope_pretty <- sprintf("y == %.2f * sqrt(x)", coef_trollope["a"])


##Predict and Plot with Equations
# Prediction data
x_vals <- seq(min(WGdata$DPH_Height), max(WGdata$DPH_Height), length.out = 100)
pred_df <- data.frame(DPH_Height = x_vals, sqrtDPH = sqrt(x_vals))
pred_df$Power <- predict(power_mod, newdata = pred_df)
pred_df$Trollope <- predict(trollope_mod, newdata = pred_df)

# Plot
ggplot(WGdata, aes(x = DPH_Height, y = Biomass_kg_ha)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +
  geom_line(data = pred_df, aes(x = DPH_Height, y = Power, color = "Power Model"), size = 1.2) +
  geom_line(data = pred_df, aes(x = DPH_Height, y = Trollope, color = "Trollope Model"), linetype = "dashed", size = 1.2) +
  labs(
    title = "Biomass vs DPH Height (Constrained Models)",
    x = "DPH Height (cm)",
    y = "Biomass (kg/ha)",
    color = "Model"
  ) +
  scale_color_manual(values = c("Power Model" = "green", "Trollope Model" = "blue")) +
  annotate("text", x = max(WGdata$DPH_Height)*0.6, y = max(WGdata$Biomass_kg_ha)*0.95,
           label = eq_power_pretty, parse = TRUE, color = "green", size = 5, hjust = 0) +
  annotate("text", x = max(WGdata$DPH_Height)*0.6, y = max(WGdata$Biomass_kg_ha)*0.85,
           label = eq_trollope_pretty, parse = TRUE, color = "blue", size = 5, hjust = 0) +
  theme_minimal()




############# POWER FUNCTION FOR BOTH MODELS, BOTH WITH SQUARE ROOTS
# Clean data
WGdata <- WGdata[is.finite(WGdata$DPH_Height) & is.finite(WGdata$Biomass_kg_ha), ]
WGdata <- WGdata[WGdata$DPH_Height > 0 & WGdata$Biomass_kg_ha > 0, ]
WGdata$sqrtDPH <- sqrt(WGdata$DPH_Height)

################## DPM HEIGHT ~ OVEN DRIED WEIGHT. NO SQUARE ROOT
# 2. Convert weight from grams to kg/ha
#    Area of 34cm diameter disc = π * (0.17 m)^2 = 0.0908 m²
frame_area <- pi * (0.17^2)  # = 0.0908 m²
WGdata$Biomass_kg_ha <- WGdata$Weight * 10 / frame_area

# Fit power model (nonlinear, allows exponent)
SHR_mod <- nls(Biomass_kg_ha ~ a * sqrtDPH^b,
                 data = WGdata,
                 start = list(a = 100, b = 1),
                 control = nls.control(maxiter = 200, warnOnly = TRUE))

# Fit Trollope-constrained model: y = a * sqrt(x)
trollope_mod <- nls(Biomass_kg_ha ~ a * sqrtDPH,
                    data = WGdata,
                    start = list(a = 100),
                    control = nls.control(maxiter = 200, warnOnly = TRUE))


#Predictions
x_vals <- seq(min(WGdata$DPH_Height), max(WGdata$DPH_Height), length.out = 100)
pred_df <- data.frame(DPH_Height = x_vals, sqrtDPH = sqrt(x_vals))
pred_df$SHR <- predict(SHR_mod, newdata = pred_df)
pred_df$Trollope <- predict(trollope_mod, newdata = pred_df)

# Extract coefficients
coef_SHR <- coef(SHR_mod)
coef_trollope <- coef(trollope_mod)

# Build labels for the plot
eq_SHR <- sprintf("y == %.2f * sqrt(x)^%.2f", coef_SHR["a"], coef_SHR["b"])
eq_trollope <- sprintf("y == %.2f * sqrt(x)", coef_trollope["a"])

##PLOT

ggplot(WGdata, aes(x = DPH_Height, y = Biomass_kg_ha)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +
  geom_line(data = pred_df, aes(x = DPH_Height, y = SHR, color = "SHR"), size = 1.2) +
  geom_line(data = pred_df, aes(x = DPH_Height, y = Trollope, color = "Trollope"), linetype = "dashed", size = 1.2) +
  annotate("text", x = max(WGdata$DPH_Height)*0.1, y = max(WGdata$Biomass_kg_ha)*0.95,
           label = eq_SHR, parse = TRUE, color = "green", size = 5, hjust = 0) +
  annotate("text", x = max(WGdata$DPH_Height)*0.1, y = max(WGdata$Biomass_kg_ha)*0.85,
           label = eq_trollope, parse = TRUE, color = "blue", size = 5, hjust = 0) +
  labs(
    x = "DPH Height (cm)",
    y = "Biomass (kg/ha)",
    color = "Model"
  ) +
  scale_color_manual(values = c("SHR" = "green", "Trollope" = "blue")) +
  theme_beautiful()+ theme(
    legend.position = c(0.1, 1),
    legend.justification = c(0, 1),
    legend.box.just = "left",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = "gray90"))








####################################################################
########## POWER FUNCTION WITH NO SQUARE ROOT FOR MY MODEL

# Clean data
WGdata <- WGdata[is.finite(WGdata$DPH_Height) & is.finite(WGdata$Biomass_kg_ha), ]
WGdata <- WGdata[WGdata$DPH_Height > 0 & WGdata$Biomass_kg_ha > 0, ]
WGdata$sqrtDPH <- sqrt(WGdata$DPH_Height)

# Remove rows with NA or infinite values
WGdata <- WGdata[is.finite(WGdata$DPH_Height) & is.finite(WGdata$Biomass_kg_ha), ]

# Remove rows where DPH or biomass is zero or negative (important for power models)
WGdata <- WGdata[WGdata$DPH_Height > 0 & WGdata$Biomass_kg_ha > 0, ]

# Create square root column (no sqrt of 0 or negative because of filter above)
WGdata$sqrtDPH <- sqrt(WGdata$DPH_Height)

# 1. Your model (x = DPH)
model_yours <- nls(Biomass_kg_ha ~ a * DPH_Height^b,
                   data = WGdata,
                   start = list(a = 1, b = 1),
                   control = nls.control(maxiter = 200, warnOnly = TRUE))

# 2. Trollope model (x = sqrt(DPH))
model_trollope <- nls(Biomass_kg_ha ~ a * DPH^b,
                      data = WGdata,
                      start = list(a = 1, b = 1),
                      control = nls.control(maxiter = 200, warnOnly = TRUE))

##################################### EXPECTED GRASS BIOMASS FENCED

# Load required libraries
library(ggplot2)

# Simulate example data
set.seed(123)
treatments <- c("Control", "F", "TF", "TFB", "THF")
fence_status <- c("Fenced", "Unfenced")

# Create synthetic dataset
data <- expand.grid(Treatment = treatments,
                    Fence = fence_status,
                    Replicate = 1:15)

# Simulate values with an increasing trend across treatments
data$BiomassChange <- with(data, ifelse(
  Treatment == "Control", rnorm(nrow(data), mean = 200, sd = 50),
  ifelse(Treatment == "F", rnorm(nrow(data), mean = 400, sd = 60),
         ifelse(Treatment == "TF", rnorm(nrow(data), mean = 600, sd = 70),
                ifelse(Treatment == "TFB", rnorm(nrow(data), mean = 1000, sd = 80),
                       rnorm(nrow(data), mean = 1100, sd = 90))))) +
    ifelse(Fence == "Fenced", 100, 0)  # Add boost for fencing
)

# Plot
 BF <- ggplot(data, aes(x = Treatment, y = BiomassChange, fill = Fence)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Fenced" = "cornsilk3", "Unfenced" = "white")) +
  labs(
    y = "Change in grass biomass (kg/ha)",
    Fill = "null"
  ) +
  theme_beautiful() +
  theme(
    legend.position = "top",
    axis.title.x = element_blank()
  )

ggsave(BF,filename ="Plots/Change Grass BiomassFenced.png",
      width = 16, height = 14, units = "cm" )

 
 ############################################EXPECTED CHANGE IN GRASS BIOMASS
 
 # Define treatments
 treatments <- c("Control", "F", "TF", "TFB", "TFH")
 
 # Simulate example data
 set.seed(123)
 n <- 15  # number of replicates
 means <- c(1000, 1200, 1800, 3000, 3500)
 
 # Generate dataset
 data <- data.frame(
   Treatment = rep(treatments, each = n),
   BiomassChange = unlist(lapply(means, function(m) rnorm(n, mean = m, sd = 300)))
 )
 # Create the plot
  ggplot(data, aes(x = Treatment, y = BiomassChange)) +
   geom_boxplot(fill = "grey90", color = "black") +
   labs(
     y = "Change in standing grass biomass (kg/ha)",
     x = "Treatments"
   ) +
   theme_beautiful() +
   theme(
     axis.text.x = element_text(angle = 0, hjust = 0.5),
     panel.grid.major.x = element_blank()
   )
 
 #ggsave(CB,filename ="Plots/Change Grass Biomass.png",
  #           width = 16, height = 14, units = "cm" )
  
  ##################### GRAASS BIOMASS ENCROACHED 
 
  # Sample structure (replace this with your real data)
  set.seed(123)
  data <- data.frame(
    Treatment = rep(c("Control", "F", "TF", "TFB", "TFH"), each = 20),
    Encroachment = rep(rep(c("Highly encroached", "Moderately encroached"), each = 10), times = 5),
    BiomassChange = c(
      rnorm(10, 400, 80), rnorm(10, 450, 80),
      rnorm(10, 500, 90), rnorm(10, 700, 100),
      rnorm(10, 1000, 120), rnorm(10, 1200, 130),
      rnorm(10, 1300, 150), rnorm(10, 1700, 160),
      rnorm(10, 1600, 170), rnorm(10, 2000, 180)
    )
  )
  
  # Ensure correct factor order
  data$Treatment <- factor(data$Treatment, 
                           levels = c("Control", "F", "TF", "TFB", "TFH"))
  
  # Create the boxplot
 ggplot(data, aes(x = Treatment, y = BiomassChange, fill = Encroachment)) +
    geom_boxplot(position = position_dodge(width = 0.75), width = 0.6) +
    scale_fill_manual(values = c("Highly encroached" = "#00557F", "Moderately encroached" = "#FFC000")) +
    labs(y = "Change in grass biomass (kg/ha)", x = "Treatments", fill = NULL) +
    theme_beautiful() +theme(
      legend.position = c(0.1, 1),
      legend.justification = c(0, 1),
      legend.box.just = "left",
      legend.title = element_text(face = "bold"),
      legend.background = element_rect(fill = "white", color = "gray90"))
  
   
  #ggsave(HE, filename = "Plots/Encroched Grass Biomass change.png",
   #                 width = 16, height = 14, units = "cm")
 
 ####################################################SEEDLINGS AND SAPLINGS
 data <- data.frame(
   Treatment = rep(c("Control", "F", "TF", "TFB", "TFH"), each = 20),
   Encroachment = rep(rep(c("Highly encroached", "Moderately encroached"), each = 10), times = 5),
   ChangeSeedlings = c(
     rnorm(20, 28000, 30), rnorm(20, 24000, 30),
     rnorm(20, 24000, 40), rnorm(20, 20000, 40),
     rnorm(20, 19000, 30), rnorm(20, 15000, 30),
     rnorm(20, 8000, 50), rnorm(20, 5000, 60),
     rnorm(20, 11600, 70), rnorm(20, 8500, 80)
   )
 )
 
 # Ensure correct factor order
 data$Treatment <- factor(data$Treatment, 
                          levels = c("Control", "F", "TF", "TFB", "TFH"))
 
 # Create the boxplot
 ggplot(data, aes(x = Treatment, y = ChangeSeedlings, fill = Encroachment)) +
   geom_boxplot(position = position_dodge(width = 0.75), width = 0.6) +
   scale_fill_manual(values = c("Highly encroached" = "#00557F", "Moderately encroached" = "#FFC000")) +
   labs(y = "Change in seedlings and saplings (/ha)", x = "Treatments", fill = NULL) +
   theme_beautiful() +theme(
     legend.position = c(0.1, 1),
     legend.justification = c(0, 1),
     legend.box.just = "left",
     legend.title = element_text(face = "bold"),
     legend.background = element_rect(fill = "white", color = "gray90"))
 
 
 #ggsave(HE, filename = "Plots/Encroched Grass Biomass change.png",
 #                 width = 16, height = 14, units = "cm")
 
 
 
 
 ################################ COMPARING TREATMENT HEIGHTS
 
 #Gdata <- read.csv("DATA/March2025/GrassesCombined.csv",stringsAsFactors = TRUE)
 
 Grdata <- read_csv("DATA/March2025/GrassesCombined.csv")
 
 # Standardize column names
 names(Grdata) <- tolower(names(Grdata))
 # Rename all column names to lowercase and replace spaces with underscores
 names(Grdata) <- gsub(" ", "_", tolower(names(Grdata)))
 
 
 # Clean and format, FILTERING NAs
 
 Grdata <- Grdata %>%
   filter(!is.na(dpm_height)) %>%  # Exclude NAs in dpm_height
   mutate(
     year = as.factor(year),
     site = as.factor(site),
     plot = as.factor(plot),
     subplot = as.factor(subplot),
     treatment = as.factor(treatment),
     dpm_height = as.numeric(dpm_height) # Ensure numeric
   )
 
 # Check missing values
 #summary(Grdata$dpm_height)
 
 ###2. Create Pre/Post Variable
 
 Grdata <- Grdata %>%
   mutate(period = ifelse(as.numeric(as.character(year)) < 2025, "PRE", "POST")) %>%
   mutate(period = factor(period, levels = c("PRE", "POST")))
 
 ### Summarize by Subplot or Plot..Average height per subplot and period:
 
 summary_Grdata <- Grdata %>%
   group_by(site, plot, subplot, treatment, period) %>%
   summarise(mean_height = mean(dpm_height, na.rm = TRUE)) %>%
   ungroup()
 
 ####4. Calculate Change in Height (Post - Pre)
 height_change <- summary_Grdata %>%
   pivot_wider(names_from = period, values_from = mean_height) %>%
   mutate(delta = POST - PRE)
 
 ### Compare Treatment Effects (ANOVA on Delta)
# anova_model <- aov(delta ~ treatment, data = height_change)
# summary(anova_model)
 # the results show that the treatment groups do not differ significantly in their effect on the response variable.
 
 ## Optional: Tukey post-hoc test
# TukeyHSD(anova_model)
 
 
 ## USE OF Mixed-Effects Model (Handles Repeated Measures)
 #This is more robust as it uses the original data, accounts for plot/subplot as random effects, and tests interaction between time and treatment:
 
# lmm <- lmer(dpm_height ~ period * treatment + (1 | site/plot/subplot), data = Grdata)
 #summary(lmm)
 # Look at the interaction terms (periodPOST:treatmentX) to assess whether any treatment caused a significant change post-treatment.
 
 
 #Filtering NAs only when modeling Height
 
 model <- lmer(dpm_height ~ period * treatment + (1 | site/plot/subplot),
               data = Grdata[!is.na(Grdata$dpm_height), ])
 
 
 
 ##Plotting boxplot for before and after treatment
 #ggplot(Grdata[!is.na(Grdata$dpm_height), ],
  #      aes(x = treatment, y = dpm_height, fill = period)) +
   #geom_boxplot() +
   #theme_beautiful()
 
 ### Violin plot
 ggplot(Grdata[!is.na(Grdata$dpm_height), ],
        aes(x = treatment, y = dpm_height, fill = period)) +
   geom_violin(trim = FALSE) +
   labs(y = "Dpm height (cm)", x = "Treatments") +
   theme_beautiful()
 
 
 #7. Visualization (Optional)
 
 ggplot(Grdata, aes(x = period, y = dpm_height, color = treatment)) +
   stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.3)) +
   stat_summary(fun = mean, geom = "line", aes(group = treatment), position = position_dodge(width = 0.3)) +
   labs(title = "Change in Height by Treatment", y = "Mean Height", x = "Period")
 
 #######################################################################################################
 
  #####TRANSFORMING DATA  
 
 WGdata <- read_csv("DATA/DPM.csv")
 
 ################## DPM HEIGHT ~ OVEN DRIED WEIGHT. NO SQUARE ROOT
 # 2. Convert weight from grams to kg/ha
 #    Area of 34cm diameter disc = π * (0.17 m)^2 = 0.0908 m²
 frame_area <- pi * (0.17^2)  # = 0.0908 m²
 WGdata$Biomass_kg_ha <- WGdata$Weight * 10 / frame_area
 
 #LOG TRANSFORMING BOTH VARIABLE
 #Log-transform both variables (natural log)
 WGdata$log_Biomass_kg_ha <- log(WGdata$Biomass_kg_ha)
 WGdata$log_DPH_Height <- log(WGdata$DPH_Height)
 
 # Fit a linear model using log-transformed variables
 modelG <- lm(log_Biomass_kg_ha ~ log_DPH_Height, data = WGdata)
 tab_model(modelG)
 
 coef <- coefficients(modelG)
 r2 <- summary(modelG)$r.squared
 n <- nobs(modelG)
 eq <- paste0(
   "y = ", round(coef[1], 2), " + ", round(coef[2], 2), "x\n",
   "R² = ", round(r2, 3), ", n = ", n)

  ## Plot log-log regression
 LG <- ggplot(WGdata, aes(y = log_Biomass_kg_ha, x = log_DPH_Height)) +
   geom_point() +  
   geom_smooth(method = "lm", se = FALSE, color = "red") +
   annotate("text", x = Inf, y = -Inf, label = eq, hjust = 1.1, vjust = -0.5, size = 4, color = "black") +
   labs(
     x = "Log DPM Height (cm)",
     y = "Log Standing grass biomass (kg/ha)") +
   theme_beautiful()
 
 ggsave(LG,filename ="Plots/LOG Biomass.png",
          width = 16, height = 14, units = "cm" )
        
 
 
 #### SQUARE ROOTING BIOMASS AND DPM HEIGHT
 
 # Step 1: Square root transform both variables
 WGdata$sqrt_Biomass_kg_ha <- sqrt(WGdata$Biomass_kg_ha)
 WGdata$sqrt_DPH_Height <- sqrt(WGdata$DPH_Height)
 
 # Step 2: Fit a linear model
 model_sqrt <- lm(sqrt_Biomass_kg_ha ~ sqrt_DPH_Height, data = WGdata)
 tab_model(model_sqrt)
 
 # Step 3: Extract equation and R²
 # Step 3: Extract equation, R², and number of observations
 coef <- coefficients(model_sqrt)
 r2 <- summary(model_sqrt)$r.squared
 n <- nobs(model_sqrt)
 eq <- paste0(
   "y = ", round(coef[1], 2), " + ", round(coef[2], 2), "x\n",
   "R² = ", round(r2, 3), ", n = ", n)
 
 
 ###plotting 
  SQT <- ggplot(WGdata, aes(x = sqrt_DPH_Height, y = sqrt_Biomass_kg_ha)) +
   geom_point()  +
   geom_smooth(method = "lm", se = FALSE, color = "red") +
   annotate("text", x = Inf, y = -Inf, label = eq, hjust = 1.1, vjust = -0.5, size = 4, color = "black") +
   labs(
     x = "√DPM Height (cm)",
     y = "√Standing grass biomass (kg/ha)") +
   theme_beautiful()
 
  ggsave(SQT,filename ="Plots/SQRT biomass.png",
       width = 16, height = 14, units = "cm" )

 ################################################  
  