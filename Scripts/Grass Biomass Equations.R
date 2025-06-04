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



################ FOURTH ATTEMPT TO CONSTRAIN THE MODELS

# --- Clean and prepare data ---
# Ensure numeric and clean
WGdata$DPH_Height <- as.numeric(WGdata$DPH_Height)
WGdata$Biomass_kg_ha <- as.numeric(WGdata$Biomass_kg_ha)

# Remove rows with missing or non-finite values
WGdata <- na.omit(WGdata)
WGdata <- WGdata[is.finite(WGdata$DPH_Height) & is.finite(WGdata$Biomass_kg_ha), ]

# Filter strictly positive values (necessary for power/sqrt models)
WGdata <- WGdata[WGdata$DPH_Height > 0 & WGdata$Biomass_kg_ha > 0, ]



# --- Fit power model: Biomass = a * DPH^b ---

power_mod <- nls(Biomass_kg_ha ~ a *DPH_Height^b,
                 data = WGdata,
                 start = list(a = 1, b = 0.5),
                 control = nls.control(maxiter = 100, warnOnly = TRUE))


# --- Fit constrained Trollope model: Biomass = a * sqrt(DPH) ---
WGdata$sqrtDPH <- sqrt(WGdata$DPH_Height)
trollope_mod <- nls(Biomass_kg_ha ~ a * sqrtDPH,
                    data = WGdata,
                    start = list(a = 1))

# --- Coefficients ---
a_power <- coef(power_mod)["a"]
b_power <- coef(power_mod)["b"]
a_trollope <- coef(trollope_mod)["a"]

# --- Prediction data ---
x_vals <- seq(0, max(WGdata$DPH_Height), length.out = 100)
pred_df <- data.frame(
  DPH_Height = x_vals,
  Power_Model = a_power * (x_vals^b_power),
  Trollope_Model = a_trollope * sqrt(x_vals)
)

# --- Reshape for plotting ---
library(tidyr)
plot_df <- pivot_longer(pred_df, cols = c(Power_Model, Trollope_Model),
                        names_to = "Model", values_to = "Biomass")

# --- Plot ---
library(ggplot2)
ggplot(WGdata, aes(x = DPH_Height, y = Biomass_kg_ha)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_line(data = plot_df, aes(x = DPH_Height, y = Biomass, color = Model), size = 1.2) +
  scale_color_manual(values = c("Power_Model" = "blue", "Trollope_Model" = "green")) +
  labs(
    title = "Biomass vs DPH: Power vs Trollope Model (Both Constrained to 0)",
    x = "DPH Height (cm)",
    y = "Biomass (kg/ha)",
    color = "Model"
  ) +
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
