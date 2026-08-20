
BiasC2 <- ggplot(plot_df, aes(x = x, y = Biomass, color = Model)) +
  geom_line(linewidth = 0.7) +
# Observed calibration data points
geom_point(
  data = WGdata,  # Your observed data frame
  aes(x = DPH_Height, y = Biomass_kg_ha),  # Adjust column names as needed
  color = "Green",
  size = 2.5,
  shape = 16,  # Solid circle
  inherit.aes = FALSE  # Don't inherit color mapping from main plot
) +
  scale_color_manual(values = model_colors, labels = c("SHR", "Trollope", "Zambatis")) +
  labs(
    x = "DPM Height (cm)",
    y = "Standing grass biomass ("*kg~ha^{-1}*")",
    color = "Model"
  ) +
  geom_text(
    data = equations_df,
    aes(x = x, y = y, label = eq),
    hjust = 0.0,   # push text right from left margin
    vjust = 1.0,    # push text down from top margin
    size  = 3.0,     # ⬅ reduced font size (adjust if needed)
    lineheight = 1.0,       # top alignment
    show.legend = "FALSE"
  ) +
  #theme_minimal(base_size = 13) +
  theme_classic()+
  theme(
    legend.position = "top")+
  # theme(
  # legend.position = c(0, 1),
  # legend.justification = c(0, 0.8),
  #legend.box.just = "left")+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )


###### Combine the plots in a single layout 
multi_pBiomass2 <- (RLMB/BiasC2) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(
    nrow = 2,
    guides = "collect"
  ) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = theme(
      plot.tag = element_text(size = 6, face = "plain", hjust = 0)
    )
  ) &
  theme_classic() &
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    strip.text = element_text(size = 10, face = "plain"),
    panel.grid.minor = element_blank(),
    plot.margin = margin( 3, 3)
  )


######################### MODEL COMPARISONS




# load data
WGdata <- read_csv("DATA/DPM.csv")
WGsqtdata <- read_csv("DATA/DPM.csv")
RSQTGdata <- read_csv("DATA/DPM.csv")

################## DPM HEIGHT ~ OVEN DRIED WEIGHT. NO TRANSFORMATION
# 2. Convert weight from grams to kg/ha
#    Area of 34cm diameter disc = π * (0.17 m)^2 = 0.0908 m²
frame_area <- pi * (0.17^2)  # = 0.0908 m²
WGdata$Biomass_kg_ha <- WGdata$Weight * 10 / frame_area


# 3. Linear regression: Biomass ~ DPH
modelWG <- lm(Biomass_kg_ha ~ DPH_Height, data = WGdata)
#summary(modelWG)
# tab_model(modelWG)

# model diagnostics
qqnorm(residuals(modelWG))
qqline(residuals(modelWG))

# checking for equal variance
plot(fitted(modelWG), residuals(modelWG),
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, lty = 2)



###adding annotation to the plot

# Extract coefficients
coefs <- coef(modelWG)
intercept <- round(coefs[1], 2)
slope <- round(coefs[2], 2)

# Create equation string for annotation
# Build the equation string (biomass = intercept + slope * x)
eq <- paste0("Biomass== ", intercept, " + ", slope, " %*% Dpm")

coef <- coefficients(modelWG)
r2 <- summary(modelWG)$r.squared
#n <- nobs(modelWG)
eq <- paste0(
  "y = ", round(coef[1], 2), " + ", round(coef[2], 2), "x\n",
  "R² = ", round(r2, 3))#, ", n = ", n)


# Plot with regression and equation

DPM <- ggplot(WGdata, aes(x = DPH_Height, y = Biomass_kg_ha)) + 
  geom_point(size = 0.7, color = "black") + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  annotate("text", x = Inf, y = -Inf, label = eq, hjust = 1.0, vjust = -0.1, size = 2.0, color = "black") +
  labs( x = "DPM height (cm)",
        y = "Standing grass biomass ("*kg~ha^{-1}*")") +
  theme_beautiful()+
  theme(
    axis.title = element_text(size = 11),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

## saving plot
#ggsave(DPM,filename ="Plots/DPM&Biomass.png",width = 16, height = 14, units = "cm")  


# MODEL performance
#performance::check_model(modelWG)


#############################################
############# LOG TRANSFORMED RLM            LOG TRANSFORMED RLM     


## 1. Convert weight from grams to kg/ha
#    Area of 34cm diameter disc = π * (0.17 m)^2 = 0.0908 m²
frame_area <- pi * (0.17^2)  # = 0.0908 m²
RSQTGdata$Biomass_kg_ha <- RSQTGdata$Weight * 10 / frame_area

# --- Log-transform the variables (natural log) ---
# Add small constant if any zeros in data (rare for height/biomass, but safe)

# Here we use 1 as additive constant; adjust if needed
#RSQTGdata$log_Biomass_kg_ha <- log(RSQTGdata$Biomass_kg_ha + 1)
# RSQTGdata$log_DPH_Height  <- log(RSQTGdata$DPH_Height + 1)


# If you're certain there are no zeros, you can skip +1:
RSQTGdata$log_Biomass_kg_ha <- log(RSQTGdata$Biomass_kg_ha)
RSQTGdata$log_DPH_Height  <- log(RSQTGdata$DPH_Height)

# 1. Free-intercept robust model
Rmodel_free <- RobustLinearReg::theil_sen_regression(log_Biomass_kg_ha ~ log_DPH_Height, data = RSQTGdata)

int_free   <- round(coef(Rmodel_free)[["(Intercept)"]], 2)
slope_free <- round(coef(Rmodel_free)[["log_DPH_Height"]], 2)

# Clean equation: log(y) = intercept + slope * log(x)
Req_free <- sprintf("log(y) = %.2f %+.2f log(x)", int_free, slope_free)

# 2. Zero-intercept robust model (forced through origin)
Rmodel_zero <- RobustLinearReg::theil_sen_regression(log_Biomass_kg_ha ~ 0 + log_DPH_Height, data = RSQTGdata)

slope_zero <- round(coef(Rmodel_zero)[["log_DPH_Height"]], 2)

# Clean equation: no intercept term
Req_zero <- sprintf("log(y) = %.2f log(x)", slope_zero)


#### --- ggplot with both models 
RLMB <- ggplot(RSQTGdata, aes(x = log_DPH_Height, y = log_Biomass_kg_ha)) +
  geom_point(color = "black", alpha = 0.54) +
  
  # Free-intercept line (solid blue)
  geom_smooth(method = "lm", formula = y ~ x,
              color = "blue", linewidth = 0.7, se = FALSE, fullrange = FALSE) +
  
  # Zero-intercept line (dashed red)
  geom_smooth(method = "lm", formula = y ~ x - 1,
              color = "red", linewidth = 0.7, linetype = "solid", se = FALSE, fullrange = FALSE) +
  
  
  ## adjusting font size for equations
  annotate("text", x = -Inf, y = Inf, label = Req_free,
           hjust = -0.1, vjust = 1.5, color = "blue", size = 5.0) +
  
  annotate("text", x = -Inf, y = Inf, label = Req_zero,
           hjust = -0.1, vjust = 3.5, color = "red", size = 5.0) +
  labs(x = "Log DPM Height (cm)",
       y = "Log Standing grass biomass ("*kg~ha^{-1}*")") +
  theme_beautiful()+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )

## ggsave plot

#ggsave(RLMB, filename = "Plots/ LogBiomass - RLM.png",width = 16, height = 14, units = "cm")   


####
# Free-intercept model
model_log_free <- RobustLinearReg::theil_sen_regression(log_Biomass_kg_ha ~ log_DPH_Height, data = RSQTGdata)
int_free   <- round(coef(model_log_free)[["(Intercept)"]], 3)
slope_free <- round(coef(model_log_free)[["log_DPH_Height"]], 3)

# Zero-intercept model
model_log_zero <- RobustLinearReg::theil_sen_regression(log_Biomass_kg_ha ~ 0 + log_DPH_Height, data = RSQTGdata)
slope_zero <- round(coef(model_log_zero)[["log_DPH_Height"]], 3)


#################################################################

## Comparing three models log SHR, Trollope, Zambatis

# predicting biomass for each model
comp <- read_csv("DATA/DPM.csv") %>%
  mutate(
    DPH_Height = as.numeric(DPH_Height)
  ) %>%
  filter(!is.na(DPH_Height)) %>%
  mutate(
    SHR = round(exp(4.67 + 1.141 * log(DPH_Height)), 0),  #round = removing decimals
    Trollope = round(-3019 + 2260 * sqrt(DPH_Height), 0),
    Zambatis = round((31.7176 * (0.3218^(1/DPH_Height)) * (DPH_Height^0.2834))^2, 0)
  )


# Save results
#write.csv(comp, "DATA/RLM LOGbiomass_predictions.csv", row.names = FALSE)

# Reshape data to long format for ggplot
df_long <- comp %>%
  pivot_longer(cols = c("SHR", "Trollope", "Zambatis"),
               names_to = "Model",
               values_to = "Biomass")

### Define the equations as text labels (adjust if your model names differ)
equations <- data.frame(
  Model = c("SHR", "Trollope", "Zambatis"),
  label = c(
    "~log(italic(y)) == 4.674 + 1.141%*% log(italic(x))",
    "~ italic(y) == -3019 + 2260 %*% sqrt(italic(x))",
    "~italic(y) == (31.7176 %*% 0.3218^(1/italic(x)) %*% italic(x)^0.2834)^2"
  ),
  color = c("blue", "black", "brown"),
  stringsAsFactors = FALSE
)


# Create the plot   
# Equations placed in the BOTTOM-RIGHT corner
COMB <- ggplot(df_long, aes(x = DPH_Height, y = Biomass, color = Model, linetype = Model)) +
  geom_line(size = 0.7) +
  geom_text(data = equations,
            aes(label = label),
            x = Inf,                  # right edge
            y = -Inf,                 # bottom edge
            hjust = 1.1,              # nudge slightly left from right edge
            vjust = c(-5.0, -3.5, -1.5),  # negative values push text upward from bottom
            color = equations$color,
            size = 1.4,
            fontface = "bold",
            parse = TRUE) +     
  scale_color_manual(values = c("SHR" = "blue", 
                                "Trollope" = "black", 
                                "Zambatis" = "brown")) +
  scale_linetype_manual(values = c("SHR" = "solid", 
                                   "Trollope" = "solid", 
                                   "Zambatis" = "solid")) +
  labs(
    x = "DPM Height (cm)",
    y = "Standing grass biomass ("*kg~ha^{-1}*")",
    color = "Model",
    linetype = "Model"
  ) +
  theme_beautiful()+
  
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 0.8),
    legend.box.just = "left")+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )


# ggsave
#ggsave(COMB, filename = "Plots/ Model Comparisons.png",width = 16, height = 14, units = "cm")   


## Combine the plots in a single layout 
#multi_pBiomass <- (DPM | RLMB| COMB) +
multi_pBiomass <- (RLMB| COMB)+
  plot_layout(
    nrow = 1,
    guides = "collect"
  ) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = theme(
      plot.tag = element_text(size = 6, face = "plain", hjust = 0)
    )
  ) &
  theme_classic() &
  theme(
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    strip.text = element_text(size = 8, face = "plain"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 5, 5, 5)
  )

##ggsave multipanel 
ggsave(multi_pBiomass,filename ="Plots/R1ModelComparisons Biomass.png",
       width = 18, height = 6, units = "cm", dpi = 300)  





#################### 
########### BIAS CORRECTED SHR MODEL -> MEAN BIOMASS FOR ALL MODELS

# residuals 
resA <- log(RSQTGdata$log_Biomass_kg_ha) - (4.67 + 1.14 * (RSQTGdata$log_DPH_Height))

sigma2 <- var(resA)
sigma2

# Prediction grid biomass
x_new <- seq(min(RSQTGdata$DPH_Height), max(RSQTGdata$DPH_Height), length.out = 120)


# 2. Predictions
pred_df3 <- data.frame(
  x = x_new,
  SHR = exp(4.67 + 1.141 * log(x_new) + 0.5*sigma2),
  Trollope = -3019 + 2260 * sqrt(x_new),
  Zambatis = (31.7176 * 0.3218^(1 / x_new) * x_new^0.2834)^2
)

# Constrain Model B
#pred_df$B[pred_df$B < 0] <- 0

# 3. Long format for ggplot
plot_df <- pred_df3 %>%
  pivot_longer(cols = -x, names_to = "Model", values_to = "Biomass")

# 4. Colors for each model
model_colors <- c(
  "SHR" = "blue",
  "Trollope" = "brown",
  "Zambatis" = "black"
)

# Placing equations 
x_pos <- min(RSQTGdata$DPH_Height) + 0.10 * diff(range(RSQTGdata$DPH_Height))
y_pos <- min(RSQTGdata$Biomass_kg_ha) + 0.00 * diff(range(RSQTGdata$Biomass_kg_ha))

# 5. Equations as separate annotation entries (with color)
equations_df <- data.frame(
  eq = c(
    "log(y) = 4.67 + 1.141 log(x)",
    "y = -3019 + 2260 sqrt(x)",
    "y = (31.7176 * 0.3218^(1/x) * x^0.2834)^2"
  ),
  Model = c("SHR", "Trollope", "Zambatis"),
  x = x_pos,
  y = y_pos + c(-0.12, -0.06, 0.0) * diff(range(RSQTGdata$Biomass_kg_ha))
)

# 6. Plot
BiasC <- ggplot(plot_df, aes(x = x, y = Biomass, color = Model)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = model_colors, labels = c("SHR", "Trollope", "Zambatis")) +
  labs(
    x = "DPM Height (cm)",
    y = "Standing grass biomass ("*kg~ha^{-1}*")",
    color = "Model"
  ) +
  geom_text(
    data = equations_df,
    aes(x = x, y = y, label = eq, color = Model),
    hjust = 0.0,
    vjust = 0,
    fontface = "plain",
    size = 1.8,
    show.legend = FALSE
  ) +
  #theme_minimal(base_size = 13) +
  theme_beautiful()+
  #theme(
  # legend.position = "top")
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 0.8),
    legend.box.just = "left")+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )


#### Combine the plots in a single layout 
# multi_pBiomass <- (DPM | RLMB|BiasC) +   # "/" for stacking vertically, or "|" for side-by-side
#   plot_layout(
#     nrow = 1,
#     guides = "collect"
#   ) +
#   plot_annotation(
#     tag_levels = "a",
#     tag_prefix = "(",
#     tag_suffix = ")",
#     theme = theme(
#       plot.tag = element_text(size = 6, face = "plain", hjust = 0)
#     )
#   ) &
#   theme_classic() &
#   theme(
#     axis.text = element_text(size = 7),
#     axis.title = element_text(size = 8),
#     strip.text = element_text(size = 8, face = "plain"),
#     panel.grid.minor = element_blank(),
#     plot.margin = margin(5, 5, 5, 5)
#   )
# 
# ##ggsave multipanel 
#ggsave(multi_pBiomass,filename ="Plots/3BiasCdModelComparison Biomass.png",
#      width = 18, height = 6, units = "cm", dpi = 300)  

###########################################################

##### REFINING BiasC TO INCLUDE n, equations moved to top

# residuals 
resA <- log(RSQTGdata$log_Biomass_kg_ha) - (4.67 + 1.14 * (RSQTGdata$log_DPH_Height))

sigma2 <- var(resA)
sigma2

# Prediction grid biomass
x_new <- seq(min(RSQTGdata$DPH_Height), max(RSQTGdata$DPH_Height), length.out = 120)


# 2. Predictions
pred_df3 <- data.frame(
  x = x_new,
  SHR = exp(4.67 + 1.141 * log(x_new) + 0.5*sigma2),
  Trollope = -3019 + 2260 * sqrt(x_new),
  Zambatis = (31.7176 * 0.3218^(1 / x_new) * x_new^0.2834)^2
)

# Constrain Model B
#pred_df$B[pred_df$B < 0] <- 0

# 3. Long format for ggplot
plot_df <- pred_df3 %>%
  pivot_longer(cols = -x, names_to = "Model", values_to = "Biomass")

# 4. Colors for each model
model_colors <- c(
  "SHR" = "blue",
  "Trollope" = "brown",
  "Zambatis" = "black"
)

# Placing equations  Top-left anchor position
x_inset <- 0.00 * diff(range(RSQTGdata$DPH_Height))    # 1% inset from left
y_step  <- 0.08 * diff(range(RSQTGdata$Biomass_kg_ha))   # row spacing
y_inset <- 0.01 * diff(range(RSQTGdata$Biomass_kg_ha))   # inset from top


# 5. Equations as separate annotation entries (with color)
equations_df <- data.frame(
  eq = c( 
    "n = 120",
    "log(y) = 4.67 + 1.141*log(x)",
    "y = -3019 + 2260 *sqrt(x)",
    "y = (31.7176 * 0.3218^(1/x) * x^0.2834)^2"
  ),
  Model = c("n", "SHR", "Trollope", "Zambatis"),
  x = min(RSQTGdata$DPH_Height),
  y = max(RSQTGdata$Biomass_kg_ha) - c(0, 1, 2, 3) * y_step
)


## 6. Plot
BiasC2 <- ggplot(plot_df, aes(x = x, y = Biomass, color = Model)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = model_colors, labels = c("SHR", "Trollope", "Zambatis")) +
  labs(
    x = "DPM Height (cm)",
    y = "Standing grass biomass ("*kg~ha^{-1}*")",
    color = "Model"
  ) +
  geom_text(
    data = equations_df,
    aes(x = x, y = y, label = eq),
    hjust = 0.0,   # push text right from left margin
    vjust = 1.0,    # push text down from top margin
    size  = 3.0,     # ⬅ reduced font size (adjust if needed)
    lineheight = 1.0,       # top alignment
    show.legend = "FALSE"
  ) +
  #theme_minimal(base_size = 13) +
  theme_classic()+
  theme(
    legend.position = "top")+
  # theme(
  # legend.position = c(0, 1),
  # legend.justification = c(0, 0.8),
  #legend.box.just = "left")+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )


###### Combine the plots in a single layout 
multi_pBiomass2 <- (RLMB/BiasC2) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(
    nrow = 2,
    guides = "collect"
  ) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = theme(
      plot.tag = element_text(size = 6, face = "plain", hjust = 0)
    )
  ) &
  theme_classic() &
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    strip.text = element_text(size = 10, face = "plain"),
    panel.grid.minor = element_blank(),
    plot.margin = margin( 3, 3)
  )


##ggsave multipanel 
ggsave(multi_pBiomass2,filename ="Plots/ModelComparison-BIOMASS.png",
       width = 16, height = 14, units = "cm", dpi = 300)


#################################################################
