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
library(robustlmm) # for robust regression analysis
library(patchwork)
library(tibble) # for table

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
 

###################################### COMPARING OLS, TLS, RLM

#

Glog <- WGdata %>%
  mutate(
    lBiomass = log(Biomass_kg_ha),
    lHeight  = log(DPH_Height)
  ) %>%
  filter(is.finite(lBiomass), is.finite(lHeight))

# OLS log log 
ols_log <- lm(lBiomass ~ lHeight, data = Glog)
summary(ols_log)

### Robust regression (RLM, log–log)


ts_log <-  RobustLinearReg::theil_sen_regression(lBiomass ~ lHeight, data = Glog)
summary(ts_log)   # returns an lm‐like object

# Extract coefficients
ts_coef <- coef(ts_log)
ts_intercept <- ts_coef[1]
ts_slope     <- ts_coef[2]


#### TLS 
X <- cbind(Glog$lHeight, Glog$lBiomass)
X_centered <- scale(X, center = TRUE, scale = FALSE)

svd_fit <- svd(X_centered)
v <- svd_fit$v

tls_slope <- -v[1, 2] / v[2, 2]
tls_intercept <- mean(Glog$lBiomass) -
  tls_slope * mean(Glog$lHeight)


### Predictions log scale
Glog <- Glog %>%
  mutate(
    ols_pred = predict(ols_log),
    ts_pred  = ts_intercept + ts_slope * lHeight,
    tls_pred = tls_intercept + tls_slope * lHeight
  )

# Performance metrics
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
mae  <- function(obs, pred) mean(abs(obs - pred))
rss  <- function(obs, pred) sum((obs - pred)^2)


# Comparison table

perf_log <- tibble(
  Model = c("OLS (log)", "Theil–Sen (log)", "TLS (log)"),
  AIC = c(
    AIC(ols_log),
    NA,    # Not likelihood‐based
    NA     # Not likelihood‐based
  ),
  RMSE = c(
    rmse(Glog$lBiomass, Glog$ols_pred),
    rmse(Glog$lBiomass, Glog$ts_pred),
    rmse(Glog$lBiomass, Glog$tls_pred)
  ),
  RSS = c(
    rss(Glog$lBiomass, Glog$ols_pred),
    rss(Glog$lBiomass, Glog$ts_pred),
    rss(Glog$lBiomass, Glog$tls_pred)
  ),
  MAE = c(
    mae(Glog$lBiomass, Glog$ols_pred),
    mae(Glog$lBiomass, Glog$ts_pred),
    mae(Glog$lBiomass, Glog$tls_pred)
  )
)

perf_log



## diagnostic checks for ols log-log

qqnorm(resid(ols_log))
qqline(resid(ols_log))

# # diagnostic checks for rlm log-log
qqnorm(resid(ts_log))
qqline(resid(ts_log))


# Influence and leverage (cook distance)
plot(cooks.distance(ts_log), type = "h")
abline(h = 4 / nrow(Graw), col = "red")

## Influence and leverage
plot(cooks.distance(ols_log), type = "h")
abline(h = 4 / nrow(Graw), col = "red")

#testing for equal variance (homoscedasticity)
plot(fitted(ols_log), resid(ols_log),
     xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2)

# tests
# library(lmtest)
bptest(ols_log)


###### diagnostic check for log TLS - orthogonal residuals
ortho_resid <- (Glog$lBiomass -
                  (tls_intercept + tls_slope * Glog$lHeight)) /
  sqrt(1 + tls_slope^2)


#Q–Q plot (TLS normality assumption)
qqnorm(ortho_resid)
qqline(ortho_resid)

#Orthogonal residuals vs fitted
fitted_tls <- tls_intercept + tls_slope * Glog$lHeight

plot(fitted_tls, ortho_resid,
     xlab = "Fitted values (TLS)",
     ylab = "Orthogonal residuals")
abline(h = 0, lty = 2)


#Orthogonal residuals vs predictor
plot(Glog$lHeight, ortho_resid,
     xlab = "log(Height)",
     ylab = "Orthogonal residuals")
abline(h = 0, lty = 2)



############ Multipanel for original and log - QQ plots
# OLS residuals are vertical
res_ols_raw <- resid(ols_raw)
res_ols_log <- resid(ols_log)

## Robust (Theil–Sen) - residuals are vertical
fit_rlm_raw <- coef(rlm_raw)[1] + coef(rlm_raw)[2] * Graw$DPH_Height
res_rlm_raw <- Graw$Biomass_kg_ha - fit_rlm_raw

fit_rlm_log <- coef(ts_log)[1] + coef(ts_log)[2] * Glog$lHeight
res_rlm_log <- Glog$lBiomass - fit_rlm_log


# TLS - residuals orthogonal 
# TLS – raw
fit_tls_raw <- tls_intercept + tls_intercept * Graw$DPH_Height
res_tls_raw <- (Graw$Biomass_kg_ha - fit_tls_raw) / sqrt(1 + tls_intercept^2)

# TLS – log
fit_tls_log <- tls_intercept + tls_intercept * Glog$lHeight
res_tls_log <- (Glog$lBiomass - fit_tls_log) / sqrt(1 + tls_intercept^2)

# producing QQ plot and saving as png

png("Plots/QQ_multipanel_models2.png",
    width = 2400, height = 1600, res = 300) # png file


 par(mfrow = c(2, 3),
mar = c(3, 3, 2, 1),   # smaller inner margins
oma = c(0, 0, 2, 0))   # small outer margin


#OLS – original scale
qqnorm(res_ols_raw, main = "a) OLS")
stats::qqline(res_ols_raw)

# (b) OLS – log scale
qqnorm(res_ols_log, main = "b) OLS (log)")
stats::qqline(res_ols_log)

#c) Robust – original scale
qqnorm(res_rlm_raw, main = "c) Robust regression")
stats::qqline(res_rlm_raw)

#d) Robust – log scale
qqnorm(res_rlm_log, main = "d) Robust regression (log)")
stats::qqline(res_rlm_log)

#e) TLS – original scale (orthogonal)
qqnorm(res_tls_raw, main = "e) TLS")
stats::qqline(res_tls_raw)

#f) TLS – log scale (orthogonal)
qqnorm(res_tls_log, main = "f) TLS (log)")
stats::qqline(res_tls_log)

dev.off()






########################## UNTRANSFORMED DATA

# Remove missing / invalid values
Graw <- WGdata %>%
  filter(
    is.finite(Biomass_kg_ha),
    is.finite(DPH_Height)
  )
#OLS (untransformed)
ols_raw <- lm(Biomass_kg_ha ~ DPH_Height, data = Graw)
summary(ols_raw)

# RLM untransformed
rlm_raw <- RobustLinearReg::theil_sen_regression(
  Biomass_kg_ha ~ DPH_Height,
  data = Graw
)
int_rlm   <- coef(rlm_raw)[["(Intercept)"]]
slope_rlm <- coef(rlm_raw)[["DPH_Height"]]


# TLS untransformed
X <- cbind(Graw$DPH_Height, Graw$Biomass_kg_ha)
X_centered <- scale(X, center = TRUE, scale = FALSE)

svd_fit <- svd(X_centered)
v <- svd_fit$v

tls_slope <- -v[1,2] / v[2,2]
tls_intercept <- mean(Graw$Biomass_kg_ha) -
  tls_slope * mean(Graw$DPH_Height)

# Predictions on original scale
Graw <- Graw %>%
  mutate(
    ols_pred = predict(ols_raw),
    rlm_pred = int_rlm + slope_rlm * DPH_Height,
    tls_pred = tls_intercept + tls_slope * DPH_Height
  )

#Performance metrics
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
mae  <- function(obs, pred) mean(abs(obs - pred))
rss  <- function(obs, pred) sum((obs - pred)^2)


# model comparison table
perf_raw1 <- tibble(
  Model = c("OLS", "Theil–Sen", "TLS"),
  AIC = c(
    AIC(ols_raw),
    NA,
    NA
  ),
  RMSE = c(
    rmse(Graw$Biomass_kg_ha, Graw$ols_pred),
    rmse(Graw$Biomass_kg_ha, Graw$rlm_pred),
    rmse(Graw$Biomass_kg_ha, Graw$tls_pred)
  ),
  RSS = c(
    rss(Graw$Biomass_kg_ha, Graw$ols_pred),
    rss(Graw$Biomass_kg_ha, Graw$rlm_pred),
    rss(Graw$Biomass_kg_ha, Graw$tls_pred)
  ), 
  MAE = c(
    mae(Graw$Biomass_kg_ha, Graw$ols_pred),
    mae(Graw$Biomass_kg_ha, Graw$rlm_pred),
    mae(Graw$Biomass_kg_ha, Graw$tls_pred)
  )
)

perf_raw1


## diagnostics checks - normality of residuals
qqnorm(resid(ols_raw))
qqline(resid(ols_raw))

# Theil-san normality of residuals
qqnorm(resid(rlm_raw))
qqline(resid(rlm_raw))

# TLS - checking orthogonal residuals are normal
 # computing orthogonal residuals
ortho_resid_raw <- (Graw$Biomass_kg_ha -
                      (tls_intercept + tls_slope * Graw$DPH_Height)) /
  sqrt(1 + tls_slope^2)

#plotting
qqnorm(ortho_resid_raw,
       main = "TLS orthogonal residuals (original scale)")
qqline(ortho_resid_raw)

# Influence and leverage
plot(cooks.distance(ols_raw), type = "h")
abline(h = 4 / nrow(Graw), col = "red")


#testing for equal variance (homoscedasticity) -OLS
plot(fitted(ols_raw), resid(ols_raw),
     xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2)


# equal variance Theil-San
plot(fitted(ols_raw), resid(rlm_raw),
     xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2)



##############################################
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
 


 #################### COMPARING AIC, RMSE, RSS, MAE FOR LOG TRANSFORMED
 
 ###### option 4
 
 
 # Ensure data is properly formatted
 RSQTGdata <- na.omit(RSQTGdata[, c("log_Biomass_kg_ha", "log_DPH_Height")])
 
 # 1. Free-intercept model
 model_log_free <- RobustLinearReg::theil_sen_regression(log_Biomass_kg_ha ~ log_DPH_Height, data = RSQTGdata)
 
 # 2. Zero-intercept model - SIMPLER APPROACH
 # Manually calculate Theil-Sen slope through origin
 calc_theil_sen_zero_intercept <- function(x, y) {
   # Calculate all pairwise slopes (through origin)
   n <- length(x)
   slopes <- numeric(0)
   
   for (i in 1:n) {
     for (j in 1:n) {
       if (i != j && x[i] != 0 && x[j] != 0) {
         slope_ij <- (y[i] - y[j]) / (x[i] - x[j])
         slopes <- c(slopes, slope_ij)
       }
     }
   }
   
   # Median slope
   median_slope <- median(slopes, na.rm = TRUE)
   return(median_slope)
 }
 
 # Calculate zero-intercept slope
 zero_slope <- calc_theil_sen_zero_intercept(RSQTGdata$log_DPH_Height, RSQTGdata$log_Biomass_kg_ha)
 
 # Create predictions for zero-intercept model
 pred_zero <- zero_slope * RSQTGdata$log_DPH_Height
 
 # Alternative: Use lm() with Theil-Sen slopes for comparison
 model_lm_free <- lm(log_Biomass_kg_ha ~ log_DPH_Height, data = RSQTGdata)
 model_lm_zero <- lm(log_Biomass_kg_ha ~ log_DPH_Height - 1, data = RSQTGdata)
 
 # COMPARISON FUNCTION
 compare_models <- function(data, pred_free, pred_zero, k_free = 2, k_zero = 1) {
   
   actual <- data$log_Biomass_kg_ha
   
   # Calculate residuals
   resid_free <- actual - pred_free
   resid_zero <- actual - pred_zero
   
   # Basic metrics
   n <- nrow(data)
   
   # RSS
   rss_free <- sum(resid_free^2)
   rss_zero <- sum(resid_zero^2)
   
   # RMSE
   rmse_free <- sqrt(mean(resid_free^2))
   rmse_zero <- sqrt(mean(resid_zero^2))
   
   # MAE
   mae_free <- median(abs(resid_free))
   mae_zero <- median(abs(resid_zero))
   
   # AIC approximation (n * log(RSS/n) + 2k)
   aic_free <- n * log(rss_free / n) + 2 * k_free
   aic_zero <- n * log(rss_zero / n) + 2 * k_zero
   
   # R-squared (pseudo for robust)
   ss_total <- sum((actual - mean(actual))^2)
   r2_free <- 1 - (rss_free / ss_total)
   r2_zero <- 1 - (rss_zero / ss_total)
   
   # Adjusted R-squared
   r2_adj_free <- 1 - ((1 - r2_free) * (n - 1) / (n - k_free))
   r2_adj_zero <- 1 - ((1 - r2_zero) * (n - 1) / (n - k_zero))
   
   # Create comparison table
   comparison <- data.frame(
     Metric = c("RSS", "RMSE", "MAE", "AIC", "R²", "Adj-R²"),
     Free_Intercept = c(
       round(rss_free, 4),
       round(rmse_free, 4),
       round(mae_free, 4),
       round(aic_free, 2),
       round(r2_free, 4),
       round(r2_adj_free, 4)
     ),
     Zero_Intercept = c(
       round(rss_zero, 4),
       round(rmse_zero, 4),
       round(mae_zero, 4),
       round(aic_zero, 2),
       round(r2_zero, 4),
       round(r2_adj_zero, 4)
     )
   )
   
   return(comparison)
 }
 
 # Get predictions for free-intercept model
 pred_free <- predict(model_log_free, newdata = data.frame(log_DPH_Height = RSQTGdata$log_DPH_Height))
 
 # Compare models
 comparison <- compare_models(
   data = RSQTGdata,
   pred_free = pred_free,
   pred_zero = pred_zero,
   k_free = 2,  # intercept + slope
   k_zero = 1   # slope only
 )
 
 print(comparison)
 
 
 
 
 
 
 ###################################################################
 ##################################### RLM PREDICTING BIOMASS FROM DPM HEIGHT
 

 # Read the data
 PrdRLM <- read.csv("DATA/March2025/GrassHeight.csv")  
 

# model 1 with free intercept LOG transformation
 
 intercept_free <- 4.67
 slope_free <- 1.14
 
 # MODEL 2: Zero-intercept model 
 slope_zero <- 1.14
 
 # Calculate biomass predictions using both models
 PrdRLM <-  PrdRLM %>%
   mutate(
     # Free-intercept model predictions
     biomass_free = exp(intercept_free) * (DPM_Height ^ slope_free),
     
     # Zero-intercept model predictions
     biomass_zero = DPM_Height ^ slope_zero,
     
     # For reference, also calculate the log-transformed predictions
     log_biomass_free = intercept_free + slope_free * log(DPM_Height),
     log_biomass_zero = slope_zero * log(DPM_Height)
   )
 
 # View the results
 cat("=== Biomass Predictions ===\n")
 print(head( PrdRLM))
 
 # Export the predictions to a new CSV file
 #write.csv(data, "DATA/RLM_biomass_predictions.csv", row.names = FALSE)
 
 


 ########################################################################################
 ##################################################################################
 ############################################################################
 
 #### USING SQUARE ROOT TRANSFORMATIONS
 
 ## 1. Convert weight from grams to kg/ha
 #    Area of 34cm diameter disc = π * (0.17 m)^2 = 0.0908 m²
 frame_area <- pi * (0.17^2)  # = 0.0908 m²
 RSQTGdata$Biomass_kg_ha <- RSQTGdata$Weight * 10 / frame_area # 10 = conversion factor: 10 000 m2 ha−1 /1000 g kg-1
 

 # Using square root transformation 
 RSQTGdata$sqrt_Biomass_kg_ha <- sqrt( RSQTGdata$Biomass_kg_ha)
 RSQTGdata$sqrt_DPH_Height  <- sqrt(RSQTGdata$DPH_Height)
 
 # 1. Free-intercept robust model with square root
 RSmodel_free <- RobustLinearReg::theil_sen_regression(sqrt_Biomass_kg_ha ~ sqrt_DPH_Height, data = RSQTGdata)
 int_free   <- round(coef(RSmodel_free)[["(Intercept)"]], 2)
 slope_free <- round(coef(RSmodel_free)[["sqrt_DPH_Height"]], 2)
 
 # Clean equation: sqrt(y) = intercept + slope * log(x)
 Req_free <- sprintf("sqrt(y) = %.2f %+.2f sqrt(x)", int_free, slope_free)
 
 # 2. Zero-intercept robust model (forced through origin)
 RSmodel_zero <- RobustLinearReg::theil_sen_regression(sqrt_Biomass_kg_ha ~ 0 + sqrt_DPH_Height, data = RSQTGdata)
 
 slope_zero <- round(coef(RSmodel_zero)[["sqrt_DPH_Height"]], 2)
 
 # Clean equation: no intercept term
 Req_zero <- sprintf("sqrt(y) = %.2f sqrt(x)", slope_zero)
 
 
 ##### --- ggplot with both models 
 RLMB2 <- ggplot(RSQTGdata, aes(x = sqrt_DPH_Height, y = sqrt_Biomass_kg_ha)) +
   geom_point(color = "black", alpha = 0.3) +
   
   # Free-intercept line (solid blue)
   geom_smooth(method = "lm", formula = y ~ x,
               color = "blue", linewidth = 0.9, se = FALSE, fullrange = FALSE) +
   
   # Zero-intercept line (dashed red)
   geom_smooth(method = "lm", formula = y ~ x - 1,
               color = "red", linewidth = 0.7, linetype = "solid", se = FALSE, fullrange = FALSE) +
   
   
   ## adjusting font size for equations
   annotate("text", x = -Inf, y = Inf, label = Req_free,
            hjust = -0.1, vjust = 1.5, color = "blue", size = 1.8) +
   
   annotate("text", x = -Inf, y = Inf, label = Req_zero,
            hjust = -0.1, vjust = 3.5, color = "red", size = 1.8) +
   labs(x = "Sqrt DPM Height (cm)",
        y = "Sqrt Standing grass biomass ("*kg~ha^{-1}*")")+
   theme_beautiful()+
   theme(
     axis.title = element_text(size = 12),      # Axis titles
     axis.text = element_text(size = 12)        # Axis tick labels
   )
 
 ## ggsave plot
 
 #ggsave(RLMB2, filename = "Plots/ SqrtBiomass - RLM.png",width = 16, height = 14, units = "cm")   
 
 
################## COMPARING AIC, RMSE, RSS
 
 #  Free-intercept model (regular Theil-Sen regression)
 RSmodel_free <- RobustLinearReg::theil_sen_regression(sqrt_Biomass_kg_ha ~ sqrt_DPH_Height, data = RSQTGdata)
 
 # Zero-intercept model (force through origin)
 RSmodel_zero <- RobustLinearReg::theil_sen_regression(sqrt_Biomass_kg_ha ~ 0 + sqrt_DPH_Height, data = RSQTGdata)
 
 # Calculate predictions
 RSQTGdata$pred_free <- coef(RSmodel_free)[["(Intercept)"]] + coef(RSmodel_free)[["sqrt_DPH_Height"]] * RSQTGdata$sqrt_DPH_Height
 RSQTGdata$pred_zero <- slope_zero * RSQTGdata$sqrt_DPH_Height
 
 # Calculate residuals
 RSQTGdata$resid_free <- RSQTGdata$sqrt_Biomass_kg_ha - RSQTGdata$pred_free
 RSQTGdata$resid_zero <- RSQTGdata$sqrt_Biomass_kg_ha - RSQTGdata$pred_zero
 
 # Calculate metrics
 n <- nrow(RSQTGdata)
 
 RSS_free <- sum(RSQTGdata$resid_free^2)
 RSS_zero <- sum(RSQTGdata$resid_zero^2)
 
 RMSE_free <- sqrt(mean(RSQTGdata$resid_free^2))
 RMSE_zero <- sqrt(mean(RSQTGdata$resid_zero^2))
 
 # AIC = n * log(RSS/n) + 2*k
 AIC_free <- n * log(RSS_free/n) + 2 * 2  # 2 parameters
 AIC_zero <- n * log(RSS_zero/n) + 2 * 1  # 1 parameter
 
 # Create comparison table
 comparison <- data.frame(
   Model = c("Free Intercept", "Zero Intercept"),
   AIC = round(c(AIC_free, AIC_zero), 1),
   RMSE = round(c(RMSE_free, RMSE_zero), 2),
   RSS = round(c(RSS_free, RSS_zero), 2),
   Slope = c(13.95, 13.95),
   Intercept = c(-4.6, 0),
   Parameters = c(2, 1)
 )
 
 print("=== Model Comparison ===")
 print(comparison)
 
 # Which model is better?
 delta_AIC <- AIC_free - AIC_zero
 cat("\nAIC difference:", round(delta_AIC, 1), "\n")
 
 if (abs(delta_AIC) < 2) {
   cat("Models are statistically equivalent (ΔAIC < 2)\n")
   cat("Recommendation: Use simpler zero-intercept model (more parsimonious)\n")
 } else if (delta_AIC > 0) {
   cat("Zero-intercept model is better (lower AIC by", round(delta_AIC, 1), "points)\n")
   cat("Recommendation: Use zero-intercept model\n")
 } else {
   cat("Free-intercept model is better (lower AIC by", round(-delta_AIC, 1), "points)\n")
   cat("Recommendation: Use free-intercept model\n")
 }
 # Answer: Free-intercept model is better (lower AIC by 8.9 points)
 
###################################################################
 
 # back transforming sqrt
 
 # Predict on square-root scale
 RSQTGdata$pred_sqrt_free <- predict(RSmodel_free, data = RSQTGdata)
 RSQTGdata$pred_sqrt_zero <- predict(RSmodel_zero, data = RSQTGdata)
 
 # manually calculating for zero intercept model
 b <- coef(RSmodel_zero)[1]
 RSQTGdata$pred_sqrt_zero <- b * RSQTGdata$sqrt_DPH_Height
 
 
 # Enforce non-negativity
 RSQTGdata$pred_sqrt_free[RSQTGdata$pred_sqrt_free < 0] <- 0
 #RSQTGdata$pred_sqrt_zero[RSQTGdata$pred_sqrt_zero < 0] <- 0
 RSQTGdata$pred_biomass_zero <- RSQTGdata$pred_sqrt_zero^2
 
 
 # Back-transform to biomass
 RSQTGdata$pred_biomass_free <- RSQTGdata$pred_sqrt_free^2
 RSQTGdata$pred_biomass_zero <- RSQTGdata$pred_sqrt_zero^2
 

 