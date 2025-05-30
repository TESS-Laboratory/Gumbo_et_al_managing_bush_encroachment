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




# 1. Load the CSV file
Fdata <- read_csv("C:/workspace/gumbo_dev/DATA/March2025/Fuelload .csv") 

#Fdata <- read_csv("DATA/March2025/Fuelload .csv")  
# 2. Check the structure
str(data)
summary(data)

##RENAMING COLUMNS
colnames(Fdata) <- c("x", "y")

# 3. Separate known values
known <- subset(Fdata, !is.na(y))  # Rows where y is NOT missing
unknown <- subset(Fdata, is.na(y)) # Rows where y IS missing

# 4. Fit a power model: y = a * x^b
model <- nls(y ~ a * x^b, data = known, start = list(a = 700, b = 0.75))

# 5. View model summary
summary(model)

## Obtain the coeffient
coef(model)
 

# 6. Predict y for unknown x
unknown$y_pred <- predict(model, newdata = unknown)

# 7. Combine known and predicted data
known$y_pred <- known$y  # Copy original values
final <- rbind(known, unknown)

# 8. Save to new CSV (optional)
write.csv(final, "fuel_load_with_predictions.csv", row.names = FALSE)

# 9. Plot (optional)
plot(known$x, known$y, pch=19, col="blue", xlab="Height (cm)", ylab="Fuel Load (kg/ha)")
points(unknown$x, unknown$y_pred, pch=17, col="red")
curve(coef(model)[1] * x^coef(model)[2], add=TRUE, col="darkgreen", lwd=2)
legend("topleft", legend=c("Observed", "Predicted", "Model Fit"), 
       col=c("blue", "red", "darkgreen"), pch=c(19, 17, NA), lty=c(NA, NA, 1))


###################################
## Predicting fuel load use known equation using Trollope equation

# Apply the model
Fdata$predicted_y <- -3019 + 2260 * Fdata$x

# Save to a new CSV if needed
write.csv(data, "fuel_load_linear_predictions.csv", row.names = FALSE)



############################### USING ZAMBATIS EQUATION


# 1. Loading data

FZdata <- read.csv("C:/workspace/gumbo_dev/DATA/March2025/Fuelload .csv")  

# 2. Apply equation only for x <= 26
# Create a new column for predicted y (initialize with NA)
FZdata$y_pred <- NA

# Apply the equation only to rows where x <= 26
# Apply the formula only for rows where DPM_Height <= 26
FZdata$y_pred <- NA  # Create or clear the prediction column

FZdata$y_pred[FZdata$DPM_Height <= 26] <- (
  31.7176 * (0.32186^(1 / FZdata$DPM_Height[FZdata$DPM_Height <= 26])) *
    FZdata$DPM_Height[FZdata$DPM_Height <= 26]^0.2834
)^2


### for BOTH EQUATIONS AS USED BY ZAMBATIS ET AL
# Start fresh
FZdata$y_pred <- NA  # Initialize prediction column

# Apply first equation where DPM_Height <= 26
FZdata$y_pred[FZdata$DPM_Height <= 26] <- (
  31.7176 * (0.32186^(1 / FZdata$DPM_Height[FZdata$DPM_Height <= 26])) *
    FZdata$DPM_Height[FZdata$DPM_Height <= 26]^0.2834
)^2

# Apply second equation where DPM_Height > 26
FZdata$y_pred[FZdata$DPM_Height > 26] <- (
  17.3543 * (0.9893^FZdata$DPM_Height[FZdata$DPM_Height > 26]) *
    FZdata$DPM_Height[FZdata$DPM_Height > 26]^0.5413
)^2

# View results
head(FZdata)

# Optional: Save the full sheet with predictions
# write.csv(FZdata, "Fuel_load_predictions.csv", row.names = FALSE)

####################################################################################
## VISUALIZATION OF THE GRASS BIOMASS

GBdata <- read_csv("C:/workspace/gumbo_dev/DATA/March2025/Biomass_estimated.csv")

# Clean and format, FILTERING NAs

GBdata <- GBdata %>%
  filter(!is.na(Biomass)) %>%  # Exclude NAs in dpm_height
  mutate(
    Year = as.factor(Year),
    Site = as.factor(Site),
    Plot = as.factor(Plot),
    Subplot = as.factor(Subplot),
    Treatment = as.factor(Treatment),
    Biomass = as.numeric(Biomass) # Ensure numeric
  )

# Check missing values
summary(GBdata$Biomass)

###2. Create Pre/Post Variable

GBdata <- GBdata %>%
  mutate(period = ifelse(as.numeric(as.character(Year)) < 2025, "Pre_treatment", "Post_treatment")) %>%
  mutate(period = factor(period, levels = c("Pre_treatment", "Post_treatment")))

### Summarize by Subplot or Plot..Average biomass per subplot and period:

summary_GBdata <- GBdata %>%
  group_by(Site, Plot, Subplot, Treatment, period) %>%
  summarise(mean_Biomass = mean(Biomass, na.rm = TRUE)) %>%
  ungroup()

####4. Calculate Change in biomass (Post - Pre)
biomass_change <- summary_GBdata %>%
  pivot_wider(names_from = period, values_from = mean_Biomass) %>%
  mutate(delta = Post_treatment - Pre_treatment)

### Compare Treatment Effects (ANOVA on Delta)
anova_model <- aov(delta ~ Treatment, data = biomass_change)
summary(anova_model)
# the results show that the treatment groups do not differ significantly in their effect on the response variable.

## Optional: Tukey post-hoc test
TukeyHSD(anova_model)


## USE OF Mixed-Effects Model (Handles Repeated Measures)
#This is more robust as it uses the original data, accounts for plot/subplot as random effects, and tests interaction between time and treatment:
# Look at the interaction terms (periodPOST:treatmentX) to assess whether any treatment caused a significant change post-treatment.

##### using LMM 
model <-lmer(Biomass ~ period * Treatment + (1 | Site/Plot/Subplot), data = GBdata)

summary(model)

##Plotting boxplot for before and after treatment
Bg1 <- ggplot(GBdata, aes(x = Treatment, y = Biomass, fill = period)) +
  geom_boxplot() +
  theme_beautiful() + labs(y = "Biomass (kg/ha)", x = "Period") 


#7. Visualization (Optional)

Bg2 <- ggplot(GBdata, aes(x = period, y = Biomass, color = Treatment)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.3)) +
  stat_summary(fun = mean, geom = "line", aes(group = Treatment), position = position_dodge(width = 0.3)) +
  labs(y = "Biomass (kg/ha)", x = "Period")+ theme_beautiful()



  # Saving as png
  ggsave(Bg1,
         filename = "C:/Plots/Biomass1.png",
         width = 16, height = 14, units = "cm" ) 
  
  
###########################################################################################
   ### ABSOLUTE GRASS BIOMASS
  
  # Filter for 2024 and 2025
  grass_data <- GBdata %>%
    filter(Year %in% c(2024, 2025))
  
## Step 3: Boxplot or summary of grass height by treatment and year
  ggplot(grass_data, aes(x = Treatment, y = Biomass, fill = factor(Year))) +
    geom_boxplot() +
    labs(#title = "Grass Biomass by Treatment (2024 vs 2025)",
         x = "Treatments",
         y = "Biomass (kg/ha)",
         fill = "Year") +
    theme_beautiful()
  
  ##Running statistical tests
  grass_data <- grass_data %>%
    mutate(
      Treatment = as.factor(Treatment),
      Year = as.factor(Year))
  
  # ANOVA model
  model <- aov(Biomass ~ Year * Treatment, data = grass_data)
  summary(model)
  
# 1. Post-hoc test to see which treatments differ
  #Use emmeans (estimated marginal means) for pairwise comparisons:
emmeans_model <- emmeans(model, ~ Treatment)
  pairs(emmeans_model)
  
  
##2. Visualize grass height across treatments
  ggplot(grass_data, aes(x = Treatment, y = Biomass, fill = Treatment)) +
    geom_boxplot() +
    facet_wrap(~ Year) +
    theme_beautiful() +
    labs(#title = "Grass Height by Treatment in 2024 and 2025",
         y = "Grass Biomass (kg/ha)")


########################################################################################
#### DELTA (RELATIVE DIFFERENCE)
  
  # Calculate delta (change in biomass)
  GBdata <- GBdata %>%
  mutate(delta = Post_treatment - Pre_treatment)

  # Boxplot of delta by treatment
  DB <- ggplot(biomass_change, aes(x = Treatment, y = delta)) +
    geom_boxplot() +
    labs(y = "Delta Biomass (kg/ha)", x = "Treatments") +
    theme_beautiful()   
  
  # Saving as png
  ggsave(DB,filename ="C:/workspace/gumbo_dev/Plots/Delta Biomass.png",
         width = 16, height = 14, units = "cm" )
  

########################################################################################
  
# enter data
DPMdata <- read_csv("C:/workspace/gumbo_dev/DATA/DPM .csv")


  
  # Apply square-root transformation to DPH column
  DPMdata$sqrt_DPH_Height <- sqrt(DPMdata$DPH_Height)
  
  frame_area <- pi * (0.17^2)  # = 0.0908 m²
  
  DPMdata$sqrt_DPH_Height <- DPMdata$DPH_Height *10/ frame_area
  
  # Fit the linear model: biomass ~ sqrt(DPH)
  model2 <- lm(Weight ~ sqrt_DPH_Height, data = DPMdata)
  
  # Summarize the model
  summary(model2)
  
  # Plot the data and regression line
  plot(DPMdata$sqrt_DPH_Height, DPMdata$Weight,
       main = "Regression: Biomass ~ sqrt(DPH)",
       xlab = "Square Root of DPH (cm)", ylab = "Oven-dried biomass (kg/ha)",
       pch = 16, col = "darkgreen")
  abline(model, col = "red", lwd = 2)

  
  ################## DPM HEIGHT ~ OVEN DRIED WEIGHT. NO SQUARE ROOT
# 2. Convert weight from grams to kg/ha
#    Area of 34cm diameter disc = π * (0.17 m)^2 = 0.0908 m²
  frame_area <- pi * (0.17^2)  # = 0.0908 m²
  DPMdata$Biomass_kg_ha <- DPMdata$Weight * 10 / frame_area
  
  # 3. Linear regression: Biomass ~ DPH
  model <- lm(Biomass_kg_ha ~ DPH_Height, data = DPMdata)
  
  ## square rooting DPH
  # Fit a model with square-root transformed DPH
  model_sqrt <- lm(Biomass_kg_ha ~ sqrt(DPH_Height), data = DPMdata)
  summary(model_sqrt)
  
  
    
  # 4. View regression summary
  summary(model)
  
  # 5. Plot with regression line
  plot(DPMdata$DPH_Height, DPMdata$Biomass_kg_ha,
     
       xlab = "DPM height(cm)", ylab = "Biomass (kg/ha)",
       pch = 16, col = "blue")
  abline(model, col = "red", lwd = 2)+ theme_beautiful()
  
  
  
#################################################################
###########square rooting DPM height
Avdata <- read_csv("C:/workspace/gumbo_dev/DATA/DPM .csv")  
  
  # Step 1: Ensure data is sorted as needed (optional)
Avdata <- Avdata[order(Avdata$DPH_Height), ]  # Sort by DPH if needed
  
  # Step 2: Average DPH values in pairs
avg_dph <- tapply(Avdata$DPH_Height, (seq_along(Avdata$DPH_Height) + 1) %/% 2, mean)
  
  # Step 3: Square-root transformation of the averaged DPH
sqrt_avg_dph <- sqrt(avg_dph)
  
  # Step 4: Fit model with square-root of original DPH values
Avdata$sqrt_DPH_Height <- sqrt(Avdata$DPH_Height)
  modelB <- lm(Weight ~ sqrt_DPH_Height, data = Avdata)
  
  # Step 5: Predict biomass using the averaged & transformed DPH values
  predicted_biomass <- predict(modelB, newdata = data.frame(sqrt_DPH_Height = sqrt_avg_dph))
  
  summary(modelB)
  
  # Step 6: Combine results into a dataframe
  result <- data.frame(
    Pair = seq_along(avg_dph),
    Avg_DPH = round(avg_dph, 2),
    sqrt_DPH = round(sqrt_avg_dph, 2),
    Predicted_Biomass_kg_ha = round(predicted_biomass, 2)
  )
  
  # View result
  print(result)
  