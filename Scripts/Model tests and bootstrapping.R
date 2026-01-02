library(robustlmm)
mod_robust <- rlmer(density_ha ~ Treatment * Fencing  + (1 | Site/Plot),
                    data = trt_comparison2)

summary(mod_robust)

VarCorr(mod_robust)

Treeden3 <- glmmTMB(density_ha ~ Treatment * Fencing  + (1 | Site) + (1|Plot), 
                family = t_family(), data = trt_comparison2)

summary(Treeden3)

#nesting random effects
Treeden4 <- glmmTMB(density_ha ~ Treatment * Fencing  + (1 | Site/Plot), 
                    family = t_family(), data = trt_comparison2,
                    control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

summary(Treeden3)



##scaling the response variable. Large numbers make optimization unstable. 
trt_comparison2$DeltaDensity_scaled <- scale(trt_comparison2$density_ha)

mod_t <- glmmTMB(
  DeltaDensity_scaled ~ Treatment*Fencing + (1|Site) + (1|Plot),
  family = t_family(),
  data = trt_comparison2
)

summary(mod_t)


# scaled with one RE dropped


mod_t2 <- glmmTMB(
  DeltaDensity_scaled ~ Treatment*Fencing + (1|Site),
  family = t_family(),
  data = trt_comparison2
)

summary(mod_t2)

performance::check_model(mod_t2)


##Check for outliers
residuals_std <- scale(resid(mod_t2))  # Standardize residuals
outliers <- which(abs(residuals_std) > 3)  # Find outliers
print(mod_t2)


## Shapiro-Wilk Test for Normal distribution of residuals
shapiro.test(resid(mod_t2))  #p < 0.05 → residuals are non-normal.
# result p = 0.001554, hence residuals are non-normal

##B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(mod_t), breaks = 5)
leveneTest(resid(mod_t) ~ fitted_binned)  # p < 0.05 → unequal variance (heteroscedasticity)
# results p = 0.0569 equal  variance 




Treeden22 <- glmmTMB(density_ha ~ Treatment * Fencing  + (1 | Site),
                   data = trt_comparison2, family = gaussian(link = "identity"))



VarCorr(Treeden22)


##### histogram for residuals in Treedens

# Extract residuals from your model
model_residuals <- resid(Modtden)

# Create a combined plot layout to view them side-by-side
par(mfrow = c(1, 2)) # This sets the plotting area to 1 row, 2 columns

# 1. HISTOGRAM with a normal curve overlay
hist(model_residuals,
     main = "Histogram of Residuals",
     xlab = "Residual Value",
     ylab = "Frequency",
     col = "lightblue",
     probability = TRUE) # 'probability = TRUE' allows the density curve to overlay

# Overlay a normal distribution curve for comparison
x <- seq(min(model_residuals), max(model_residuals), length = 100)
y <- dnorm(x, mean = mean(model_residuals), sd = sd(model_residuals))
lines(x, y, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = "Normal Curve", col = "red", lwd = 2, bty = "n")

# 2. DENSITY PLOT with a normal curve overlay
plot(density(model_residuals),
     main = "Density Plot of Residuals",
     xlab = "Residual Value",
     ylab = "Density",
     col = "blue",
     lwd = 2)

# Overlay a normal distribution curve for comparison
curve(dnorm(x, mean = mean(model_residuals), sd = sd(model_residuals)),
      col = "red",
      lwd = 2,
      lty = 2, # Makes the line dashed
      add = TRUE)

# Add a legend
legend("topright", 
       legend = c("Residual Density", "Normal Curve"),
       col = c("blue", "red"),
       lwd = 2,
       lty = c(1, 2), # lty 1 is solid, 2 is dashed
       bty = "n")

# Reset the plotting layout to default (1 plot per window)
par(mfrow = c(1, 1))



## Trying other ways to deal with non-normality
# This plot is crucial
plot(fitted(TreedenMod), resid(TreedenMod),
     main = "Residuals vs. Fitted",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)
 #Results : For Non-linearity (U-shaped curve): The points should be randomly scattered 
 #around the red line.If you see a clear pattern (e.g., a U-shape or curve), it 
 #means your model is missing a non-linear relationship. This will cause non-normal residuals.

# Also, create a Scale-Location plot for checking heteroscedasticity
plot(fitted(TreedenMod), sqrt(abs(resid(TreedenMod))),
     main = "Scale-Location Plot",
     xlab = "Fitted Values", ylab = "sqrt(|Residuals|)")
 #For heteroscedasticity (Funnel shape): The spread of the residuals should be constant 
  #across all fitted values. If the spread fans out or narrows (looks like a funnel), 
#you have heteroscedasticity. This also causes non-normality.


##2. QQ-Plot with Confidence Intervals:
 #This gives you a better visual than a histogram to see where the distribution deviates from normality (e.g., in the tails vs. the center).

qqnorm(resid(TreedenMod))
qqline(resid(TreedenMod), col = "red")
# For a better plot with confidence intervals, use 'car' package
qqPlot(resid(TreedenMod))


#Using robust regression

library(MASS)

Mod_robust <- rlm(density_ha ~ Treatment * Fencing, data = trt_comparison2) # Robust linear model
# Check the residuals of THIS model
qqPlot(resid(Mod_robust))
shapiro.test(resid(Mod_robust)) # p=0.0041 hence non-norma distribution

#B. Levene’s Test for Homoscedasticity
# Bin fitted values into 3-5 groups
fitted_binned <- cut(fitted(Mod_robust), breaks = 5)
leveneTest(resid(Mod_robust) ~ fitted_binned) # p = 0.091 equal variance

### MODEL performance
performance::check_model(Mod_robust)



###BOOTSTRAPPING THE DATA

# Fit your original model
lmerTrden <- lmer(density_ha ~ Treatment * Fencing  + (1 | Site), data = trt_comparison2)

# For convergence diagnostics:
performance::check_convergence(lmerTrden) # TRUE the model converged

# Define a function to extract the statistics you want
# Common choices: fixef for fixed effects, sigma for residual SD
my_stat <- function(m) {
  return(fixef(m)) # Return fixed effects coefficients
}

# Run the parametric bootstrap
boot_results <- bootMer(lmerTrden,
                        FUN = my_stat,
                        nsim = 1000, # Number of bootstrap simulations
                        type = "parametric") # The key argument

# View bootstrap results
print(boot_results)
# Calculate bootstrap confidence intervals
boot.ci(boot_results, type = "perc", index = 2) # CI for the 2nd coefficient (Days)



####BOOTSTRAPPING PART 2 

 #library(boot)

# Assuming your model is called 'my_model'
# Fit your original model
lmerTrden2 <- lmer(density_ha ~ Treatment * Fencing  + (1 | Site), data = trt_comparison2)

# Run bootstrap for fixed effects
boot_results <- bootMer(lmerTrden2,
                        FUN = function(m) fixef(m),
                        nsim = 1000,
                        type = "parametric")

# Check the names of the coefficients to know your 'index'
print(boot_results$t0)
# This will print something like:
# (Intercept)   TreatmentB
# 105.5         -800.0

# Let's say 'TreatmentB' is the second coefficient (index = 2)
# Get CI for the effect of Treatment B vs. Treatment A (the intercept)
boot.ci(boot_results, type = "perc", index = 2)
boot.ci(boot_results, type = "perc", index = 3)
boot.ci(boot_results, type = "perc", index = 4)
boot.ci(boot_results, type = "perc", index = 5)

# Now, create a boxplot to visualize the deltas for each treatment
# This helps ground the statistical result in the actual data
boxplot(density_ha ~ Treatment, data = trt_comparison2,
        main = "Effect of Treatment on Change in Tree Density",
        xlab = "Treatment",
        ylab = "Δ tree Density",
        col = "lightgreen")
# Add a horizontal line at zero for reference (no change)
abline(h = 0, lty = 2, col = "red")



##### Boostrapping to show all coefficients at once 
# boot_results$t is a matrix with nsim rows and ncoefficients columns.

# 1. Get the number of coefficients
num_coef <- length(boot_results$t0)

# 2. Create a matrix to store the results
results_table <- matrix(NA, nrow = num_coef, ncol = 3)
colnames(results_table) <- c("Coefficient", "2.5%", "97.5%")
rownames(results_table) <- names(boot_results$t0) # This gets the coefficient names

# 3. Loop through each coefficient and calculate the 2.5th and 97.5th percentiles
for (i in 1:num_coef) {
  boot_samples <- boot_results$t[, i]
  ci <- quantile(boot_samples, probs = c(0.025, 0.975), na.rm = TRUE)
  results_table[i, ] <- c(names(boot_results$t0)[i], ci)
}

# 4. View the beautiful results table for ALL coefficients
print(results_table, quote = FALSE) 



#################
###################### BOOTSTRAPPING FOR NON-PARAMETRIC DATA#####################################

# Case Bootstrap Function (resampling Locations)
case_boot_function <- function(trt_comparison2, indices) {
  # 1. Create the bootstrap sample by resampling Locations
  # This finds all data points belonging to the resampled locations
  boot_data <- NULL
  for (i in unique(indices)) {
    Site_id <- unique(trt_comparison2$Site)[i]
    Site_data <- dplyr::filter(trt_comparison2, Site == Site_id)
    boot_data <- rbind(boot_data, Site_data)
  }

# Fit the initial model to get the structure

lmerTrden3 <- lmer(density_ha ~ Treatment * Fencing  + (1 | Site), data = trt_comparison2)

# Run the bootstrap (R = number of bootstrap simulations)
# Use enough to get stable results (1000-10000). Start with 1000 for testing.
set.seed(123) # For reproducibility
boot_results <- boot(
  data = trt_comparison2,
  statistic = case_boot_function,
  R = 1000,
  parallel = "multicore", # Speeds it up significantly
  ncpus = parallel::detectCores() - 1 # Use all but one CPU core
)

# Print the results
print(boot_results)



  