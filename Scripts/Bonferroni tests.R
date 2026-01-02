# Assuming your model is named 'lmer_model'
library(emmeans)

# Calculate estimated marginal means for the Treatment*Fencing interaction
emm_interaction <- emmeans(Seedl3, ~ Treatment * Fencing) # seedling density
summary(emm_interaction)

# Create a list of contrasts specifically for the fenced group
my_contrasts <- list(
  "TFB vs C (Unfenced)"  = c(0, 1, 0, 0, 0,  0, -1, 0, 0, 0),
  "TFB vs F (Unfenced)" = c(0, 0, 1, 0, 0,  0, 0, -1, 0, 0),
  "TFB vs TF (Unfenced)"= c(0, 0, 0, 1, 0,  0, 0, 0, -1, 0),
  "TFB vs THF (Unfenced)"= c(0, 0, 0, 0, 1,  0, 0, 0, 0, -1)
)

# Run the contrasts with Bonferroni correction
contrast_results <- contrast(emm_interaction, method = my_contrasts, adjust = "bonferroni")

# View the results
print(contrast_results)


############# BENFERRONI SAPLING HEIGHT

emm_interactionSH <- emmeans(SapH1, ~ Treatment * Fencing) 
summary(emm_interactionSH)

# Create a list of contrasts specifically for the fenced group
my_contrasts <- list(
  "THF vs TF (Unfenced)"  = c(0, 1, 0, 0, 0,  0, -1, 0, 0, 0),
  "THF vs TFB (Unfenced)" = c(0, 0, 1, 0, 0,  0, 0, -1, 0, 0),
  "THF vs F (Unfenced)"= c(0, 0, 0, 1, 0,  0, 0, 0, -1, 0),
  "THF vs C (Unfenced)"= c(0, 0, 0, 0, 1,  0, 0, 0, 0, -1)
)

# Run the contrasts with Bonferroni correction
contrast_resultsSH <- contrast(emm_interactionSH, method = my_contrasts, adjust = "bonferroni")

# View the results
print(contrast_resultsSH)


##BONFERRONI CORRECTION FOR GRASS HEIGHT

emm_interactionGH <- emmeans(GrasH1, ~ Treatment * Fencing) 
summary(emm_interactionGH)

# Create a list of contrasts specifically for the fenced group
my_contrasts <- list(
  "THF vs C (Fenced)"  = c(0, 1, 0, 0, 0,  0, -1, 0, 0, 0),
  "THF vs TF (Fenced)"  = c(0, 1, 0, 0, 0,  0, -1, 0, 0, 0),
  "THF vs TFB (Fenced)" = c(0, 0, 1, 0, 0,  0, 0, -1, 0, 0),
  "THF vs F (Fenced)"= c(0, 0, 0, 1, 0,  0, 0, 0, -1, 0)
)

# Run the contrasts with Bonferroni correction
contrast_resultsGH <- contrast(emm_interactionGH, method = my_contrasts, adjust = "bonferroni")

# View the results
print(contrast_resultsGH)



