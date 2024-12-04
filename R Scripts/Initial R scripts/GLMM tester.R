library(tidyverse)
install.packages("lme4")
install.packages("MuMIn")

library(lme4)
library(MuMIn)

dataW <- read_csv("C:/Users/tg488/OneDrive - University of Exeter/Project developement - Taps/Woody_P_Tester_2.csv")

###Ensure columns are properly formatted
dataW$Site <- as.factor(dataW$Site)
dataW$Box <- as.factor(dataW$Box)
dataW$Subbox <- as.factor(dataW$Subbox)
dataW$Spp_name <- as.factor(dataW$Spp_name)


install.packages("Matrix", dependencies = TRUE)

update.packages(ask = FALSE)

install.packages("glmmTMB")
library(glmmTMB)


model_height <- glmmTMB(Max_height ~ Spp_name + (1 | Site/Box/Subbox), data = dataW, family = gaussian)
summary(model_height)
str(data)





#Model 1: GLMM for Max_height (assuming Gaussian distribution)
model_height <- lmer(Max_height ~ Spp_name + (1 | Site/Box/Subbox), data = dataW)

# Model 2: GLMM for Stem_count (assuming Poisson distribution)
model_stem <- glmer(Stem_count ~ Spp_name + (1 | Site/Box/Subbox), family = "poisson", data = dataW)

# Summarize results
summary(model_height)
summary(model_stem)