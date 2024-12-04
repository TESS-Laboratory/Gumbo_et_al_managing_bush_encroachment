library(tidyverse)
attach(Results)
head(Results)
####Plotting graphs for woody plants
ggplot(Results) +
  geom_col(aes(x = SITE, y = Stem_count), fill = "darkgrey", colour = "black", width = 0.75) +
  labs(y = "Total number of stems") + theme_classic() + 
  theme(axis.title.x = element_text(size = 14), 
    axis.title.y = element_text(size = 14), 
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12))
######
ggplot(Results) +
  geom_col(aes(x = Site, y = Trees), fill = "grey", colour = "black", width = 0.75) +
  labs(y = "Total number of trees") + theme_classic()
####
ggplot(Results) +
  geom_col(aes(x = SITE, y = Mean_height_of_trees), fill = "darkgrey", colour = "black", width = 0.75) +
  labs(y = "Mean tree height") + theme_classic() + 
  theme(axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12))

####
ggplot(Results) + geom_col(aes(x = Site, y = Mean_stems), fill = "grey", colour = "black", width = 0.75) +
  labs(y = "Mean stem count per plant") + theme_classic()
####
ggplot(Results) + geom_col(aes(x = Site, y = Seedlings), fill = "grey", colour = "black", width = 0.75) +
  labs(y = "Seedlings population") + theme_classic()
### seedling height
ggplot(Results) + geom_col(aes(x = Site, y = Mean_seedlings_height), fill = "grey", colour = "black", width = 0.75) +
  labs(y = "Average seedlings height") + theme_classic()
#### saplings
ggplot(Results) + geom_col(aes(x = Site, y = Saplings), fill = "grey", colour = "black", width = 0.75) +
  labs(y = "Saplings population") + theme_classic()
#### mean saplings 
ggplot(Results) + geom_col(aes(x = Site, y = Mean_saplings_height), fill = "grey", colour = "black", width = 0.75) +
  labs(y = "Average saplings height") + theme_classic()

############################ plotting graph with multiple variables

