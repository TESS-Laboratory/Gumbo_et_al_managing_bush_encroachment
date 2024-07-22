library(tidyverse)
Woody <- Woody[2:nrow(Woody), ]
Woody_Plants<- Woody_Plants[2:nrow(Woody_Plants), ]
view(Woody_Plants)

Woody_Plants <- Woody_Plants %>%
group_by(Site,) %>%
count(Spp_name)
