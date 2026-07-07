##  BROWSE PRESSURE
## browse pressure to determine how much each treatment consumed at each plot

library(tidyverse)
library(dplyr)
# Function for short-term browse pressure (hours instead of days)
calculate_short_browse_pressure <- function(num_goats,
                                            avg_weight_kg = 30,
                                            intake_rate_percent = 3,   # Daily intake as % of BW
                                            time_hours = 5,
                                            plot_area_ha = 0.24) {
  
  # Daily DM per goat
  daily_dm_per_goat <- (intake_rate_percent / 100) * avg_weight_kg
  
  # Fraction of day
  fraction_day <- time_hours / 24
  
  # Total DM consumed
  total_dm_consumed <- num_goats * daily_dm_per_goat * fraction_day
  
  # Browse pressure
  browse_pressure <- total_dm_consumed / plot_area_ha
  
  list(
    parameters = list(
      num_goats = num_goats,
      avg_weight_kg = avg_weight_kg,
      intake_rate_percent = intake_rate_percent,
      time_hours = time_hours,
      plot_area_ha = plot_area_ha
    ),
    daily_dm_per_goat_kg = round(daily_dm_per_goat, 3),
    total_dm_consumed_kg = round(total_dm_consumed, 3),
    browse_pressure_kg_per_ha = round(browse_pressure, 2),
    interpretation = paste("Browse pressure for", time_hours, "hours:", 
                           round(browse_pressure, 2), "kg DM/ha")
  )
}

# Your specific scenario
result <- calculate_short_browse_pressure(num_goats = 30, 
                                          avg_weight_kg = 30, 
                                          time_hours = 5, 
                                          plot_area_ha = 0.24)

cat(result$interpretation, "\n")
print(result)


########### COMPARING BROWSE PRESSURE FOR MAY and NOVEMBER

# Experiment 2  - NOVEMBER
exp2 <- calculate_short_browse_pressure(
  num_goats = 30,
  avg_weight_kg = 30,
  time_hours = 5,
  plot_area_ha = 0.24
)

# Experiment 1 - MAY
exp1 <- calculate_short_browse_pressure(
  num_goats = 15,
  avg_weight_kg = 30,
  time_hours = 3,
  plot_area_ha = 0.24 # this is provisional  it will change when we use precise geolocated data 
)


## prepare data for stacking 
browse_df <- tibble(
  Experiment = c("Experiment 1", "Experiment 2"),
  Daily_DM_per_goat = c(exp1$daily_dm_per_goat_kg,
                        exp2$daily_dm_per_goat_kg),
  Fraction_of_day = c(3/24, 5/24),
  Goats_per_ha = c(15/0.24, 30/0.24)
) %>%
  mutate(
    Time_scaled_DM = Daily_DM_per_goat * Fraction_of_day,
    Browse_pressure = Time_scaled_DM * Goats_per_ha
  ) %>%
  select(Experiment, Time_scaled_DM, Browse_pressure) %>%
  pivot_longer(-Experiment,
               names_to = "Component",
               values_to = "kg_DM_ha")


# stacked bar plot
ggplot(browse_df,
       aes(x = Experiment,
           y = kg_DM_ha,
           fill = Component)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    y = "Browse pressure (kg DM / ha)",
    x = NULL,
    fill = "Component",
    title = "Comparison of Browse Pressure Between Experiments"
  ) +
  theme_minimal(base_size = 12)









#####################################################################
#################### OPTION B FOR DETERMINING BROWSE PRESSURE USING HOURLY RATE

calculate_hourly_browse_pressure <- function(num_goats,
                                             avg_weight_kg = 30,
                                             intake_rate_percent = 3,
                                             time_hours,
                                             plot_area_ha = 0.24) {
  
  # Daily DM intake per goat (kg)
  daily_dm_per_goat <- (intake_rate_percent / 100) * avg_weight_kg
  
  # Hourly DM intake per goat (kg/hour)
  hourly_dm_per_goat <- daily_dm_per_goat / 24
  
  # Total DM consumed during browsing period
  total_dm_consumed <- num_goats * hourly_dm_per_goat * time_hours
  
  # Browse pressure (kg DM / ha)
  browse_pressure <- total_dm_consumed / plot_area_ha
  
  list(
    parameters = list(
      num_goats = num_goats,
      avg_weight_kg = avg_weight_kg,
      intake_rate_percent = intake_rate_percent,
      time_hours = time_hours,
      plot_area_ha = plot_area_ha
    ),
    hourly_dm_per_goat_kg = round(hourly_dm_per_goat, 3),
    total_dm_consumed_kg = round(total_dm_consumed, 3),
    browse_pressure_kg_per_ha = round(browse_pressure, 2),
    interpretation = paste(
      "Browse pressure for", time_hours, "hours:",
      round(browse_pressure, 2), "kg DM/ha"
    )
  )
}


result2 <- calculate_short_browse_pressure(num_goats = 30, 
                                avg_weight_kg = 30, 
                                time_hours = 5, 
                                plot_area_ha = 0.24)
print(result2)

## Apply to both follow-up treatments
May <- calculate_hourly_browse_pressure(
  num_goats = 15,
  time_hours = 3,
  plot_area_ha = 0.24
)

Nov <- calculate_hourly_browse_pressure(
  num_goats = 30,
  time_hours = 5,
  plot_area_ha = 0.24
)

# preparing data for stacking
browse_df <- tibble(
  Experiment = c("May", "November"),
  Hourly_DM_per_goat = c(May$hourly_dm_per_goat_kg,
                         Nov$hourly_dm_per_goat_kg),
  Hours = c(3, 5),
  Goats_per_ha = c(15 / 0.24, 30 / 0.24)
) %>%
  mutate(
    Time_component = Hourly_DM_per_goat * Hours,
    Browse_pressure = Time_component * Goats_per_ha
  ) %>%
  select(Experiment, Browse_pressure) %>%
  pivot_longer(
    cols = c(Browse_pressure),
    names_to = "Component",
    values_to = "kg_DM_ha"
  )


# stacked bar 
ggplot(browse_df,
       aes(x = Experiment,
           y = kg_DM_ha
          )) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    y = "Browse pressure (kg DM / ha)",
    x = NULL
  ) +
  theme_beautiful()

####################################################################
#########################################################################

# USING ACTUAL PLOT AREA PER SITE

# Read site-level data
sites <- read_csv("DATA/Browser_plot_area.csv")

# Columns: Location, plot_area_ha

# Constants
avg_weight_kg <- 30
intake_rate_percent <- 3

daily_dm_per_goat <- (intake_rate_percent / 100) * avg_weight_kg
hourly_dm_per_goat <- daily_dm_per_goat / 24

browse_by_site <- sites %>%
  mutate(
    # Experiment 1
    May_total_dm = 15 * hourly_dm_per_goat * 3,
    `Initial_treatment_(May)` = May_total_dm / Plot_area_ha,
    
    # Experiment 2
    Nov_total_dm = 30 * hourly_dm_per_goat * 5,
    `Second_treatment_(November)` = Nov_total_dm / Plot_area_ha
  )

###### Shaping for stacked bar plots

plot_df <- browse_by_site %>%
  dplyr::select(Site, `Initial_treatment_(May)`,`Second_treatment_(November)`) %>% 
  pivot_longer(
    cols = c( `Initial_treatment_(May)`,`Second_treatment_(November)`),
    names_to = "Period",
    values_to = "provisional_browse_pressure_kg_ha"
  ) 


plot_df <- browse_by_site %>%
  dplyr::select(Site, contains("Initial"), contains("Second")) %>%
  pivot_longer(
    cols = -Site,
    names_to = "Period",
    values_to = "provisional_browse_pressure_kg_ha"
  )

# stacking order with new labels
plot_df$Period <- factor(
  plot_df$Period,
  levels = c("Initial_treatment_(May)", "Second_treatment_(November)"),
  labels = c("May 2025", "November 2025")
)


## ggplot - stacked bar plots

bpS <- ggplot(plot_df,
              aes(x = Site,
                  y = provisional_browse_pressure_kg_ha,
                  fill = Period)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  labs(y = "Estimated browse pressure ("*kg~ha^{-1}*")",
       x = "Site") +
  theme_classic() + 
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12)  
  )

# saving plot
ggsave(bpS, filename = "Plots/Browsing pressureSITE.png", 
       width = 16, height = 12, units = "cm")
