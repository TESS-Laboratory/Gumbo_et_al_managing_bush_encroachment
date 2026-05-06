# CHIRPS
library(tidyverse)
library(chirps)
library(terra)
library(nasapower)

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
        linetype = "blank")
    )
}



# 1. Load the global monthly CHIRPS NetCDF
# Load only the 'precip' variable
chirps_global <- rast("DATA/chirps-v2.0.monthly.nc", subds = "precip")


# Crop to Zimbabwe
# Zimbabwe bounding box
zimbabwe <- ext(25.2, 33.1, -22.5, -15.6)
chirps_zim <- crop(chirps_global, zimbabwe)

# Define SHR
SHHR <- vect(data.frame(lon=29.32, lat=-19.71), geom=c("lon","lat"), crs="EPSG:4326")

# Extract rainfall for SHR
rain_values <- extract(chirps_zim, SHHR, fun=mean, na.rm=TRUE)


# Subset raster - Calculating which layers correspond to 2000–2024
chirps_2000_2024 <- chirps_global[[228:527]]
nlyr(chirps_2000_2024)  # should return 300

# define matching dates
dates <- seq(as.Date("2000-01-01"), as.Date("2024-12-01"), by = "month")
length(dates)  # 300 → matches raster

# extract rainfall for SHR
# Extract returns a data frame with first column = ID
rain_df <- extract(chirps_2000_2024, SHHR)

# Convert rainfall to tidy format
rain_df <- rain_df %>%
  select(starts_with("precip_")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "layer",
    values_to = "rain_mm"
  ) %>%
  mutate(date = dates) %>%
  select(date, rain_mm)

# tidy dataframe

rain_df <- rain_df %>%
  mutate(
    YEAR  = year(date),
    month = month(date, label = TRUE, abbr = TRUE)
  ) %>%
  select(YEAR, month, date, rain_mm) %>%
  arrange(date)


# Create monthly averages
monthly_avg <- rain_df %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
  group_by(month) %>%
  summarise(avg_monthly_rain = mean(rain_mm, na.rm = TRUE))

# ggplot
Rain <- ggplot(monthly_avg, aes(x = month, y = avg_monthly_rain)) +
  geom_col(fill = "steelblue") +
  labs(
       x = "Month", y = "Mean monthly rainfall (mm)") +
  theme_beautiful()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

# saving plot
ggsave(Rain,filename ="Plots/RainfallSHR.png",
       width = 16, height = 14, units = "cm")  


#################################################################################
######################   TEMPERATURE RANGE (Tmax, Tmin)
# Downloading NASA power temperature
temp_power <- get_power(
  community = "AG",
  lonlat    = c(29.32, -19.71),
  pars      = c("T2M", "T2M_MAX", "T2M_MIN"),
  temporal_api = "MONTHLY",
  dates     = c(200001, 202412)
)


# build temp df
month_lookup <- c(
  JAN = 1, FEB = 2, MAR = 3, APR = 4,
  MAY = 5, JUN = 6, JUL = 7, AUG = 8,
  SEP = 9, OCT = 10, NOV = 11, DEC = 12
)

# building temp df with mean, tmax, etc
temp_df <- temp_power %>%
  select(YEAR, JAN:DEC, PARAMETER) %>%
  pivot_longer(
    cols = JAN:DEC,
    names_to = "month",
    values_to = "value"
  ) %>%
  mutate(
    month_num = month_lookup[month],
    date = as.Date(sprintf("%04d-%02d-01", YEAR, month_num))
  ) %>%
  select(date, PARAMETER, value) %>%
  pivot_wider(
    names_from  = PARAMETER,
    values_from = value
  ) %>%
  rename(
    Tmean = T2M,
    Tmax  = T2M_MAX,
    Tmin  = T2M_MIN
  ) %>%
  #mutate(
   # DTR = Tmax - Tmin
  #) %>%
  arrange(date)

# use rainfall data

rain_df <- rain_df %>%
  mutate(
    YEAR  = year(date),
    month = month(date, label = TRUE, abbr = TRUE)
  ) %>%
  select(YEAR, month, date, rain_mm) %>%
  arrange(date)


# combine rainfall and temp
climate_df <- rain_df %>%
  inner_join(temp_df, by = "date") %>%
  mutate(
    month = month(date, label = TRUE, abbr = TRUE)
  )


# compute monthly climatology
climate_monthly <- climate_df %>%
  group_by(month) %>%
  summarise(
    rain_mm = mean(rain_mm, na.rm = TRUE),
    Tmean   = mean(Tmean, na.rm = TRUE),
    Tmax    = mean(Tmax, na.rm = TRUE),
    Tmin    = mean(Tmin, na.rm = TRUE),
    #DTR     = mean(DTR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    month = factor(
      month,
      levels = c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")
    ))
  

## Convert temperature columns to long format
climate_long <- climate_monthly %>%
  pivot_longer(cols = c(Tmean, Tmax, Tmin),  # c(Tmean, Tmax, Tmin, DTR) if including it
               names_to = "temp_var",
               values_to = "temp_value")


# scale factor
# Determine scaling factor
scale_factor <- max(climate_monthly$rain_mm, na.rm = TRUE) / max(climate_long$temp_value, na.rm = TRUE)

# creating a plot using ggplot
combtr <- ggplot() +
  # Rainfall as bars
  geom_col(data = climate_monthly, aes(x = month, y = rain_mm, fill = "Rainfall"),
           width = 0.7, alpha = 0.8) +
  
  # Temperature lines
  geom_line(data = climate_long,
            aes(x = month, y = temp_value * scale_factor, color = temp_var, group = temp_var),
            linewidth = 1.2) +
  geom_point(data = climate_long,
             aes(x = month, y = temp_value * scale_factor, color = temp_var),
             size = 2.0) +
  # Dual y-axis
  scale_y_continuous(
    name = "Mean monthly rainfall (mm)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Temperature (°C)")
  ) +
  
  # Colour and fill
  scale_fill_manual(values = c("Rainfall" = "steelblue"), name = NULL) +
  scale_color_manual(values = c(
    "Tmean" = "black",
    "Tmax"  = "red",
    "Tmin"  = "brown"
  ), name = NULL) +
  
  labs(
    x = "Month"
  ) +
  theme_beautiful() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    axis.title.y.left = element_text(color = "black", size = 12, angle = 90, vjust = 0.5),
    axis.title.y.right = element_text(color = "black", size = 12, angle = 90, vjust = 0.5),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

# saving plot
ggsave(combtr, filename = "Plots/SHRClimate2.png", width = 16, height = 12, units = "cm" )




#################################################
################# Calculating average annual temp and rainfall from 2000 to 2024

# Step 1: Add YEAR column
climate_df <- climate_df %>%
  mutate(YEAR = year(date))

# Step 2: Compute annual totals/averages
annual_climate <- climate_df %>%
  group_by(YEAR) %>%
  summarise(
    annual_rain_mm = sum(rain_mm, na.rm = TRUE),  # total rainfall per year
    annual_Tmean   = mean(Tmean, na.rm = TRUE),   # mean temperature per year
    annual_Tmax    = mean(Tmax, na.rm = TRUE),
    annual_Tmin    = mean(Tmin, na.rm = TRUE),
    annual_DTR     = mean(DTR,  na.rm = TRUE),
    .groups = "drop"
  )

# Step 3: Compute average across all years
overall_annual_avg <- annual_climate %>%
  summarise(
    mean_annual_rain_mm = mean(annual_rain_mm, na.rm = TRUE),
    mean_annual_Tmean   = mean(annual_Tmean, na.rm = TRUE),
    mean_annual_Tmax    = mean(annual_Tmax, na.rm = TRUE),
    mean_annual_Tmin    = mean(annual_Tmin, na.rm = TRUE),
    mean_annual_DTR     = mean(annual_DTR,  na.rm = TRUE)
  )

overall_annual_avg




###################################################################

## CALCULATING MEAN ANNUAL RAINFALL AND TEMPERATURE

# Reorder 
climate_df <- climate_df %>%
  mutate(YEAR = year(date))


# compute annual metrics
annual_climate <- climate_df %>%
  mutate(YEAR = year(date)) %>%
  group_by(YEAR) %>%
  summarise(
    rain_mm_annual = sum(rain_mm, na.rm = TRUE),   # total annual rainfall
    Tmean_annual   = mean(Tmean, na.rm = TRUE),    # mean of monthly means
    Tmax_annual    = mean(Tmax, na.rm = TRUE),
    Tmin_annual    = mean(Tmin, na.rm = TRUE),
    DTR_annual     = mean(DTR,  na.rm = TRUE),
    .groups = "drop"
  )

# coverting temperature variable 

annual_long <- annual_climate %>%
  pivot_longer(
    cols = c(Tmean_annual, Tmax_annual, Tmin_annual, DTR_annual),
    names_to = "temp_var",
    values_to = "temp_value"
  )

#scaling factor for plotting
scale_factor <- max(annual_climate$rain_mm_annual, na.rm = TRUE) / max(annual_long$temp_value, na.rm = TRUE)

# plot annual temp and rainfall
yearly <- ggplot() +
  # Rainfall as bars
  geom_col(data = annual_climate, aes(x = YEAR, y = rain_mm_annual, fill = "Rainfall"),
           width = 0.7, alpha = 0.8) +
  
  # Temperature lines
  geom_line(data = annual_long,
            aes(x = YEAR, y = temp_value * scale_factor, color = temp_var, group = temp_var),
            linewidth = 1.2) +
  geom_point(data = annual_long,
             aes(x = YEAR, y = temp_value * scale_factor, color = temp_var),
             size = 2) +
  
  # Dual y-axis
  scale_y_continuous(
    name = "Mean annual rainfall (mm)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Temperature (°C)")
  ) +
  # Colors for rainfall and temperatures
  scale_fill_manual(values = c("Rainfall" = "steelblue"), name = NULL) +
  scale_color_manual(values = c(
    "Tmean_annual" = "black",
    "Tmax_annual"  = "red",
    "Tmin_annual"  = "grey",
    "DTR_annual"   = "darkgreen"
  ), name = NULL) +
  
  labs(
    x = "Year"
  ) +
  
  theme_beautiful() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    axis.title.y.left = element_text(color = "black", size = 12, angle = 90, vjust = 0.5),
    axis.title.y.right = element_text(color = "black", size = 12, angle = 90, vjust = 0.5),
    legend.position = "top",
    legend.text = element_text(size = 10)
  )

##save plot

#ggsave(yearly, filename = "Plots/AnnualTEMPRAIN.png", width = 16, height = 12, units = "cm")