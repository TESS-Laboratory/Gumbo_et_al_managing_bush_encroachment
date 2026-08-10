# =============================================================================
# Seasonal and Interannual Variability — SHR Climate Context
# Source data: CHIRPS v2.0 monthly NetCDF + NASA POWER
# Study site:  Shangani Holistic Rangeland, Zimbabwe (29.32°E, -19.71°S)
# Panel A:     Seasonal climatology (2000–2024)
# Panels B–C:  Annual precipitation & Gini index (1981–Nov 2025)
# =============================================================================

library(tidyverse)
library(terra)
library(nasapower)
library(patchwork)

# -----------------------------------------------------------------------------
# THEME
# -----------------------------------------------------------------------------

theme_beautiful <- function() {
  theme_bw() +
    theme(
      text              = element_text(family = "Helvetica"),
      axis.text         = element_text(size = 8,  color = "black"),
      axis.title        = element_text(size = 9,  color = "black"),
      axis.line.x       = element_line(linewidth = 0.3, color = "black"),
      axis.line.y       = element_line(linewidth = 0.3, color = "black"),
      axis.ticks        = element_line(linewidth = 0.3, color = "black"),
      panel.border      = element_blank(),
      panel.grid        = element_blank(),
      plot.margin       = unit(c(0.4, 0.5, 0.4, 0.5), "cm"),
      plot.title        = element_text(size = 9, hjust = 0.5, color = "black"),
      legend.text       = element_text(size = 8, color = "black"),
      legend.title      = element_text(size = 8, color = "black"),
      legend.key.size   = unit(0.9, "line"),
      legend.background = element_rect(fill = "transparent", linetype = "blank")
    )
}


# =============================================================================
# SECTION 1 — LOAD AND PROCESS CHIRPS PRECIPITATION DATA
# =============================================================================

# Load global CHIRPS monthly NetCDF (precip variable only)
chirps_global <- rast("DATA/chirps-v2.0.monthly.nc", subds = "precip")

# Subset to 2000–2024 (layers 228–527 = 300 months)
chirps_2000_2024 <- chirps_global[[228:527]]

# Site location
SHHR <- vect(
  data.frame(lon = 29.32, lat = -19.71),
  geom = c("lon", "lat"),
  crs  = "EPSG:4326"
)

# Sequence of dates matching raster layers
dates <- seq(as.Date("2000-01-01"), as.Date("2024-12-01"), by = "month")


# Extract monthly rainfall at SHR
rain_raw <- extract(chirps_2000_2024, SHHR)

rain_df <- rain_raw |>
  select(starts_with("precip_")) |>
  pivot_longer(everything(), names_to = "layer", values_to = "rain_mm") |>
  mutate(date = dates) |>
  select(date, rain_mm) |>
  mutate(
    YEAR  = year(date),
    month = month(date, label = TRUE, abbr = TRUE)
  )


# =============================================================================
# SECTION 2 — DOWNLOAD NASA POWER TEMPERATURE DATA
# =============================================================================

temp_power <- get_power(
  community    = "AG",
  lonlat       = c(29.32, -19.71),
  pars         = c("T2M", "T2M_MAX", "T2M_MIN"),
  temporal_api = "MONTHLY",
  dates        = c(200001, 202412)
)

month_lookup <- c(
  JAN = 1, FEB = 2, MAR = 3, APR = 4,
  MAY = 5, JUN = 6, JUL = 7, AUG = 8,
  SEP = 9, OCT = 10, NOV = 11, DEC = 12
)

temp_df <- temp_power |>
  select(YEAR, JAN:DEC, PARAMETER) |>
  pivot_longer(JAN:DEC, names_to = "month_abbr", values_to = "value") |>
  mutate(
    month_num = month_lookup[month_abbr],
    date      = as.Date(sprintf("%04d-%02d-01", YEAR, month_num))
  ) |>
  select(date, PARAMETER, value) |>
  pivot_wider(names_from = PARAMETER, values_from = value) |>
  rename(Tmean = T2M, Tmax = T2M_MAX, Tmin = T2M_MIN) |>
  arrange(date)


# =============================================================================
# SECTION 3 — MERGE INTO FULL CLIMATE DATA FRAME
# =============================================================================

climate_df <- rain_df |>
  inner_join(temp_df, by = "date") |>
  mutate(
    YEAR  = year(date),
    month = factor(
      month(date, label = TRUE, abbr = TRUE),
      levels = c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")
    )
  )


# =============================================================================
# SECTION 4 — SEASONAL CLIMATOLOGY (monthly averages across all years)
# =============================================================================

climate_monthly <- climate_df |>
  group_by(month) |>
  summarise(
    rain_mm = mean(rain_mm, na.rm = TRUE),
    Tmean   = mean(Tmean,   na.rm = TRUE),
    Tmax    = mean(Tmax,    na.rm = TRUE),
    Tmin    = mean(Tmin,    na.rm = TRUE),
    .groups = "drop"
  )

climate_long <- climate_monthly |>
  pivot_longer(c(Tmean, Tmax, Tmin),
               names_to  = "temp_var",
               values_to = "temp_value")

scale_clim <- max(climate_monthly$rain_mm) / max(climate_long$temp_value)

# Panel A: Seasonal climatogram
panel_A <- ggplot() +
  geom_col(
    data  = climate_monthly,
    aes(x = month, y = rain_mm, fill = "Rainfall"),
    width = 0.7, alpha = 0.8
  ) +
  geom_line(
    data      = climate_long,
    aes(x = month, y = temp_value * scale_clim,
        color = temp_var, group = temp_var),
    linewidth = 1.0
  ) +
  geom_point(
    data = climate_long,
    aes(x = month, y = temp_value * scale_clim, color = temp_var),
    size = 1.8
  ) +
  scale_y_continuous(
    name     = "Mean monthly rainfall (mm)",
    sec.axis = sec_axis(~ . / scale_clim, name = "Temperature (°C)")
  ) +
  scale_fill_manual(values  = c("Rainfall" = "steelblue"), name = NULL) +
  scale_color_manual(
    values = c("Tmean" = "black", "Tmax" = "red", "Tmin" = "brown"),
    name   = NULL
  ) +
  labs(
    x     = "Month"#,
   #title = "(a) Seasonal climatology (2000–2024)"
  ) +
  theme_beautiful() +
  theme(
    axis.title.y.left  = element_text(color = "black", size = 10, angle = 90),
    axis.title.y.right = element_text(color = "black", size = 10, angle = 90),
    legend.position    = "top"
  )


# =============================================================================
# SECTION 5 — ANNUAL PRECIPITATION THROUGH TIME (1981–Nov 2025)
# =============================================================================

# Extract full CHIRPS record: Jan 1981 – Nov 2025 (all available layers)
n_layers   <- nlyr(chirps_global)
dates_full <- seq(as.Date("1981-01-01"), by = "month", length.out = n_layers)

rain_df_full <- extract(chirps_global[[1:n_layers]], SHHR) |>
  select(starts_with("precip_")) |>
  pivot_longer(everything(), names_to = "layer", values_to = "rain_mm") |>
  mutate(
    date  = dates_full,
    YEAR  = year(date),
    month = month(date, label = TRUE, abbr = TRUE)
  ) |>
  select(date, YEAR, month, rain_mm)

# Annual totals across the full record
annual_climate_full <- rain_df_full |>
  group_by(YEAR) |>
  summarise(
    rain_mm_annual = sum(rain_mm, na.rm = TRUE),
    .groups        = "drop"
  )

# Long-term mean (complete years only; 2025 has Jan–Nov)
mean_ann_rain_full <- annual_climate_full |>
  filter(YEAR < 2025) |>
  pull(rain_mm_annual) |>
  mean()

# Panel B: Annual precipitation time series
panel_B <- ggplot(annual_climate_full, aes(x = YEAR, y = rain_mm_annual)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.75) +
  geom_hline(
    yintercept = mean_ann_rain_full,
    linetype   = "dashed",
    color      = "black",
    linewidth  = 0.3
  ) +
  # annotate(
  #   "text",
  #   x     = 1981,
  #   y     = mean_ann_rain_full + 65,
  #   label = paste0("Mean = ", round(mean_ann_rain_full, 0), " mm"),
  #   hjust = 0, size = 3.5, color = "black"
  # ) +
  scale_x_continuous(breaks = seq(1985, 2025, by = 10)) +
  labs(
    x = "Year",
    y = "Annual rainfall (mm)"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# =============================================================================
# SECTION 6 — INTRA-ANNUAL RAINFALL VARIABILITY: UNRANKED GINI INDEX (UGi)
# UGi quantifies the degree to which monthly precipitation totals are
# clustered within the year, preserving temporal (calendar) order.
# Formula: UGi = sum_i sum_j |x_i - x_j| / (2 * n^2 * x_bar)
# Values near 0 = uniformly distributed rainfall; near 1 = highly clustered.
# 2025 excluded — incomplete year (Jan–Nov only).
# =============================================================================

# UGi function: mean absolute pairwise difference, normalised by 2 * n^2 * mean
ugi_index <- function(x) {
  x  <- x[!is.na(x) & x >= 0]
  n  <- length(x)
  mu <- mean(x)
  if (n == 0 || mu == 0) return(NA_real_)
  sum(outer(x, x, FUN = function(a, b) abs(a - b))) / (2 * n^2 * mu)
}

# Compute UGi per complete year (exclude partial 2025)
annual_variability_full <- rain_df_full |>
  filter(YEAR < 2025) |>
  group_by(YEAR) |>
  summarise(
    ugi     = ugi_index(rain_mm),
    rain_cv = sd(rain_mm, na.rm = TRUE) / mean(rain_mm, na.rm = TRUE),
    .groups = "drop"
  )

mean_ugi_full <- mean(annual_variability_full$ugi, na.rm = TRUE)

# Panel C: UGi through time
panel_C <- ggplot(annual_variability_full, aes(x = YEAR, y = ugi)) +
  geom_line(color = "#8B4513", linewidth = 0.9) +
  geom_point(color = "#8B4513", size = 1.2) +
  geom_hline(
    yintercept = mean_ugi_full,
    linetype   = "dashed",
    color      = "black",
    linewidth  = 0.2
  ) +
  # annotate(
  #   "text",
  #   x     = 1981,
  #   y     = mean_ugi_full + 0.035,
  #   label = paste0("Mean = ", round(mean_ugi_full, 2)),
  #   hjust = 0, size = 3.5, color = "black"
  # ) +
  scale_x_continuous(breaks = seq(1985, 2025, by = 10)) +
  scale_y_continuous(limits = c(0.4, 0.9)) +
  labs(
    x = "Year",
    y = "UGi (intra-annual rainfall)"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# =============================================================================
# SECTION 7 — MULTI-PANEL FIGURE
# Layout: seasonal climatogram (full width top) / annual rain | Gini (bottom)
# =============================================================================

climate_multipanel <- panel_A / (panel_B | panel_C) +
  plot_layout(heights = c(1.1, 1))+
plot_annotation(
  tag_levels = 'a',
  tag_prefix = '(',
  tag_suffix = ')',
  theme = theme(plot.tag = element_text(size = 8, hjust = 0))  # Left align tags
) &
  theme(
    axis.text = element_text(size = 11),        # Increase axis label font size
    axis.title = element_text(size = 11),       # Increase axis title font size
    plot.tag = element_text(size = 11, hjust = 0)) 



ggsave(climate_multipanel,filename ="Plots/SHR_UGiClimate_Multipanel.png",
       width = 16, height = 16, units = "cm")

climate_multipanel
