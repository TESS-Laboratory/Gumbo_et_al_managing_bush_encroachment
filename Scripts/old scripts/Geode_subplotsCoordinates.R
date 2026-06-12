#CENTER that define its polygon.
library(tidyverse)
library(dplyr)
library(stringr)
library(sf)
library(geosphere)
library(terra)

# load data
geodepoints <- read.csv("DATA/CoordinatesS/GeodeplusSample.csv")
#geodepoints <- read.csv("C:/Users/tg488/OneDrive - University of Exeter/Project development - Taps/Files from gumbo_dev/Coordinaates at SHR/GeodeplusSample.csv")
##

pts_sf <- geodepoints

#Convert points to sf
pts_sf <- st_as_sf(
  pts_sf,
  coords = c("longitude", "latitude"),
  crs = 4326   # EPSG:4326
)
# Extract plot id and centre
pts_sf <- pts_sf %>%
  mutate(
    is_center = grepl("ZCENTER$", I.D_code),               # TRUE for center points
    Subplot  = ifelse(is_center, NA, sub("CR[0-4]$", "", I.D_code)),
    BlockID   = sub("Z.*$", "", I.D_code)                 # identifies the block for center
  )

# extract center points
centers <- pts_sf %>% filter(is_center)


#sanity check 
centers %>% group_by(BlockID) %>% tally()

# build polygon function
build_subplot_polygon <- function(block, plot_base, pts_sf, centers) {
  
  # Shared center for this block
  center <- centers %>% filter(BlockID == block)
  if (nrow(center) != 1) stop(paste("Block", block, "has", nrow(center), "centers!"))
  
  # Get corners for this plot
  corners <- pts_sf %>% filter(BlockID == block, Subplot == plot_base)
  if (nrow(corners) < 3) stop(paste("Plot", plot_base, "in block", block,
                                    "has less than 3 corners!"))
  
  # Combine corners + center
  pts <- rbind(corners, center)
  
  # Order points around centroid for valid polygon
  coords <- st_coordinates(pts)[, 1:2, drop = FALSE]
  centroid <- colMeans(coords)
  angles <- atan2(coords[,2] - centroid[2], coords[,1] - centroid[1])
  coords <- coords[order(angles), ]
  coords <- rbind(coords, coords[1, ])  # close polygon
  
  st_polygon(list(coords))
}

#Prepare list of plots
plots_df <- pts_sf %>%
  filter(!is_center) %>%
  distinct(BlockID, Subplot)

# # Build all polygons
polygons_sf <- st_sf(
  plots_df,
  geometry = st_sfc(
    mapply(
      build_subplot_polygon,
      block = plots_df$BlockID,
      plot_base = plots_df$Subplot,
      MoreArgs = list(pts_sf = pts_sf, centers = centers),
      SIMPLIFY = FALSE
    ),
    crs = st_crs(pts_sf)
  )
)

#Check number of vertices per polygon
sapply(st_geometry(polygons_sf), function(g) nrow(g[[1]]) - 1)  # should be 4

#Calculate area (m²)
polygons_sf$Area_m2 <- as.numeric(st_area(polygons_sf))
polygons_sf %>% select(BlockID,Subplot, Area_m2)

# Create summary with one row per polygon
area_summary <- polygons_sf %>%
  st_drop_geometry() %>%        # remove geometry column
  select(Subplot, Area_m2)  %>%
  mutate(Area_m2 = round(Area_m2))   

# Save as csv file 
  #write.csv(area_summary, "DATA/GEODE_Subplot_area.csv", row.names = FALSE)



###DENSITY PLOTS

# Extracting sites from subplot
area_summary <- polygons_sf %>%
  st_drop_geometry() %>%
  select(Subplot, Area_m2) %>%
  mutate(
    Area_m2 = round(Area_m2),
    Site = str_extract(Subplot, "^S[A-F]")  # extract SA, SB, SC, 
  )


# density plots at site level
Gdens <- ggplot(area_summary, aes(x = Area_m2)) +
  geom_density(fill = "steelblue", alpha = 0.6)+
  facet_wrap(~ Site, scales = "free_y") +
  geom_vline(xintercept = 600, linetype = "dashed") +
  labs(x = "Area (m²)", y = "Density") +
  theme_classic()

# Saving file
 #ggsave(Gdens, filename = "Plots/Geode densityPLOT.png", 
 #width = 16, height = 12, units = "cm")



#### HISTOGRAM BY SITE

# Extracting sites from subplot
area_summary <- polygons_sf %>%
  st_drop_geometry() %>%
  select(Subplot, Area_m2) %>%
  mutate(
    Area_m2 = round(Area_m2),
    Site = str_extract(Subplot, "^S[A-F]")  # extract SA, SB, SC, 
  )

# plot histogram
Ghist <- ggplot(area_summary, aes(x = Area_m2)) +
  geom_histogram(bins = 25, fill = "steelblue", color = "black") +
  geom_vline(
    xintercept = 600,
    linetype = "dashed",
    color = "red",
    linewidth = 0.5
  ) +
  facet_wrap(~ Site) +
  labs(x = "Area (m²)", y = "Count") +
  theme_classic()

# saving
ggsave(Ghist, filename = "Plots/Geode Histogram.png", 
       width = 16, height = 12, units = "cm")


#### Summary table
area_summary_stats <- area_summary %>%
  group_by(Site) %>%
  summarise(
    n = n(),
    mean_area = mean(Area_m2, na.rm = TRUE),
    median_area = median(Area_m2, na.rm = TRUE))


### Count of exact polygon areas per site

area_counts <- area_summary %>%
  group_by(Site, Area_m2) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(Site, Area_m2)


# save as csv
write.csv(area_counts, "DATA/Area_counts.csv", row.names = FALSE)


### Bin areas into classes  ( = 40 m² bins)

Binned_area_counts <- area_summary %>%
  mutate(bin = cut(Area_m2, breaks = seq(400, 800, by = 40))) %>%
  group_by(Site, bin) %>%
  summarise(count = n(), .groups = "drop")


#### Frequency classes per site at 10% 

Site_area_classes <- area_summary %>%
  mutate(
    class = case_when(
      Area_m2 < 540 ~ "<539",
      Area_m2 >= 540 & Area_m2 <= 600 ~ "540–600",
      Area_m2 >= 601 & Area_m2 <= 660 ~ "601–660",
      Area_m2 >= 661 ~ ">661"
    )
  ) %>%
  group_by(Site, class) %>%
  summarise(count = n(), .groups = "drop")


# classes placed orderly

Site_area_classes <- Site_area_classes %>%
  mutate(
    class = factor(class, levels = c("<539", "540–600", "601–660", ">661"))
  ) %>%
  arrange(Site, class)


# Pivot table for better viewing

area_classes_wide <- Site_area_classes %>%
  pivot_wider(names_from = class, values_from = count, values_fill = 0)

# save as csv
write.csv(area_classes_wide, "DATA/area_classes_by_site.csv", row.names = FALSE)
