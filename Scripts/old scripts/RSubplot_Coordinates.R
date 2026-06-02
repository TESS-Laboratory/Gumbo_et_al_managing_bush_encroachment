library(tidyverse)
library(sf)
library(terra)


###### Using sf instead of terra
# load data
pts_sf <- read_csv("DATA/CoordinatesS/samplePlotCoordnates.csv")

#Convert points to sf
pts_sf <- st_as_sf(
  pts_sf,
  coords = c("Longitude", "Latitude"),
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
write.csv(area_summary, "DATA/RSubplot_total_area.csv", row.names = FALSE)
