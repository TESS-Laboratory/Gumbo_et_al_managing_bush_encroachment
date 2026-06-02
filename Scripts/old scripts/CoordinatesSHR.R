library(tidyverse)
library(vegan)
library(multcompView)
library(patchwork)
library(lmerTest)
library(Matrix)
library(lme4)
library(emmeans)
library(here) ## MOST IMPORTANT NOT TO FORGET THIS ONE in quarto
library(robustbase)
library(sjPlot)
library(flextable)
library(officer)
library(glmmTMB)
library(DHARMa)   # model diagnostics GLMM
library(performance)  # model diagnostics
library(car)
library(openxlsx)
library(broom.mixed) #convert model objects to data frames
library(corrplot)
library(robustlmm) # for robust regression analysis
library(boot)  # For bootsrapping
library(DescTools)
library(effectsize) # For easy centering
library(marginaleffects)
library(effects)
library(ggeffects)
library(knitr)  # for table
library(gtsummary)
library(MASS)
library(multcomp) # for compact letters
library(sf)#for coordinates
library(terra) 

#create theme beautiful
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
        linetype = "blank"
      )
    )
}




# Read Excel file
Cdf <- read_csv("DATA/Coordinates_SHR.csv")

# Create sf object in decimal degrees (WGS84)
sf_dd <- st_as_sf(
  Cdf,
  coords = c("Longitude", "Latitude"),
  crs = 4326   # EPSG:4326
)

# Reproject to UTM Zone 35 South (Gweru)
sf_utm <- st_transform(sf_dd, crs = 32735)

# Extract UTM coordinates
utm <- st_coordinates(sf_utm)

Cdf$Easting  <- utm[, "X"]
Cdf$Northing <- utm[, "Y"]


########### ISOLATING SUBPLOTS BY EXCLUDING Quadrats


# Filter all endings with Q1 - Q5
Cdf_noR <- Cdf[!grepl("Q[1-5]$", Cdf$I.D_code), ]


# Create a clean subPlot ID (SA–SF only)
Cdf$PlotID <- sub(
  "^(S[A-F]P[1-5]Z[1-4]+).*",
  "\\1",
  Cdf$I.D_code
)


#Exclude subquadrats (Q1–Q5) for subplot boundaries
Cdf_plot_pts <- Cdf[
  grepl("CR", Cdf$I.D_code) |        # explicit corner markers
    grepl("CENTER$", Cdf$I.D_code) |     # shared central corner 
    !grepl("Q[1-5]$", Cdf$I.D_code),   # corner points without DS
]


#  Build plot polygons using a convex hull

plot_ids <- unique(Cdf_plot_pts$PlotID)

plot_polygons <- lapply(plot_ids, function(pid) {
  
  df <- Cdf_plot_pts[Cdf_plot_pts$PlotID == pid,
                     c("Easting", "Northing")]
  
  # Create point vector
  pts <- vect(as.matrix(df),
              type = "points",
              crs  = "EPSG:32735")
  
  # Build polygon using convex hull
  hull <- convHull(pts)
  
  hull
})

# naming them
names(plot_polygons) <- plot_ids


# Diagnose geometry types on polygons

  table(sapply(plot_polygons, geomtype)) # = 86 complete polygons

# Keep ONLY valid polygon geometries (required fix)
  is_poly <- sapply(plot_polygons, function(x) geomtype(x) == "polygons")
  
  plot_polygons_poly <- plot_polygons[is_poly]
  
  length(plot_polygons_poly)

  
#Combine polygons safely
  
  plots_vect <- vect(plot_polygons_poly)
  

  # identify problematic subplots
  failed_plots <- setdiff(names(plot_polygons), names(plot_polygons_poly))
  failed_plots
  
    
# check number of points per subplot
table(Cdf_plot_pts$PlotID)




################### CODE TO PRODUCE RECTANGULAR POLYGONS

# Build rectangular plots

plot_polygons_rect <- list()

for (pid in plot_ids) {
  
  df <- Cdf_plot_pts[Cdf_plot_pts$PlotID == pid,
                     c("Easting", "Northing")]
  
  if (nrow(df) < 3) next
  
  pts <- vect(as.matrix(df),
              type = "points",
              crs  = "EPSG:32735")
  
  hull <- convHull(pts)
  rect <- minRect(hull)
  
  # keep only true polygons
  if (geomtype(rect) == "polygons") {
    plot_polygons_rect[[pid]] <- rect
  }
}


#verify if plots id exist
length(plot_polygons_rect)
names(plot_polygons_rect)[1:10]

# remove failed geometries 
plot_polygons <- plot_polygons[!sapply(plot_polygons, is.null)]

is_poly <- sapply(plot_polygons, function(x) geomtype(x) == "polygons")
plot_polygons_rect <- plot_polygons[is_poly]


# Combine polygons (now names are preserved)
plots_vect <- vect(plot_polygons_rect)

# Attach PlotID (now works)
plots_vect$PlotID <- names(plot_polygons_rect)

#verify 
table(plots_vect$PlotID)

# Recalculate area safely
plots_vect$Area_m2 <- terra::expanse(plots_vect)

# Summarise  area per subplot
by(plots_vect$Area_m2, plots_vect$PlotID, summary)



# sanity line check
stopifnot(length(plots_vect) == length(plots_vect$PlotID))


# verifying 
table(plots_vect$PlotID)

 #confriming the object is geometrically valid
length(plots_vect)
class(plots_vect)

#reattaching subplots - subplots that have complete corners 
names(plot_polygons_rect)


##Show subplots approximately 600 m² 
#Ensure Area_m2 exists
# Calculate it explicitly if not done yet
plots_vect$Area_m2 <- terra::expanse(plots_vect)

#use logical indexing -  Define “approximately 600 m²” as ±10% (i.e. 540–660 m²).

plots_600 <- plots_vect[
  plots_vect$Area_m2 >= 540 & plots_vect$Area_m2 <= 660,
]

# check results - number of subplots ca. 600m2
plots_600[, c("PlotID", "Area_m2")]
length(plots_600)


# Visual sanity check
plot(plots_vect)
points(Cdf_plot_pts$Easting,
       Cdf_plot_pts$Northing,
       col = "red", pch = 20)


### CREATING A TABLE THAT SHOWS SUBPLOTS AND THEIR AREA
## Step 1: Create a clean table of subplot areas
plot_area_table <- data.frame(
  PlotID   = plots_vect$PlotID,
  Area_m2  = round(plots_vect$Area_m2, 2)  # Round to 2 decimal places
)

# sort by subplotID
plot_area_table <- plot_area_table[order(plot_area_table$PlotID), ]

#  Convert to a flextable
ft <- flextable(plot_area_table)
ft <- autofit(ft)  # Adjust column widths

#  Create Word document
doc <- read_docx() %>%
  body_add_par("Plot Areas (m²) Summary", style = "heading 1") %>%
  body_add_flextable(ft)






################### EXPORTING TO GOOGLE EARTH OR QGIS

# transform to latitude/longitude.. Transform to WGS84 (EPSG:4326)

# Step 1: Transform polygons to WGS84
plots_vect_ll <- terra::project(plots_vect, "EPSG:4326")

# Step 2: Initialize empty data frame
plot_coords <- data.frame()

# Step 3: Loop over each polygon
for (i in 1:length(plots_vect_ll)) {
  
  pid <- plots_vect_ll$PlotID[i]
  poly <- plots_vect_ll[i, ]
  
  # Extract polygon vertices as matrix
  coords_mat <- terra::geom(poly)  # columns are x (lon), y (lat), part, ring
  coords_df <- data.frame(
    PlotID    = pid,
    Longitude = coords_mat[, "x"],
    Latitude  = coords_mat[, "y"],
    CornerID  = seq_len(nrow(coords_mat))
  )
  
  plot_coords <- rbind(plot_coords, coords_df)
}

# Step 4: Inspect
head(plot_coords)


#Option B — KML (direct polygon visualization)
terra::writeVector(plots_vect_ll, "Plots_GoogleEarth.kml", filetype = "KML")





####### OPTION 2 - coordinates for a subplot will be arranged in a single row
# Step 1: Transform polygons to WGS84
plots_vect_ll <- terra::project(plots_vect, "EPSG:4326")

# Step 2: Initialize empty list to hold rows
plot_rows <- list()

# Step 3: Loop over each plot polygon
for (i in 1:length(plots_vect_ll)) {
  
  pid <- plots_vect_ll$PlotID[i]
  poly <- plots_vect_ll[i, ]
  
  # Extract polygon vertices
  coords_mat <- terra::geom(poly)  # columns: x = lon, y = lat, part, ring
  
  # Flatten coordinates: lon1, lat1, lon2, lat2, ...
  coords_flat <- as.vector(t(coords_mat[, c("x", "y")]))
  
  # Create a data frame row
  plot_row <- data.frame(
    PlotID = pid,
    t(as.data.frame(coords_flat))
  )
  
  plot_rows[[i]] <- plot_row
}

# Step 4: Combine all rows into one data frame
plot_coords_single <- do.call(rbind, plot_rows)

# Optional: rename columns as Lon1, Lat1, Lon2, Lat2, etc.
num_corners <- (ncol(plot_coords_single) - 1) / 2
coord_names <- c("PlotID", unlist(lapply(1:num_corners, function(i) c(paste0("Lon", i), paste0("Lat", i)))))
colnames(plot_coords_single) <- coord_names

# Step 5: Save to CSV
 #write.csv(plot_coords_single, "Plot_Coordinates_SingleRow.csv", row.names = FALSE)

# Step 6: Inspect
head(plot_coords_single)




########### OPTION 3 - corner coordinates for a subplot will be arranged in rows

# Ensure polygons are in geographic coordinates
plots_vect_ll <- terra::project(plots_vect, "EPSG:4326")

# extract plot corners coordinates
plot_coords <- data.frame()

for (i in seq_len(length(plots_vect_ll))) {
  
  pid <- plots_vect_ll$PlotID[i]
  poly <- plots_vect_ll[i, ]
  
  # Extract geometry as matrix
  g <- terra::geom(poly)
  
  # Keep only coordinate columns
  coords <- g[, c("x", "y")]
  
  # Build table
  tmp <- data.frame(
    PlotID    = pid,
    CornerID  = seq_len(nrow(coords)),
    Longitude = coords[, "x"],
    Latitude  = coords[, "y"]
  )
  
  plot_coords <- rbind(plot_coords, tmp)
}

## inspect table
head(plot_coords)
tail(plot_coords)

# Save table as CSv 
write.csv(
plot_coords,
"SubPlot_Corners.csv",
row.names = FALSE
)



############ REDOING SUBPLOTS TO INCLUDE COORDINATES THAT ARE SHARED AT CORNERS

# Keep only valid corner-related coordinates
Cdf_plot_pts <- Cdf[
  grepl("CR", Cdf$I.D_code) |          # explicit corner markers
    grepl("CENTER$", Cdf$I.D_code) |      # shared central corner
    !grepl("Q[1-5]$", Cdf$I.D_code),  # corner points without DS
]


# Convert points to spatial objects

pts <- vect(
  Cdf_plot_pts,
  geom = c("Easting", "Northing"),
  crs  = "EPSG:32735"   # UTM Zone 35S (Zimbabwe)
)

# split subplots
plot_ids <- unique(pts$PlotID)

# building polygons with existing coordinates
plot_polygons <- list()

for (pid in plot_ids) {
  
  p <- pts[pts$PlotID == pid, ]
  
  # Need at least 3 points to form a polygon
  if (length(p) < 3) next
  
  # Convert to polygon (convex hull = safest)
  poly <- terra::convHull(p)
  
  poly$PlotID <- pid
  poly$Area_m2 <- terra::expanse(poly)
  
  plot_polygons[[pid]] <- poly
}

# combine polygons
unique(sapply(plot_polygons_ok, class))
unique(sapply(plot_polygons_ok, geomtype))


plot_polygons_ok <- plot_polygons_ok[
  sapply(plot_polygons_ok, function(x) {
    inherits(x, "SpatVector") && geomtype(x) == "polygons"
  })
]

# see number of polygons
length(plot_polygons_ok)


# combine multiple SpatVector objects
plots_final <- vect(plot_polygons_ok)

# compute area
plots_final$Area_m2 <- expanse(plots_final)

# summarise
summary(plots_final$Area_m2)
length(plots_final)

#validate geometry
table(geomtype(plots_final))

# visual sanity
plot(plots_final, col = "lightgrey")

# extract subplots area into data frame

plot_area_df <- data.frame(
  PlotID  = plots_final$PlotID,
  Area_m2 = plots_final$Area_m2
)


# sanity checks
summary(plot_area_df$Area_m2)
nrow(plot_area_df)   # = 91

## Export to CSV 
  # write.csv(
  plot_area_df,
  file = "subplot_areas.csv",
  row.names = FALSE
)


# flag subplots far from ca. 600m2
plot_area_df$Flag_600m2 <- plot_area_df$Area_m2 >= 540 &
  plot_area_df$Area_m2 <= 660



#### REMOVING COORDINATES THAT ARE NOISY OR OUTLYING
ids_to_remove <- c(
  "SAP5Z3CR0",
  "SAP5Z1",
  "SAP5Z2CR1",
  "SAP5Z1CR3",
  "SAPFZ1CR4",
  "SAP4Z3CR0",
  "SAP4Z1CR0",
  "SAP4Z1CR0",
  "SAP4Z4CR0",
  "SAP4Z2CR0",
  "SAP3Z4CR0",
  "SAP3Z3CR0",
  "SAP3Z4CR0",
  "SAP3Z2CR1",
  "SBP5Z1",
  "SBP5Z2CR0",
  "SBP5Z3CR0",
  "SBP5Z4CR3",
  "SBP4Z3CR2",
  "SBP4Z3CR0",
  "SBP4Z2CR0",
  "SBP3Z4CR0",
  "SBP3Z1CR0",
  "SBP3Z4CR0",
  "SBP3Z2CR0",
  "SBP2Z4CR0",
  "SBP2Z4CR2",
  "SBP2Z1CR0",
  "SBP2Z1CR3",
  "SBP1Z1CR0",
  "SCP5Z2",
  "SCP5ZCENTER",
  "SCP5Z1CR2",
  "SCP5Z2CR2",
  "SCP5Z1CR3",
  "SCP5Z3CR4",
  "SCP4Z2CR2",
  "SCP4Z3CR0",
  "SCP4Z4CR4",
  "SCP4ZCENTER",
  "SCP3Z2CR2",
  "SCP3Z1CR2",
  "SCP3Z3CR4",
  "SCP2Z4CR3",
  "SCP2Z4CR4",
  "SCP2Z3CR1",
  "SCP2Z1CR0",
  "SCP2Z3CR0",
  "SCP2Z1CR3",
  "SCP1Z1",
  "SCP1Z2CR4",
  "SCP1Z4CR4",
  "SCP1Z3CR0",
  "SCP1Z3CR0",
  "SCP1CENTER",
  "SDP4Z3CR0",
  "SDP4Z4CR0",
  "SDP4Z2CR0",
  "SDP3Z4CR0",
  "SDP3Z1CR0",
  "SDP3Z3CR0",
  "SDP2Z3CR0",
  "SDP2Z3CR0",
  "SDP2Z2CR0",
  "SDP3Z1CR3",
  "SDP2Z1CR0",
  "SDP2Z4CR0",
  "SDP1Z3CR0",
  "SDP1Z4CR0",
  "SDP1Z2CR0",
  "SDP1Z2CR0",
  "SDP1Z3CR3",
  "SEP5Z1CR0",
  "SEP2Z2CR0",
  "SEP2Z3CR0",
  "SEP2Z4CR0",
  "SEP2Z1CR0",
  "SEP1Z1CR4",
  "SEP1Z1CR0",
  "SEP1Z2CR0",
  "SEP1Z4CR0",
  "SEP1Z3CR0",
  "SEP2Z1CR0",
  "SEP2ZCENTER",
  "SEP3Z2CR0",
  "SEP3Z4CR0",
  "SEP3Z4CR2",
  "SEP3Z3CR0",
  "SEP3Z23",
  "SEP3Z3CR1",
  "SEP3ZCENTER",
  "SEP3Z1",
  "SEP3Z1CR1",
  "SEP3Z2CR0",
  "SEZ1CR4",
  "SEP4Z4CR0",
  "SEP4Z3CR0",
  "SEP4Z4CR0",
  "SEP4Z1CR3",
  "SEP4Z2CR0",
  "SEP4Z1CR0",
  "SEP4Z1CR1",
  "SFP5Z2CR0",
  "SFP5Z1CR1",
  "SFP5Z1CR4",
  "SFP5Z1CR0",
  "SFP5Z3CR0",
  "SFP5Z3CR1",
  "SFP5Z4CR0",
  "SFP4Z2CR0",
  "SFP4Z1CR2",
  "SFP4Z1CR3",
  "SFP4Z2CR4",
  "SFP4Z2CR0",
  "SFP4Z4CR0",
  "SFP4Z3CR0",
  "SFP4Z3CR3",
  "SFP2Z3CR0",
  "SFP2Z2CR0",
  "SFP2Z4CR0",
  "SFP3Z3CR2",
  "SFP4Z2CR0",
  "SFP4Z3CR0",
  "SFP2Z4CR1"
)

# Then filter 
Cdf_clean <- Cdf_plot_pts |>
  filter(!I.D_code %in% ids_to_remove)


# check if deleted
nrow(Cdf_plot_pts)
nrow(Cdf_clean)


## saving clean sheet as CSV

#write.csv(
  Cdf_clean,
  "subplot_coordinates_clean.csv",
  row.names = FALSE
)


########## redoing polygon and calculating area using cleaned data

Cdf_clean <- read_csv("Data/RefinedSHR_points.csv")

Cdf_clean <- Cdf_clean |>
  filter(grepl("CR|CENTER", I.D_code))

# Creating subplots without excluding shared markers
Cdf_clean$PlotID <- Cdf_clean$I.D_code |>
  str_remove("CR[0-9]+$") |>
  str_remove("CENTER$") |>
  str_remove("Q[1-5]$")

# Verification
table(is.na(Cdf_clean$PlotID))

# polygon creation
pts <- vect(
  Cdf_clean,
  geom = c("Easting", "Northing"),
  crs  = "EPSG:32735"
)

plot_ids <- unique(pts$PlotID)

plot_polygons <- list()

for (pid in plot_ids) {
  
  p <- pts[pts$PlotID == pid, ]
  
  if (length(p) < 3) next
  
  poly <- convHull(p)
  poly$PlotID  <- pid
  poly$Area_m2 <- expanse(poly)
  
  plot_polygons[[pid]] <- poly
}

plots_final <- vect(plot_polygons)

# check number of polygons
length(plots_final)


## extract area of each plot
plot_area_table <- data.frame(
PlotID  = plots_final$PlotID,
Area_m2 = plots_final$Area_m2
)




#################### Redoing subplot
# Creating subplots without excluding shared markers
Cdf_clean$SubplotID <- Cdf_clean$I.D_code |>
  str_remove("CR[0-9]+$") |>
  str_remove("CENTER$") |>
  str_remove("Q[1-5]$")


#verify 
stopifnot(!any(is.na(Cdf_clean$SubplotID)))

# convert to spatial points no grouping
pts <- vect(
  Cdf_clean,
  geom = c("Easting", "Northing"),
  crs  = "EPSG:32735"
)


# build four sided polygon 

build_quad <- function(p) {
  
  if (length(p) < 4) return(NULL)
  
  # Get convex hull
  h <- convHull(p)
  
  # If hull has more than 4 vertices, reduce to 4 extremes
  coords <- crds(h)
  
  if (nrow(coords) > 4) {
    
    # extreme points: min/max X and Y
    idx <- unique(c(
      which.min(coords[,1]),
      which.max(coords[,1]),
      which.min(coords[,2]),
      which.max(coords[,2])
    ))
    
    coords <- coords[idx, ]
  }
  
  # rebuild polygon
  vect(coords, type = "polygons", crs = crs(p))
}

# apply to each subplot 

subplot_polygons <- list()

for (sid in unique(pts$SubplotID)) {
  
  p <- pts[pts$SubplotID == sid, ]
  
  poly <- build_quad(p)
  
  if (!is.null(poly)) {
    poly$SubplotID <- sid
    poly$Area_m2   <- expanse(poly)
    subplot_polygons[[sid]] <- poly
  }
}


# combine to one GIS layer
subplots_final <- vect(subplot_polygons)


# validate results
length(subplots_final)
summary(subplots_final$Area_m2)

# check subplots with missing corners
table(pts$SubplotID)
table(pts$SubplotID)[table(pts$SubplotID) < 4]



######## REDOING SUBPLOT  **********
# Creating subplots without excluding shared markers
Cdf_clean$SubplotID <- Cdf_clean$I.D_code |>
  str_remove("CR[0-9]+$") |>
  str_remove("CENTER$") |>
  str_remove("Q[1-5]$")

# convert to spatial points no grouping
pts <- vect(
  Cdf_clean,
  geom = c("Easting", "Northing"),
  crs  = "EPSG:32735"
)

# Compute subplot centroids (spatial reference)
subplot_centers <- aggregate(pts, by = "SubplotID", fun = mean)


# Build subplot polygons using nearest corners
build_subplot_polygon <- function(center, all_points, k = 4) {
  
  # distances from center to all points
  d <- distance(center, all_points)
  
  # select k nearest points
  idx <- order(d)[1:k]
  p   <- all_points[idx]
  
  if (length(p) < 4) return(NULL)
  
  poly <- convHull(p)
  poly$Area_m2 <- expanse(poly)
  
  poly
}

# Apply to all subplots
subplot_polygons <- list()

for (sid in unique(subplot_centers$SubplotID)) {
  
  center <- subplot_centers[subplot_centers$SubplotID == sid, ]
  
  poly <- build_subplot_polygon(center, pts, k = 4)
  
  if (!is.null(poly)) {
    poly$SubplotID <- sid
    subplot_polygons[[sid]] <- poly
  }
}


# Combine into final polygon layer
subplots_final <- vect(subplot_polygons)

length(subplots_final)
summary(subplots_final$Area_m2)

# create subplot area table
subplot_area_table <- data.frame(
  SubplotID = subplots_final$SubplotID,
  Total_Area_m2 = subplots_final$Area_m2
)

# summarise subplot area
subplot_area_table <- subplot_area_table[
  order(subplot_area_table$SubplotID),
]


####################### REDOING SUBPLOTS FOCUSING ON CENTER
# flag CENTER
Cdf_clean$IsCENTER <- grepl("CENTER$", Cdf_clean$I.D_code)

# Remove Center from identity but keep for geometry
Cdf_clean$SubplotID <- Cdf_clean$I.D_code |>
  str_remove("CENTER$") |>
  str_remove("CR[0-9]+$") |>
  str_remove("Q[1-5]$")

#Convert to spatial points
pts <- vect(
  Cdf_clean,
  geom = c("Easting", "Northing"),
  crs  = "EPSG:32735"
)

# define subplot centroids
subplot_centers <- aggregate(
  pts[!pts$IsCENTER, ],
  by = "SubplotID",
  fun = mean
)

# Use center as shared geometry
build_subplot_polygon <- function(center, all_points, k = 4) {
  
  d <- distance(center, all_points)
  idx <- order(d)[1:k]
  p <- all_points[idx]
  
  if (length(p) < 4) return(NULL)
  
  poly <- convHull(p)
  poly$Area_m2 <- expanse(poly)
  
  poly
}

# apply 
subplot_polygons <- list()

for (sid in unique(subplot_centers$SubplotID)) {
  
  center <- subplot_centers[subplot_centers$SubplotID == sid, ]
  
  poly <- build_subplot_polygon(center, pts, k = 4)
  
  if (!is.null(poly)) {
    poly$SubplotID <- sid
    subplot_polygons[[sid]] <- poly
  }
}

# check 
subplots_final <- vect(subplot_polygons)
length(subplots_final)   

# Subplot area table 
subplots_final$Area_m2 <- expanse(subplots_final)

subplot_area_table <- data.frame(
  SubplotID = subplots_final$SubplotID,
  Total_Area_m2 = subplots_final$Area_m2
)


# keep only properly labelled rows
subplot_area_table_clean <- subset(
  subplot_area_table_clean,
  grepl("^S[A-Z]P[1-5]Z[1-4]$", SubplotID)
)

subplot_area_table_clean

# Save as csv file 
#write.csv(subplot_area_table_clean, "Subplot_total_area.csv", row.names = FALSE)


# Count subplots by area threshold

# subplots < 540 m²
n_less_540 <- sum(subplot_area_table_clean$Total_Area_m2 < 540)
n_less_540

## subplots > 660 m²
n_greater_660 <- sum(subplot_area_table_clean$Total_Area_m2 > 660)
n_greater_660

 #Subplots < 300 m²
n_less_300 <- sum(subplot_area_table_clean$Total_Area_m2 < 300)
 n_less_300

 #subplots less than 500 
 n_less_500 <- sum(subplot_area_table_clean$Total_Area_m2 < 500)
 
 
# Subplots > 700 m²
n_greater_700 <- sum(subplot_area_table_clean$Total_Area_m2 > 700)
n_



##### CONVERTING EACH SUBPLOT TO HECTARES
subplot_area_table_clean$Area_ha <-
  subplot_area_table_clean$Total_Area_m2 / 10000

# Construct full SubplotID (must match area table)
Seedlings <- Seedlings %>%
  mutate(
    SubplotID = paste0(Site, Plot, Subplot)
  )


#Join subplot area (m²) to weed counts
Seedlings <- Seedlings %>%
  left_join(
    subplot_area_table_clean %>%
      dplyr::select(SubplotID, Total_Area_m2),
    by = "SubplotID"
  )

# Convert area to hectares
Seedlings <- Seedlings %>%
  mutate(
    Area_ha = Total_Area_m2 / 10000
  )

# calculate seedling density per hectare
Seedlings <- Seedlings %>%
  mutate(
    density_ha = Seedlings / Area_ha
  )

# check 
summary(Seedlings$density_ha)

any(is.infinite(Weeds$density_ha))
any(is.na(Weeds$density_ha))





#
Seedlings_final <- Seedlings  %>%
  select(
    Site,
    Plot,
    Subplot,
    SubplotID,
    Treatment,
    Fencing,
    Year,
    Seedlings ,
    Total_Area_m2,
    Area_ha,
    density_ha
  )
