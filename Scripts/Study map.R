### CREATING STUDY MAP 

# Load libraries
library(ggplot2)
# spatial data handling
library(sf)           
# combining plots
library(cowplot)       
library(grid)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(patchwork)


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


####################################################################################

 ## OPTION 6 

# Study area boundary
study_boundary <-  st_read("DATA/CoordinatesS/Boundary shpedt/Boundary shpedt.shp")

# Roads
roads <- st_read("DATA/CoordinatesS/Boundary shpedt/SR_roads.kml", quiet = TRUE)

# Water sources
water_surfaces <- st_read("DATA/CoordinatesS/Boundary shpedt/SR_water tanks.kml", quiet = TRUE)

# dams
dams <- st_read("DATA/CoordinatesS/Boundary shpedt/SR_dams.kml", quiet = TRUE)

# Transform KML layers to match boundary CRS
roads <- st_transform(roads, st_crs(study_boundary))
water_surfaces <- st_transform(water, st_crs(study_boundary))
dams <- st_transform(dams, st_crs(study_boundary))

# transform to km
roads <- st_transform(roads,32736)
water_surfaces <- st_transform(water,32736)
dams <- st_transform(dams,32736)



# Study location points
study_csv <- read_csv("DATA/SHR_QgisMAPS/RefSHR_samplingsites.csv")


#Convert CSV to spatial point (WGS84)
study_point <- st_as_sf(
  study_csv,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

#Transform study point to boundary CRS
study_point <- st_transform(study_point, st_crs(study_boundary))

## Create bounding box from study boundary
bb <- st_bbox(study_boundary)


#Load Africa and Zimbabwe boundaries
africa <- ne_countries(
  continent = "Africa",
  returnclass = "sf"
)


# CRreate stand alone africa map
africa <- ne_countries(continent = "Africa", returnclass = "sf")
zimbabwe <- africa[africa$name == "Zimbabwe", ]

# Fix limits to only show Africa properly
map_africa <- ggplot() +
  geom_sf(data = africa, fill = "white", color = "black", linewidth = 0.2) +
  geom_sf(data = zimbabwe, fill = NA, color = "black", linewidth = 0.3) +
  geom_sf(data = study_point, color = "green", size = 0.5) +
  labs(title = "(a)") +
  coord_sf(xlim = c(-20, 55), ylim = c(-40, 40), expand = FALSE) + # Africa bounds
  theme_void() #+

  #theme(
   # plot.title = element_text(face = "bold", hjust = 0.5),
    #panel.border = element_rect(color = "black", fill = NA)
  #) + 
  
  # ---- SCALE BAR ----
annotation_scale(
  location = "br",        # bottom-left
  width_hint = 0.2,      # relative width
  text_cex = 0.4,         # scale text size
  line_width = 0.3
) 

##Create study location map (stand-alone, large)
# option 2 tempering with coordinates font size
map_study <- ggplot() +
  geom_sf(data = study_boundary, aes(color = "SHR boundary"), fill = NA, linewidth = 0.6) +
  geom_sf(data = roads, aes(color = "Roads"), linewidth = 0.4) +
  geom_sf(data = dams, aes(color = "Dams"), linewidth = 0.6) +
  #geom_sf(data = study_point, aes(color = "Sampling sites"), size = 0.5) +
  
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      #"Study site" = "green",
      "SHR boundary" = "black",
      "Roads" = "brown",
      "Dams" = "blue"
     # "Sampling sites" = "green"
    )
  ) +
  
  labs(
    title = "(b)",
    color = "Legend"#,
    #x = "Longitude",
    #y = "Latitude"
  ) +
  
  # ---- SCALE BAR (PANEL B ONLY) ----
annotation_scale(
  location = "bl",
  width_hint = 0.35, #0.25
  bar_cols = c("black", "white"),
  line_width = 0.8,
  height = unit(0.25, "cm"),
  text_cex = 0.8,
  text_col = "black",     # <-- FORCES LABEL VISIBILITY
  line_col = "black",
  unit_category = "metric",
  pad_x = unit(0.4, "cm"),
  pad_y = unit(0.4, "cm")
)+
  
  theme_bw() +
  theme(
    #plot.title = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(size = 15, face = "plain", hjust = 0.1),
    # ---- COORDINATE FORMATTING (THIS IS THE KEY PART) ----
    axis.text.x = element_text(size = 6, angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 6, angle = 90, vjust = 0.5),
    
    #axis.title.x = element_text(size = 9),
    #axis.title.y = element_text(size = 9),
    
    panel.grid.major = element_line(color = "grey90"),
    panel.background = element_rect(fill = "white")
  )


# Combine maps side by side with fixed relative sizes
final_map <- plot_grid(
  map_africa, map_study,
  ncol = 2,
  rel_widths = c(1, 4)
)

# saving
ggsave(
  filename = "Plots/SHR34study_map.png",
  plot = final_map,
  width = 16,
  height = 12,
  units = "cm"
)
