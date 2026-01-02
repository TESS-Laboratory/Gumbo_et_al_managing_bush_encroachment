library(tidyverse)
library(multcompView)
library(patchwork)
library(magick)
library(cowplot)
library(grid)

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

# Read images
img1 <- image_read("DATA/DPM pics/1.JPG")
img2 <- image_read("DATA/DPM pics/2.JPG") 
img3 <- image_read("DATA/DPM pics/3.JPG")
img4 <- image_read("DATA/DPM pics/4b.png") 

## if there is need to resize the image;  cropping an image
#  Crop by percentage (e.g., remove 30% from right)
img4_cropped <- image_crop(img4, geometry_area(
  width = image_info(img4)$width * 0.7,  # Keep 70% width
  height = image_info(img4)$height,
  x_off = 0,  # Start from left
  y_off = 0
))

# Convert to raster objects
g1 <- rasterGrob(as.raster(img1))
g2 <- rasterGrob(as.raster(img2))
g3 <- rasterGrob(as.raster(img3))
#g4 <- rasterGrob(as.raster(img4))
g4 <- rasterGrob(as.raster(img4_cropped))  # Use cropped version


# Create ggplot objects
p1 <- ggplot() + annotation_custom(g1) + theme_void()
p2 <- ggplot() + annotation_custom(g2) + theme_void()
p3 <- ggplot() + annotation_custom(g3) + theme_void()
p4 <- ggplot() + annotation_custom(g4) + theme_void()

# Arrange in 2x2 grid
multi_plot1 <- (p1 + p2)/(p3 + p4)+
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 12, face = 'bold', 
                                margin = margin(0, 0, 0, 0))

# Save the multi-panel plot
ggsave("Plots/DPM MPanel5_plot.png", multi_plot1, width = 16, height = 14, units = "cm")



## Another way of recreating objects for p1 to p4.Create ggplot objects with NO margins
p1 <- ggplot() + 
  annotation_custom(g1, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
  theme_beautiful() +
  theme(plot.margin = margin(0, 0, 0, 0))

p2 <- ggplot() + 
  annotation_custom(g2, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
  theme_beautiful() +
  theme(plot.margin = margin(0, 0, 0, 0))

p3 <- ggplot() + 
  annotation_custom(g3, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
  theme_beautiful() +
  theme(plot.margin = margin(0, 0, 0, 0))

p4 <- ggplot() + 
  annotation_custom(g4, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
  theme_beautiful() +
  theme(plot.margin = margin(0, 0, 0, 0))

# Arrange panels with minimal gaps
multi_plott <- (p1 + p2) / (p3 + p4) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') &
  theme(
    plot.tag = element_text(size = 12, face = 'bold', 
                            color = "black",  
                            margin = margin(0, 0, 0, 0)),
    plot.tag.position = c(0.1, 0.95),  
    plot.margin = margin(1, 1, 1, 1),
    panel.spacing = unit(1.0, "cm"),  # Minimal gap between panels
    plot.background = element_blank()
  )

# Save
  #ggsave("Plots/DPMmulti_panel_tight.png", multi_plott, 
       width = 16, height = 14, units = "cm", bg = "white")




############################################ CREATING MULTI-PANEL FOR TREATMENTS

## Read images
imgT1 <- image_read("DATA/Treatments/Thinning1.jpeg") 
imgT2 <- image_read("DATA/Treatments/Thinning2.JPG")
imgH3 <- image_read("DATA/Treatments/Herbicide1.JPG")
imgH4 <- image_read("DATA/Treatments/Herbicide2.JPG")
imgF5 <- image_read("DATA/Treatments/Fire1.JPG")
imgF6 <- image_read("DATA/Treatments/fire2.JPG") 
imgG7 <- image_read("DATA/Treatments/Goats1.JPG")
imgG8 <- image_read("DATA/Treatments/Goats2.JPG")


# Convert to raster objects
gg1 <- rasterGrob(as.raster(imgT1))
gg2 <- rasterGrob(as.raster(imgT2))
gg3 <- rasterGrob(as.raster(imgH3))
gg4 <- rasterGrob(as.raster(imgH4))
gg5 <- rasterGrob(as.raster(imgF5))  
gg6 <- rasterGrob(as.raster(imgF6))  
gg7 <- rasterGrob(as.raster(imgG7))  
gg8 <- rasterGrob(as.raster(imgG8))  


# Create ggplot objects
pp1 <- ggplot() + annotation_custom(gg1) + theme_void()
pp2 <- ggplot() + annotation_custom(gg2) + theme_void()
pp3 <- ggplot() + annotation_custom(gg3) + theme_void()
pp4 <- ggplot() + annotation_custom(gg4) + theme_void()
pp5 <- ggplot() + annotation_custom(gg5) + theme_void()
pp6 <- ggplot() + annotation_custom(gg6) + theme_void()
pp7 <- ggplot() + annotation_custom(gg7) + theme_void()
pp8 <- ggplot() + annotation_custom(gg8) + theme_void()

# Arrange in 2x2 grid
multi_Tplot <- (pp1 + pp2)/(pp3 + pp4)/(pp5 + pp6)/(pp7+pp8) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 12, face = 'bold', 
                  margin = margin(0, 0, 0, 0)))
        
# Save the multi-panel plot
ggsave("Plots/Treatment MPanel_plot.png", multi_Tplot, width = 16, height = 14, units = "cm")


## Another way of recreating objects for p1 to p4.Create ggplot objects with NO margins
ppp1 <- ggplot() + 
  annotation_custom(gg1, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
  theme_beautiful() +
  theme(plot.margin = margin(0, 0, 0, 0))

ppp2 <- ggplot() + 
  annotation_custom(gg2, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
  theme_beautiful() +
  theme(plot.margin = margin(0, 0, 0, 0))

ppp3 <- ggplot() + 
  annotation_custom(gg3, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
  theme_beautiful() +
  theme(plot.margin = margin(0, 0, 0, 0))

ppp4 <- ggplot() + 
  annotation_custom(gg4, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
  theme_beautiful() +
  theme(plot.margin = margin(0, 0, 0, 0))

ppp5 <- ggplot() + 
  annotation_custom(gg5, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
  theme_beautiful() +
  theme(plot.margin = margin(0, 0, 0, 0))

ppp6 <- ggplot() + 
  annotation_custom(gg6, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
  theme_beautiful() +
  theme(plot.margin = margin(0, 0, 0, 0))

ppp7 <- ggplot() + 
  annotation_custom(gg7, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
  theme_beautiful() +
  theme(plot.margin = margin(0, 0, 0, 0))

ppp8 <- ggplot() + 
  annotation_custom(gg8, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
  theme_beautiful() +
  theme(plot.margin = margin(0, 0, 0, 0))



# Arrange panels with minimal gaps
Tmulti_plot <- (ppp1 + ppp2)/(ppp4 + ppp3)/(ppp5 + ppp6)/(ppp7+ppp8) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') &
  theme(
    plot.tag = element_text(size = 12, face = 'bold', 
                            color = "black",  
                            margin = margin(0, 0, 0, 0)),
    plot.tag.position = c(0.1, 0.95),  
    plot.margin = margin(1, 1, 1, 1),
    panel.spacing = unit(0.01, "cm"),      # Gap between ALL panels (rows & columns)
    panel.spacing.x = unit(0.01, "cm"),    # Horizontal gap (between columns)
    panel.spacing.y = unit(0.01, "cm"),   # VERTICAL gap (between rows) - SMALLER!
    plot.background = element_blank()
  )

## Save the multi-panel plot
ggsave( Tmulti_plot, filename = "Plots/Treatment MPanel6c_plot.png",width = 12, height = 16, units = "cm")



