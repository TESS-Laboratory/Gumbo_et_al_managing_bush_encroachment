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


## Create ggplot objects with NO margins
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
    plot.tag = element_text(size = 10, face = 'plain', 
                            color = "black",  
                            margin = margin(0, 0, 0, 0)),
    plot.tag.position = c(0.1, 0.95),  
    plot.margin = margin(1, 1, 1, 1),
    panel.spacing = unit(0.1, "cm"),      # Gap between ALL panels (rows & columns)
    panel.spacing.x = unit(0.1, "cm"),    # Horizontal gap (between columns)
    panel.spacing.y = unit(0.1, "cm"),   # VERTICAL gap (between rows) - SMALLER!
    plot.background = element_blank()
    )
 
#plot.tag.background = element_rect(
 # fill = "white",
  #colour = NA

#### option 2 white background on annotations
Tmulti_plot <- (ppp1 + ppp2)/(ppp4 + ppp3)/(ppp5 + ppp6)/(ppp7+ppp8) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) &
  theme(
    # Annotation text
    plot.tag = element_text(
      size = 8,
      face = "bold",
      colour = "black",
      margin = margin(1, 2, 1, 2)   # small padding inside white box
    ),
    
    # Annotation position (inside image, top-left)
    plot.tag.position = c(0.04, 0.98),
    
    # Small white rectangle ONLY behind annotation
    plot.tag.background = element_rect(
      fill = "white",
      colour = "white"
    ),
    
    # Increase image area + hairline spacing
    plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(0.02, "cm")
  )

    
## Save the multi-panel plot
 ggsave( Tmulti_plot, filename = "Plots/Treatment MPanel6Ery_plot.png",width = 11, height = 16, units = "cm")


  
 
################ creating panel annotation with white background

 Tmulti_plot3 <- (ppp1 + ppp2)/(ppp4 + ppp3)/(ppp5 + ppp6)/(ppp7+ppp8) +
   theme(
     # Annotation text
     plot.tag = element_text(
       size = 8,
       face = "bold",
       colour = "black",
       margin = margin(1, 1, 1, 1)   # small padding inside white box
     ),
     
     # Increase image area + hairline spacing
     plot.margin = margin(0, 0, 0, 0),
     panel.spacing = unit(0.02, "cm")
   )
 
 
 ## Save the multi-panel plot
 ggsave( Tmulti_plot3, filename = "Plots/TreatmentMplot2.png",width = 11, height = 16, units = "cm")
 
 

########  white background on annotations 
 # Read image
 img <- image_read("Plots/TreatmentMPlot2.png")  # image with no annotations

  # Image dimensions
 info <- image_info(img)
 w <- info$width
 h <- info$height
 
 # Panel geometry
 n_col <- 2
 n_row <- 4
 panel_w <- w / n_col
 panel_h <- h / n_row
 
 labels <- paste0("(", letters[1:8], ")")
 
 # Padding inside each panel (adjust if needed)
 x_pad <- 20
 y_pad <- 40
 
 # Loop over panels
 k <- 1
 for (row in 0:(n_row - 1)) {
   for (col in 0:(n_col - 1)) {
     
     # Hairline gap between rows (pixels)
     #row_gap <- 24   #
     
     x <- col * panel_w + x_pad
     y <- row * panel_h + y_pad
     #y <- row * panel_h + y_pad + row * row_gap # adding hairline gap between rows
     
     img <- image_annotate(
       img,
       text = labels[k],
       size = 30,
       weight = 400,
       location = paste0("+", round(x), "+", round(y)),
       color = "black",
       boxcolor = "white"
     )
     
     k <- k + 1
   }
 }
 
 # Save annotated figure
 image_write(img, "Plots/Tmultipanel_annotated3Ja.png")
 

 
 
 
 
 
 
 
 
######################################################################################

##### Creating multi-panel for SHR woody cover map and paddocks

## Read images
imgWC <- image_read("DATA/SHR_Map_pics/SHR_woody_cover_map.png") 
imgPD <- image_read("DATA/SHR_Map_pics/SHR_Paddock.png")


gt <- image_read(file.path(img_dir, "gt.png"))
gm <- image_read(file.path(img_dir, "gm.png"))

p_gt <- ggplot() +
  annotation_custom(rasterGrob(as.raster(imgWC))) +
  theme_void()

p_gm <- ggplot() +
  annotation_custom(rasterGrob(as.raster(imgPD))) +
  theme_void()

pmulti_panel <- p_gt / p_gm +
  plot_annotation(tag_levels = "a")


####### second option 

##
pic1 <- rasterGrob(as.raster(imgWC))
pic2 <- rasterGrob(as.raster(imgPD))

###
p1 <- ggplot() + annotation_custom(pic1) + theme_void()
p2 <- ggplot() + annotation_custom(pic2) + theme_void()


# Arrange in grid

WCPD <- p1/p2 +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 12, hjust = 0))  # Left align tags
  )   

# saving
ggsave(WCPD, filename = "Plots/Cover map3.png", width = 16, height = 14, units = "cm")




##### Trying option 2



img_dir <- "DATA/SHR_Map_pics"

img_files <- c(
 file.path(img_dir,"SHR2_covermap.png"), 
 file.path(img_dir, "Annual_woody_cover.png")
)



plots <- lapply(img_files, function(x) {
  ggdraw() +
    draw_image(x, scale = 1) +
    theme(plot.margin = margin(0, 0, 0, 0))
})

multi_panel4 <- plot_grid(
  plotlist = plots,
  ncol = 1,
  label_x = 0.02,   # flush to left
  label_y = 0.98,   # flush to top
  hjust = 0,
  vjust = 1,
  label_size = 14
) 

#save
 ggsave(multi_panel4, filename = "Plots/Cover map5.png", width = 16, height = 14, units = "cm")

 
 #### Annotating  the image
 ## # Read image
 img <- image_read("Plots/Cover map5.png")  # image with no annotations
 
 # Image dimensions
 info <- image_info(img)
 w <- info$width
 h <- info$height
 
 # Panel geometry
 n_col <- 1
 n_row <- 2
 panel_w <- w / n_col
 panel_h <- h / n_row
 
 labels <- paste0("(", letters[1:2], ")")
 
 # Padding inside each panel (adjust if needed)
 x_pad <- 20
 y_pad <- 40
 
 # Loop over panels
 k <- 1
 for (row in 0:(n_row - 1)) {
   for (col in 0:(n_col - 1)) {
     
     # Hairline gap between rows (pixels)
     #row_gap <- 24   #
     
     x <- col * panel_w + x_pad
     y <- row * panel_h + y_pad
     #y <- row * panel_h + y_pad + row * row_gap # adding hairline gap between rows
     
     img <- image_annotate(
       img,
       text = labels[k],
       size = 30,
       weight = 400,
       location = paste0("+", round(x), "+", round(y)),
       color = "black",
       boxcolor = "white"
     )
     
     k <- k + 1
   }
 }
 
 
 
 # # Shift annotations 10% to the right
 x_shift <- 0.20 * panel_w
 
 k <- 1
 for (row in 0:(n_row - 1)) {
   for (col in 0:(n_col - 1)) {
     
     x <- col * panel_w + x_pad + x_shift
     y <- row * panel_h + y_pad
     
     img <- image_annotate(
       img,
       text = labels[k],
       size = 30,
       weight = 400,
       location = paste0("+", round(x), "+", round(y)),
       color = "black",
       boxcolor = "white"
     )
     
     k <- k + 1
   }
 }
 
 # Save annotated figure
 image_write(img, "Plots/3BCoverpanel_annotated.png")
 
