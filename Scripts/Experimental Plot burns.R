## SITE AND PLOT LEVEL ANALYSIS

library(tidyverse)



dataG <- read.csv("C:/workspace/gumbo_dev/DATA/Plot burnsSHR.csv")

str(dataG)

## using theme beautiful
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

# Summarize grasses by site
site_summary <- dataG %>%
  group_by(SITE) %>%
  summarise(
    Mean_Grasses = mean(Grasses, na.rm = TRUE),
    Median_Grasses = median(Grasses, na.rm = TRUE),
    SD_Grasses = sd(Grasses, na.rm = TRUE),
    Min_Grasses = min(Grasses, na.rm = TRUE),
    Max_Grasses = max(Grasses, na.rm = TRUE),
    Count = n()
  )

# Print the site-level summary
print(site_summary)

# Summarize grasses by site and plot
site_plot_summary <- dataG %>%
  group_by(SITE, PLOT) %>%
  summarise(
    Mean_Grasses = mean(Grasses, na.rm = TRUE),
    Median_Grasses = median(Grasses, na.rm = TRUE),
    SD_Grasses = sd(Grasses, na.rm = TRUE),
    Min_Grasses = min(Grasses, na.rm = TRUE),
    Max_Grasses = max(Grasses, na.rm = TRUE),
    Count = n()
  )

# Print the site and plot-level summary
print(site_plot_summary)

# Visualize grasses by site using a boxplot
ggplot(dataG, aes(x = SITE, y = Grasses)) +
  geom_boxplot() +
  theme_beautiful() +
  labs(
       x = "Site",
       y = "Grasses")

# Visualize grasses by site and plot
Plotburns<- ggplot(dataG, aes(x = factor(PLOT), y = Grasses, fill = SITE)) +
  geom_boxplot() +
  theme_beautiful() + theme(legend.position = "right")+
  labs(
    x = "Plot",
    y = "Unburnt grasses (%)",
    fill = "Site")

# Save as JPEG
ggsave(filename = "Unburnt_Grasses_Plot.jpeg", plot = Plotburns, width = 8, height = 6, dpi = 300)