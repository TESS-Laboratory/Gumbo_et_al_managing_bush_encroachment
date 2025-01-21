## using ordinal scale observations for fuel classes (grasses, shrubs, trees)unburnt

library(tidyverse) 

Fire <- read.csv("C:/workspace/gumbo_dev/DATA/Fire.csv")

str(Fire)

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

# Ensure numeric columns and handle non-numeric values
data <- Fire %>%
  mutate(
    Fine = as.numeric(Fine),
    Coarse = as.numeric(Coarse),
    Woody = as.numeric(Woody)
  )

# Pivot longer
data_long <- data %>%
  pivot_longer(
    cols = c(Fine, Coarse, Woody),
    names_to = "Fuel_Type",
    values_to = "Value"
  )

# Summarize the data
site_summary <- data_long %>%
  group_by(SITE, Fuel_Type) %>%
  summarize(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop")


# Create the bar plot
ggplot(site_summary, aes(x = SITE, y = Mean_Value, fill = Fuel_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Site",
    y = "Mean ordinal value",
    fill = "Fuel classes"
  ) +
  theme_beautiful()+  theme(legend.position = "right")
)


