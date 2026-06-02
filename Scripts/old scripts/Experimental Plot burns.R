library(tidyverse) 

dataGGG <- read.csv("C:/workspace/gumbo_dev/DATA/Plot burnsSHR.csv")

str(dataGGG)

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
site_summary <- dataGGG %>%
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


# Visualize grasses by site and plot
ggplot(dataGGG, aes(x = factor(PLOT), y = Grasses, fill = SITE)) +
  geom_boxplot() +
  theme_beautiful() + theme(legend.position = "right")+
  labs(
    x = "Plot",
    y = "Unburnt grasses (%)",
    fill = "Site")




# Convert Grasses, Shrubs, and Trees to numeric
dataConv <- dataGGG %>%
  mutate(
    Grasses = as.numeric(as.character(Grasses)),
    Shrubs = as.numeric(as.character(Shrubs)),
    Trees = as.numeric(as.character(Trees))
  )

# Summarize Shrubs by SITE and PLOT
summary_shrubs <- dataConv %>%
  group_by(SITE, PLOT) %>%
  summarise(
    Mean_Shrubs = mean(Shrubs, na.rm = TRUE)
  )

print(summary_shrubs)



# Create a grouped bar chart
ggplot(summary_shrubs, aes(x = factor(PLOT), y = Mean_Shrubs, fill = SITE)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_beautiful() +
  labs(
    x = "Plot",
    y = "Unburnt shrubs (%)",
    fill = "Site"
  ) +
  theme(
    legend.position = "right"
  )

# Create a grouped bar chart. modified bar chart to ensure plot 5 has all its columns in one bar
ggplot(summary_shrubs, aes(x = factor(PLOT), y = Mean_Shrubs, fill = SITE)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_beautiful() +
  labs(
    x = "Plot",
    y = "Unburnt shrubs (%)",
    fill = "Site"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right" + 
      facet_grid(~factor(ifelse(PLOT=="5", "Plot 5", "Other Plots")))
    
    
    ###ATTEMPT 2 TO ENSURE PLOT 5 has all variable responses in one column
    
    # Summarize Shrubs by SITE and PLOT
    summary_shrubs <- dataConv %>%
      group_by(SITE, PLOT) %>%
      summarise(
        Mean_Shrubs = mean(Shrubs, na.rm = TRUE)
      )
    
    
    print(summary_shrubs)
    
    
    ggplot(summary_shrubs, aes(x = factor(PLOT), y = Mean_Shrubs, fill = SITE)) +
      geom_bar(stat = "identity", position = "dodge") +
      #facet_wrap(~ PLOT, scales = "free_x") +  # This will create a facet for each plot
      theme_beautiful() +
      labs(
        x = "Plots",
        y = "Unburnt shrubs (%)",
        fill = "Site"
      ) +
      theme(
        #axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
  
    ######### TREES TREES TREES  
    
    # Summarize Shrubs by SITE and PLOT
    summary_trees <- dataConv %>%
      group_by(SITE, PLOT) %>%
      summarise(Mean_Trees = mean(Trees, na.rm = TRUE))
    
    
    print(summary_trees)
    
    
    ggplot(summary_trees, aes(x = factor(PLOT), y = Mean_Trees, fill = SITE)) +
      geom_bar(stat = "identity", position = "dodge") +
      #facet_wrap(~ PLOT, scales = "free_x") +  # This will create a facet for each plot
      theme_beautiful() +
      labs(
        x = "Plots",
        y = "Unburnt trees (%)",
        fill = "Site"
      ) +
      theme(
        #axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
    