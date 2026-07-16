# Load required library
library(tidyverse)
library(ggplot2)

# Set the values
T <- 59
H <- 64
F <- 34
B <- 6000

# Create a data frame with the combinations
combinations <- data.frame(
  Category = c("F", "TF", "TFB", "THF"),
  Cost = c(F, T + F, T + F + B, T + H + F)
)



# Create the plot
ggplot(combinations, aes(x = Category, y = Cost, fill = Category)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0("$", Cost)), vjust = -0.5, size = 5) +
  labs(
    x = "Treatment",
    y = "Estimated treatment cost per hectare"#,
    #caption = paste("T =", T, ", H =", H, ", F =", F, ", B =", B)
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, max(combinations$Cost) * 1.15))


# saving 
ggsave("Plots/Estimated treatment cost.png",
       width = 16, height = 10, units = "cm", dpi = 300, bg = "white")