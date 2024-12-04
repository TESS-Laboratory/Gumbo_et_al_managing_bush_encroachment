library(tidyverse)

install.packages("reshape2")

# Load the necessary library
library(ggplot2)

# Create the data frame
woodyparameters <- data.frame(
  Site = c("A", "B", "C", "D", "E", "F"),
  Seedlings = c(1724, 1990, 2151, 1671, 3588, 990),
  Saplings = c(2896, 1289, 972, 2191, 1545, 1361),
  Trees = c(2224, 1618, 3299, 2985, 3127, 2044))

# Convert the data to long format for ggplot
data_long <- reshape2::melt(woodyparameters, id.vars = "Site")

# Plot the data
ggplot(data_long, aes(x = Site, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Site",
    y = "Count",
    fill = "Legend"
  ) +
  theme_classic()

####summary data
summary(data)

#### Comparative analysis with visualization
# Load necessary libraries
library(ggplot2)
library(reshape2)

# Create the data frame
data <- data.frame(
  Site = c("A", "B", "C", "D", "E", "F"),
  Trees = c(2224, 1618, 3299, 2985, 3127, 2044),
  Stem_count = c(16214, 14410, 22004, 19852, 18130, 13718),
  Seedlings = c(1724, 1990, 2151, 1671, 3588, 990),
  Saplings = c(2896, 1289, 972, 2191, 1545, 1361)
)

# Convert the data to long format for ggplot
data_long <- melt(data, id.vars = "Site")

# Plot the data
ggplot(data_long, aes(x = Site, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Number of Trees, Stem_count, Seedlings, and Saplings by Site",
    x = "Site",
    y = "Count",
    fill = "Category"
  ) +
  theme_minimal()



