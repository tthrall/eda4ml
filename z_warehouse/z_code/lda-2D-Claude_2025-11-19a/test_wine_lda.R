# test_wine_lda.R
# Test script for LDA segment visualization with Wine Quality data

library(MASS)
library(tidyverse)
source("lda_2D.R")  # Load our functions

# Load Wine Quality data
wine_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
wine_data <- read_delim(wine_url, delim = ";", show_col_types = FALSE)

# Create 3-level quality classification for more interesting boundaries
wine_data <- wine_data |>
  mutate(quality_class = factor(case_when(
    quality <= 5 ~ "low",
    quality == 6 ~ "medium",
    quality >= 7 ~ "high"
  ), levels = c("low", "medium", "high")))

table(wine_data$quality_class)

# Fit LDA using two features (for 2D visualization)
lda_fit <- lda(quality_class ~ alcohol + volatile.acidity, data = wine_data)

# Create 2D data for plotting (using original feature space)
xy_data <- data.frame(
  x = wine_data$alcohol,
  y = wine_data$volatile.acidity,
  class = wine_data$quality_class
)

# Get decision boundary coefficients (geometric, through midpoints)
cat("Computing geometric decision boundaries...\n")
line_coefs <- l2D_get_xy_stats(lda_fit)
cat("\nGeometric boundaries (equal priors assumed):\n")
print(line_coefs)

# Adjust for actual prior probabilities
cat("\nAdjusting for observed prior probabilities...\n")
line_coefs_adjusted <- l2D_adjust_for_priors(line_coefs, lda_fit)
cat("\nAdjusted boundaries (accounting for class frequencies):\n")
print(line_coefs_adjusted)

# Create segments trimmed at intersections
cat("\nCreating segments...\n")
segments <- l2D_make_segments(line_coefs_adjusted, xy_data, expand_factor = 0.05)
print(segments)

# Create visualization
p <- ggplot(xy_data, aes(x = x, y = y, color = class)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_segment(
    data = segments,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "black", 
    linewidth = 1.2, 
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    values = c("low" = "#d73027", "medium" = "#fee090", "high" = "#4575b4")
  ) +
  labs(
    title = "LDA Decision Boundaries for Wine Quality",
    subtitle = "Segments trimmed at intersections",
    x = "Alcohol (%)",
    y = "Volatile Acidity (g/dm³)",
    color = "Quality"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(p)

# Save plot
ggsave("wine_lda_boundaries.png", p, width = 10, height = 7, dpi = 150)
cat("\nPlot saved as 'wine_lda_boundaries.png'\n")

# Alternative: Try with color classification (binary)
cat("\n\n--- Binary classification example ---\n")
wine_url_white <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
wine_white <- read_delim(wine_url_white, delim = ";", show_col_types = FALSE)

# Combine red and white wines
wine_combined <- bind_rows(
  mutate(wine_data, color = "red"),
  mutate(wine_white, color = "white", quality_class = NULL)
) |>
  select(alcohol, volatile.acidity, color)

# Fit LDA for wine color
lda_color <- lda(color ~ alcohol + volatile.acidity, data = wine_combined)

xy_color <- data.frame(
  x = wine_combined$alcohol,
  y = wine_combined$volatile.acidity,
  class = wine_combined$color
)

line_coefs_color <- l2D_get_xy_stats(lda_color)
line_coefs_color <- l2D_adjust_for_priors(line_coefs_color, lda_color)
segments_color <- l2D_make_segments(line_coefs_color, xy_color)

p2 <- ggplot(xy_color, aes(x = x, y = y, color = class)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_segment(
    data = segments_color,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "black",
    linewidth = 1.5,
    linetype = "solid",
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = c("red" = "#d73027", "white" = "#fee090")) +
  labs(
    title = "LDA Decision Boundary for Wine Color",
    x = "Alcohol (%)",
    y = "Volatile Acidity (g/dm³)",
    color = "Wine Type"
  ) +
  theme_minimal()

print(p2)
ggsave("wine_color_boundary.png", p2, width = 10, height = 7, dpi = 150)
cat("Plot saved as 'wine_color_boundary.png'\n")
