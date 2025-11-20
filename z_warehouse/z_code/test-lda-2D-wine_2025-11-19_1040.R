# test_wine_lda.R
# Test script for LDA segment visualization with Wine Quality data

library(here)
library(MASS)
library(tidyverse)
source(here("code", "lda-2D.R"))
source(here("code", "wine_quality_uci.R"))

# Load Wine Quality data
# wine_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
# wine_data <- read_delim(wine_url, delim = ";", show_col_types = FALSE)

## 
#  wq_data: wine quality data
#    replace "wine_quality" with "wq_data"
## 
wine_quality  <- get_wine_quality()
wq_abbrev_tbl <- abbreviate_wq_var_names()
wq_data <- wine_quality
names(wq_data) <- wq_abbrev_tbl$ abbrev
rm(wine_quality)

# Create 3-level quality classification for more interesting boundaries
wq_data <- wq_data |>
  mutate(qual_fct = factor(case_when(
    quality <= 5 ~ "low",
    quality >= 7 ~ "high", 
    TRUE         ~ "medium"
  ), levels = c("low", "medium", "high")))

table(wq_data$qual_fct)

# Fit LDA using two features (for 2D visualization)
lda_fit <- lda(qual_fct ~ alcohol + vol_acidity, data = wq_data)

# Create 2D data for plotting (using original feature space)
xy_data <- tibble::tibble(
  x     = wq_data$alcohol,
  y     = wq_data$vol_acidity,
  class = wq_data$qual_fct
)

# Get decision boundary coefficients
cat("Computing decision boundaries...\n")
line_coefs <- l2D_get_xy_stats(lda_fit)
print(line_coefs)

# Create segments trimmed at intersections
cat("\nCreating segments...\n")
segments <- l2D_make_segments(line_coefs, xy_data, expand_factor = 0.05)
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
here::here("images", "wine_quality_boundaries.png") |> 
  ggsave(
    p, width = 10, height = 7, dpi = 150)
cat("\nPlot saved as 'wine_quality_boundaries.png'\n")

# Alternative: Try with color classification (binary)
cat("\n\n--- Binary classification example ---\n")
wine_url_white <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
wine_white <- read_delim(wine_url_white, delim = ";", show_col_types = FALSE)

# Combine red and white wines
# wine_combined <- bind_rows(
#   mutate(wq_data, color = "red"),
#   mutate(wine_white, color = "white", qual_fct = NULL)
# ) |>
#   select(alcohol, vol_acidity, color)

# Fit LDA for wine color
lda_color <- lda(color ~ alcohol + vol_acidity, data = wq_data)

xy_color <- tibble::tibble(
  x     = wq_data$ alcohol,
  y     = wq_data$ vol_acidity,
  class = wq_data$ color
)

line_coefs_color <- l2D_get_xy_stats(lda_color)
segments_color   <- l2D_make_segments(line_coefs_color, xy_color)

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
here::here("images", "wine_color_boundary.png") |> 
  ggsave(
    p2, width = 10, height = 7, dpi = 150)
cat("Plot saved as 'wine_color_boundary.png'\n")
