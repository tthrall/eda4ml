###
#  lda_three_visualizations.R
#
#  Demonstrate the relationship between:
#    (1) Pairwise decision boundaries in original feature space
#    (2) LD1 and LD2 axes as direction vectors
#    (3) Data projected onto (LD1, LD2) discriminant space
#
#  For Chapter 7: Understanding LDA output from MASS::lda()
###

library(tidyverse)
library(MASS)

# Source your lda_2D.R functions
# source("lda_2D.R")

## 
#  Load and prepare wine quality data
## 

# Assuming you have a wine quality dataset
# Here's how to load it if it's in your data directory:
# wine <- read_csv("../data/wine_quality.csv")

# For this example, I'll create a subset with 3 quality levels
# You should replace this with your actual data loading:

# Example data preparation (replace with your actual code):
# wine <- your_wine_data %>%
#   mutate(
#     q_level = case_when(
#       quality <= 5 ~ "low",
#       quality == 6 ~ "medium",
#       quality >= 7 ~ "high"
#     ),
#     q_level = factor(q_level, levels = c("low", "medium", "high"))
#   )

# For demonstration, let's assume your wine data is loaded as 'wqual_z'
# with columns: alcohol, res_sugar (residual sugar), q_level


## 
#  Step 1: Compute pairwise boundaries using your functions
## 

# Get bounding box segments for decision boundaries
bb_segs_lst <- wqual_z %>% 
  get_bb_segs(
    x_1     = alcohol, 
    x_2     = res_sugar, 
    y_group = q_level
  )

# Extract the segments table
bb_segs_tbl <- bb_segs_lst$bb_segs_tbl

# Also extract means and covariance for later use
x_means <- bb_segs_lst$x_means
x_cov   <- bb_segs_lst$x_cov


## 
#  Step 2: Fit MASS::lda() to get LD axes
## 

# Fit LDA model
lda_fit <- lda(
  q_level ~ alcohol + res_sugar, 
  data = wqual_z
)

# Extract key components
cat("\n=== LDA Output from MASS::lda() ===\n")
cat("\nScaling matrix (LD coefficients):\n")
print(lda_fit$scaling)
cat("\nGroup means in original space:\n")
print(lda_fit$means)
cat("\nPrior probabilities:\n")
print(lda_fit$prior)


## 
#  Step 3: Extract LD direction vectors
## 

# LD1 and LD2 are columns of the scaling matrix
LD1_direction <- lda_fit$scaling[, "LD1"]
LD2_direction <- lda_fit$scaling[, "LD2"]

cat("\n=== LD Direction Vectors ===\n")
cat("LD1 direction:", LD1_direction, "\n")
cat("LD2 direction:", LD2_direction, "\n")

# Overall centroid (origin for LD vectors)
overall_centroid <- colMeans(
  wqual_z %>% select(alcohol, res_sugar), 
  na.rm = TRUE
)

cat("Overall centroid:", overall_centroid, "\n")

# Scale factors for visualization (adjust arrow length)
scale_LD1 <- 2.0
scale_LD2 <- 2.0


## 
#  Step 4: Project data onto LD space
## 

# Get projections
lda_proj <- predict(lda_fit)

# Create data frame for projected data
proj_df <- tibble(
  LD1     = lda_proj$x[, "LD1"],
  LD2     = lda_proj$x[, "LD2"],
  q_level = wqual_z$q_level
)

# Compute group centroids in LD space
proj_centroids <- proj_df %>%
  group_by(q_level) %>%
  summarise(
    LD1_mean = mean(LD1),
    LD2_mean = mean(LD2)
  )


## 
#  Visualization 1: Decision Boundaries in Original Space
## 

p1 <- ggplot() +
  # Data points
  geom_point(
    data = wqual_z,
    aes(x = alcohol, y = res_sugar, color = q_level),
    alpha = 0.6,
    size = 2
  ) +
  # Pairwise decision boundaries
  geom_segment(
    data = bb_segs_tbl %>% 
      filter(!is.na(x), !is.na(y), !is.na(xend), !is.na(yend)),
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "black",
    linewidth = 1,
    linetype = "solid"
  ) +
  # Add labels for boundaries
  geom_text(
    data = bb_segs_tbl %>% 
      filter(!is.na(x), !is.na(y)) %>%
      mutate(
        mid_x = (x + xend) / 2,
        mid_y = (y + yend) / 2,
        label = paste0(q_level_1, " vs ", q_level_2)
      ),
    aes(x = mid_x, y = mid_y, label = label),
    size = 3,
    nudge_y = 0.3,
    color = "black"
  ) +
  scale_color_manual(
    values = c("low" = "#D55E00", "medium" = "#E69F00", "high" = "#009E73")
  ) +
  labs(
    title = "Visualization 1: LDA Decision Boundaries",
    subtitle = "Pairwise discriminant functions in original feature space",
    x = "Alcohol (%)",
    y = "Residual Sugar (g/L)",
    color = "Wine Quality"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

print(p1)


## 
#  Visualization 2: LD Axes Overlaid on Original Space
## 

# Create arrows data frame for LD directions
ld_arrows <- tibble(
  axis = c("LD1", "LD2"),
  x    = rep(overall_centroid["alcohol"], 2),
  y    = rep(overall_centroid["res_sugar"], 2),
  xend = c(
    overall_centroid["alcohol"] + scale_LD1 * LD1_direction["alcohol"],
    overall_centroid["alcohol"] + scale_LD2 * LD2_direction["alcohol"]
  ),
  yend = c(
    overall_centroid["res_sugar"] + scale_LD1 * LD1_direction["res_sugar"],
    overall_centroid["res_sugar"] + scale_LD2 * LD2_direction["res_sugar"]
  ),
  color = c("LD1" = "blue", "LD2" = "purple")
)

p2 <- ggplot() +
  # Data points
  geom_point(
    data = wqual_z,
    aes(x = alcohol, y = res_sugar, color = q_level),
    alpha = 0.4,
    size = 2
  ) +
  # Decision boundaries (lighter)
  geom_segment(
    data = bb_segs_tbl %>% 
      filter(!is.na(x), !is.na(y), !is.na(xend), !is.na(yend)),
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "gray50",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  # LD1 axis (direction of maximum discrimination)
  geom_segment(
    data = ld_arrows %>% filter(axis == "LD1"),
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
    color = "blue",
    linewidth = 1.5
  ) +
  # LD2 axis (second discriminant direction)
  geom_segment(
    data = ld_arrows %>% filter(axis == "LD2"),
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
    color = "purple",
    linewidth = 1.5
  ) +
  # Overall centroid
  geom_point(
    aes(x = overall_centroid["alcohol"], y = overall_centroid["res_sugar"]),
    color = "black",
    size = 4,
    shape = 18
  ) +
  # Labels for LD axes
  annotate(
    "text",
    x = ld_arrows$xend[1] + 0.2,
    y = ld_arrows$yend[1] + 0.2,
    label = "LD1\n(max discrimination)",
    color = "blue",
    fontface = "bold",
    size = 4
  ) +
  annotate(
    "text",
    x = ld_arrows$xend[2] + 0.2,
    y = ld_arrows$yend[2] + 0.2,
    label = "LD2\n(2nd direction)",
    color = "purple",
    fontface = "bold",
    size = 4
  ) +
  scale_color_manual(
    values = c("low" = "#D55E00", "medium" = "#E69F00", "high" = "#009E73")
  ) +
  labs(
    title = "Visualization 2: LD Axes in Original Space",
    subtitle = "LD1 and LD2 show directions of maximum group separation",
    x = "Alcohol (%)",
    y = "Residual Sugar (g/L)",
    color = "Wine Quality"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

print(p2)


## 
#  Visualization 3: Projection onto LD Space
## 

p3 <- ggplot() +
  # Projected data points
  geom_point(
    data = proj_df,
    aes(x = LD1, y = LD2, color = q_level),
    alpha = 0.6,
    size = 2
  ) +
  # Group centroids in LD space
  geom_point(
    data = proj_centroids,
    aes(x = LD1_mean, y = LD2_mean, fill = q_level),
    color = "black",
    size = 5,
    shape = 23,
    stroke = 1.5
  ) +
  # Labels for centroids
  geom_text(
    data = proj_centroids,
    aes(x = LD1_mean, y = LD2_mean, label = q_level),
    nudge_y = 0.3,
    fontface = "bold",
    size = 4
  ) +
  # Reference lines at origin
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray40") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
  scale_color_manual(
    values = c("low" = "#D55E00", "medium" = "#E69F00", "high" = "#009E73")
  ) +
  scale_fill_manual(
    values = c("low" = "#D55E00", "medium" = "#E69F00", "high" = "#009E73")
  ) +
  labs(
    title = "Visualization 3: Data Projected onto LD Space",
    subtitle = "New coordinate system maximizes group separation",
    x = "LD1 (First Discriminant Axis)",
    y = "LD2 (Second Discriminant Axis)",
    color = "Wine Quality",
    fill = "Wine Quality"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

print(p3)


## 
#  Combined Figure: All Three Visualizations
## 

library(patchwork)

combined_plot <- p1 / p2 / p3 +
  plot_annotation(
    title = "Three Perspectives on Linear Discriminant Analysis",
    subtitle = "Understanding the relationship between decision boundaries and LD axes",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

print(combined_plot)

# Save the combined figure
ggsave(
  filename = "lda_three_views.pdf",
  plot = combined_plot,
  width = 10,
  height = 14,
  units = "in"
)


## 
#  Pedagogical Notes for Chapter 7
## 

cat("\n\n=== PEDAGOGICAL SUMMARY ===\n\n")

cat("KEY DISTINCTIONS FOR STUDENTS:\n\n")

cat("1. PAIRWISE DECISION BOUNDARIES (Visualization 1):\n")
cat("   - These are LINES where δ_j(x) = δ_k(x)\n")
cat("   - They partition the feature space into regions\n")
cat("   - Each observation is classified to the nearest region\n")
cat("   - Number of boundaries: choose(K, 2) = ", choose(3, 2), "\n\n")

cat("2. LD AXES (Visualization 2):\n")
cat("   - These are DIRECTIONS, not boundaries\n")
cat("   - LD1 points in the direction of maximum group separation\n")
cat("   - LD2 is orthogonal to LD1 and captures remaining separation\n")
cat("   - Number of axes: min(K-1, p) = min(", 3-1, ",", 2, ") = ", 
    min(2, 2), "\n\n")

cat("3. LD PROJECTION (Visualization 3):\n")
cat("   - Data transformed to new coordinate system (LD1, LD2)\n")
cat("   - This is DIMENSIONALITY REDUCTION (if min(K-1, p) < p)\n")
cat("   - Group centroids are maximally separated in this space\n")
cat("   - Classification can be done in this reduced space\n\n")

cat("MATHEMATICAL CONNECTION:\n")
cat("- Your pairwise boundaries use: Σ^(-1) μ_k (the slope coefficients)\n")
cat("- MASS::lda()$scaling gives: eigenvectors of W^(-1)B\n")
cat("- Both use the pooled covariance matrix Σ ≈ W\n")
cat("- The LD axes span the K-1 dimensional discriminant subspace\n")
cat("- The boundaries are hyperplanes in the original p-dimensional space\n\n")

cat("For your textbook, consider this progression:\n")
cat("1. Start with 2D example (alcohol vs residual_sugar)\n")
cat("2. Show decision boundaries first (intuitive)\n")
cat("3. Introduce LD axes as 'optimal projection directions'\n")
cat("4. Show that projecting onto LD axes gives maximum separation\n")
cat("5. Emphasize: LDA provides BOTH classification AND dimension reduction\n")

##
#  EOF
##
