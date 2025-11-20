# demo_priors_effect.R
# Demonstrate how prior probabilities affect LDA decision boundaries

library(MASS)
library(tidyverse)
source("lda_2D.R")

# Create synthetic data with unequal class sizes
set.seed(42)
n_class1 <- 100
n_class2 <- 300

data_synthetic <- rbind(
  data.frame(x = rnorm(n_class1, mean = 0, sd = 1),
             y = rnorm(n_class1, mean = 0, sd = 1),
             class = "A"),
  data.frame(x = rnorm(n_class2, mean = 2, sd = 1),
             y = rnorm(n_class2, mean = 2, sd = 1),
             class = "B")
)

# Fit LDA - will use observed priors
lda_observed <- lda(class ~ x + y, data = data_synthetic)
cat("Observed priors:\n")
print(lda_observed$prior)

# Fit LDA with equal priors (override observed)
lda_equal <- lda(class ~ x + y, data = data_synthetic, prior = c(0.5, 0.5))
cat("\nEqual priors (forced):\n")
print(lda_equal$prior)

# Get decision boundaries
xy_data <- data.frame(x = data_synthetic$x, y = data_synthetic$y, class = data_synthetic$class)

# Step 1: Get geometric boundary (midpoint between means)
line_geom <- l2D_get_xy_stats(lda_observed)
cat("\nGeometric boundary (through midpoint, no prior adjustment):\n")
print(line_geom)

# Step 2a: Adjust for observed priors
line_observed <- l2D_adjust_for_priors(line_geom, lda_observed)
cat("\nBoundary adjusted for observed priors:\n")
print(line_observed)

# Step 2b: Adjust for equal priors (for comparison)
line_equal <- l2D_adjust_for_priors(line_geom, lda_equal)
cat("\nBoundary adjusted for equal priors:\n")
print(line_equal)

# Step 3: Create segments
seg_geom <- l2D_make_segments(line_geom, xy_data)
seg_observed <- l2D_make_segments(line_observed, xy_data)
seg_equal <- l2D_make_segments(line_equal, xy_data)

# Plot comparison
p <- ggplot(xy_data, aes(x, y, color = class)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_segment(
    data = seg_geom,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "gray50",
    linewidth = 1,
    linetype = "dotted",
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = seg_observed,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "red",
    linewidth = 1.2,
    linetype = "solid",
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = seg_equal,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "blue",
    linewidth = 1.2,
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  annotate("text", x = -2.5, y = 4.5, 
           label = "Gray dotted: geometric (midpoint)",
           color = "gray50", hjust = 0, size = 3.5) +
  annotate("text", x = -2.5, y = 4, 
           label = sprintf("Red solid: observed priors (π_A=%.2f, π_B=%.2f)", 
                          line_observed$prior1, line_observed$prior2),
           color = "red", hjust = 0, size = 3.5) +
  annotate("text", x = -2.5, y = 3.5,
           label = "Blue dashed: equal priors (π_A=0.50, π_B=0.50)",
           color = "blue", hjust = 0, size = 3.5) +
  labs(
    title = "Effect of Prior Probabilities on LDA Decision Boundary",
    subtitle = sprintf("Class A: n=%d, Class B: n=%d", n_class1, n_class2),
    x = "X", y = "Y"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)
ggsave("prior_effect_demo.png", p, width = 10, height = 8, dpi = 150)

cat("\nNote: The boundary shifts toward the MORE COMMON class when using observed priors.\n")
cat("This reflects the Bayesian principle: we need stronger evidence to classify into the rarer class.\n")
cat("The geometric boundary (gray) passes through the midpoint between class means.\n")
