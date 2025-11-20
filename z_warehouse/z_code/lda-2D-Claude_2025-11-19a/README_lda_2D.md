# LDA 2D Visualization with Trimmed Segments

This implementation creates decision boundary segments for LDA that are trimmed at their intersections with other boundaries, similar to the approach shown in the Stack Overflow Python example you found.

## Files

- **lda_2D.R**: Main functions for computing and drawing LDA decision boundaries
- **test_wine_lda.R**: Test script demonstrating usage with Wine Quality data

## Key Functions

The package provides a modular, three-function workflow:

### 1. `l2D_get_xy_stats(lda_fit, feature_names = NULL)`
Extracts **geometric** decision boundary coefficients from an LDA model. For each pair of classes, returns homogeneous line coefficients (c0, c1, c2) where the boundary passes through the midpoint between class means, perpendicular to the line connecting them.

**Does NOT account for prior probabilities** - this is the pure geometric boundary.

### 2. `l2D_adjust_for_priors(line_coefs, lda_fit)`
Adjusts the c₀ intercept coefficients to account for unequal class prior probabilities by adding log(π₁/π₂). This shifts the boundary toward the more common class, implementing the Bayes-optimal decision rule.

**Optional** - skip this step if you want equal prior assumptions.

### 3. `l2D_make_segments(line_coefs, xy_data, expand_factor = 0.1)`
Converts line coefficients (from either function above) to drawable segments for `geom_segment()`. 

**Key innovation**: segments are trimmed at intersections with other boundaries, creating shorter, more relevant visualizations.

## How It Works

### Modular Design Philosophy

The three-function design separates concerns and provides pedagogical clarity:

1. **Geometry first** (`l2D_get_xy_stats`): Shows the pure geometric intuition - boundaries pass through midpoints
2. **Statistical adjustment** (`l2D_adjust_for_priors`): Demonstrates how Bayesian reasoning shifts boundaries based on class frequencies
3. **Visualization** (`l2D_make_segments`): Handles the purely graphical problem of clipping lines to display regions

This modularity allows you to:
- Show geometric boundaries alone (equal prior assumption)
- Compare geometric vs. statistical boundaries side-by-side
- Examine the effect of different prior choices
- Use the same visualization code for both approaches

### Decision Boundary Calculation

For each pair of classes i and j, the LDA decision boundary is where δᵢ(x) = δⱼ(x), where:

δₖ(x) = xᵀΣ⁻¹μₖ - (1/2)μₖᵀΣ⁻¹μₖ + log(πₖ)

This gives a linear boundary: wᵀx + c = 0, where:
- w = Σ⁻¹(μᵢ - μⱼ) 
- c = -(1/2)(μᵢᵀΣ⁻¹μᵢ - μⱼᵀΣ⁻¹μⱼ) + log(πᵢ/πⱼ)

**Implementation notes**: 
- `l2D_get_xy_stats()` computes the geometric boundary (c without the prior term)
- `l2D_adjust_for_priors()` adds the log(πᵢ/πⱼ) adjustment to c₀
- The direction vector w uses the mean difference as an approximation (works well when features are similarly scaled)

### Segment Trimming Algorithm

1. **Find bbox intersections**: Determine where each line enters/exits the plot area
2. **Find line-line intersections**: Calculate where this boundary crosses other boundaries
3. **Sort points along the line**: Order all intersection points by distance
4. **Trim at first intersection**: Create segment from bbox entry to first intersection

This mimics the Python approach where `np.linspace(x_min, 0.272, 100)` stops at the intersection point (0.272).

## Usage Example

```r
library(MASS)
library(tidyverse)
source("lda_2D.R")

# Fit LDA model
lda_fit <- lda(Species ~ Sepal.Length + Sepal.Width, data = iris)

# Prepare data
xy_data <- data.frame(
  x = iris$Sepal.Length, 
  y = iris$Sepal.Width,
  class = iris$Species
)

# Step 1: Get geometric decision boundaries
line_coefs <- l2D_get_xy_stats(lda_fit)

# Step 2 (optional): Adjust for prior probabilities
line_coefs_adjusted <- l2D_adjust_for_priors(line_coefs, lda_fit)

# Step 3: Create drawable segments
segments <- l2D_make_segments(line_coefs_adjusted, xy_data)

# Plot
ggplot(xy_data, aes(x, y, color = class)) +
  geom_point() +
  geom_segment(
    data = segments, 
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "black", 
    linewidth = 1,
    inherit.aes = FALSE
  )
```

### Workflow Options

**With prior adjustment (recommended for real data):**
```r
line_coefs <- l2D_get_xy_stats(lda_fit)
line_coefs <- l2D_adjust_for_priors(line_coefs, lda_fit)
segments <- l2D_make_segments(line_coefs, xy_data)
```

**Without prior adjustment (pure geometry, equal priors assumed):**
```r
line_coefs <- l2D_get_xy_stats(lda_fit)
segments <- l2D_make_segments(line_coefs, xy_data)
```

**Compare both on same plot (pedagogical):**
```r
line_geom <- l2D_get_xy_stats(lda_fit)
line_stat <- l2D_adjust_for_priors(line_geom, lda_fit)

seg_geom <- l2D_make_segments(line_geom, xy_data)
seg_stat <- l2D_make_segments(line_stat, xy_data)

ggplot(xy_data, aes(x, y, color = class)) +
  geom_point() +
  geom_segment(data = seg_geom, aes(x=x, y=y, xend=xend, yend=yend),
               color = "blue", linetype = "dashed", inherit.aes = FALSE) +
  geom_segment(data = seg_stat, aes(x=x, y=y, xend=xend, yend=yend),
               color = "red", inherit.aes = FALSE)
# Blue = geometric, Red = with priors
```

## Testing

Run the test script:
```r
source("test_wine_lda.R")
```

This will create two examples:
1. 3-class wine quality classification (multiple intersecting boundaries)
2. 2-class wine color classification (single boundary)

## Notes

- Works with 2-class (binary) and multi-class LDA
- For multi-class, creates one boundary for each pair of classes
- Segments are automatically trimmed at their intersections
- The `expand_factor` parameter controls how much to extend the plot area beyond the data range

## Alternative Approaches

If this proves too complex, you mentioned being prepared to cite existing solutions like the `klaR::partimat()` function, which provides comprehensive partition plots for multiple classifiers including LDA.
