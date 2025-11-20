# LDA 2D Package - Updated Design Summary

## Overview

The updated `lda_2D.R` package now provides a clean, modular three-function workflow for visualizing LDA decision boundaries in 2D. This design separates geometric intuition from statistical adjustment, making it ideal for pedagogical purposes.

## Three-Function Workflow

### 1. `l2D_get_xy_stats(lda_fit, feature_names = NULL)`
**Purpose**: Calculate geometric decision boundaries

**What it does**: 
- For each pair of classes, computes line coefficients (c0, c1, c2) for: c1*x + c2*y + c0 = 0
- Boundary passes through the midpoint between class means
- Direction perpendicular to the line connecting means
- **Does NOT account for prior probabilities**

**Returns**: data.frame with c0, c1, c2, class1, class2

**When to use**: 
- To understand the pure geometric structure
- When assuming equal priors
- As the first step before statistical adjustment

---

### 2. `l2D_adjust_for_priors(line_coefs, lda_fit)`
**Purpose**: Adjust boundaries for unequal class frequencies

**What it does**:
- Takes geometric boundaries from `l2D_get_xy_stats()`
- Adds log(π₁/π₂) to the intercept c₀
- Shifts boundary toward the more common class
- Implements Bayes-optimal decision rule

**Returns**: data.frame with adjusted c0, plus prior1, prior2, c0_geometric

**When to use**:
- With real data having unequal class sizes
- To demonstrate Bayesian reasoning
- When optimality matters

**Skip when**:
- You want to assume equal priors
- Teaching pure geometric concepts first

---

### 3. `l2D_make_segments(line_coefs, xy_data, expand_factor = 0.1)`
**Purpose**: Convert line coefficients to drawable segments

**What it does**:
- Takes any line coefficients (from either function above)
- Finds intersections with plot bounding box
- Finds intersections with other decision boundaries
- Trims segments at first intersection (key innovation!)
- Creates shorter, cleaner visualizations

**Returns**: data.frame with x, y, xend, yend for `geom_segment()`

**Key feature**: Segments stop at intersections, mimicking the Python Stack Overflow solution

---

## Pedagogical Advantages

### Progressive Revelation
```r
# Start with geometry
line_geom <- l2D_get_xy_stats(lda_fit)
plot_boundaries(line_geom)  # Show to students

# Then add statistical reasoning
line_stat <- l2D_adjust_for_priors(line_geom, lda_fit)
plot_boundaries(line_stat)  # Show how it changes
```

### Side-by-Side Comparison
```r
# Perfect for textbook figures!
seg_geom <- l2D_make_segments(l2D_get_xy_stats(lda_fit), data)
seg_stat <- l2D_adjust_for_priors(l2D_get_xy_stats(lda_fit), lda_fit) |>
            l2D_make_segments(data)

ggplot(data, aes(x, y, color = class)) +
  geom_point() +
  geom_segment(data = seg_geom, color = "blue", linetype = "dashed") +
  geom_segment(data = seg_stat, color = "red") +
  labs(caption = "Blue = geometric, Red = with priors")
```

### Flexibility
- Use Step 1 only for equal prior assumption
- Use Steps 1+2 for realistic classification
- Use Steps 1+2+3 for publication-quality figures

---

## Mathematical Details

### Geometric Boundary (Step 1)
For classes i and j with means μᵢ and μⱼ:
- Direction: w = μᵢ - μⱼ
- Point: midpoint = (μᵢ + μⱼ)/2
- Line: w·(x - midpoint) = 0
- Homogeneous: c₁x + c₂y + c₀ = 0 where c₀ = -w·midpoint

### Statistical Adjustment (Step 2)
The Bayes-optimal boundary is where δᵢ(x) = δⱼ(x):
- δₖ(x) = x^T Σ⁻¹ μₖ - ½μₖ^T Σ⁻¹ μₖ + log(πₖ)
- This adds log(πᵢ/πⱼ) to the intercept
- Shifts toward more common class (requiring stronger evidence for rarer class)

### Segment Trimming (Step 3)
Algorithm:
1. Find where each line crosses the plot bounding box (2 points)
2. Find where it crosses all other decision boundaries (n-1 points for n boundaries)
3. Sort all points by distance along the line
4. Draw from bbox entry to first intersection

This creates segments that stop where boundaries meet, like the Python example: 
`np.linspace(x_min, intersection_point, 100)`

---

## Example Usage

### Basic Usage
```r
library(MASS)
library(tidyverse)
source("lda_2D.R")

# Your LDA model
lda_fit <- lda(Species ~ Sepal.Length + Sepal.Width, data = iris)
xy_data <- data.frame(x = iris$Sepal.Length, y = iris$Sepal.Width, 
                      class = iris$Species)

# Three-step workflow
line_coefs <- l2D_get_xy_stats(lda_fit)
line_coefs <- l2D_adjust_for_priors(line_coefs, lda_fit)
segments <- l2D_make_segments(line_coefs, xy_data)

# Plot
ggplot(xy_data, aes(x, y, color = class)) +
  geom_point() +
  geom_segment(data = segments, 
               aes(x=x, y=y, xend=xend, yend=yend),
               color = "black", inherit.aes = FALSE)
```

### For Textbook (Show Both)
```r
# Compute both versions
geom_line <- l2D_get_xy_stats(lda_fit)
stat_line <- l2D_adjust_for_priors(geom_line, lda_fit)

# Create segments
geom_seg <- l2D_make_segments(geom_line, xy_data)
stat_seg <- l2D_make_segments(stat_line, xy_data)

# Publication-quality comparison plot
ggplot(xy_data, aes(x, y, color = class)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_segment(data = geom_seg,
               aes(x=x, y=y, xend=xend, yend=yend),
               color = "blue", linewidth = 1, linetype = "dashed",
               inherit.aes = FALSE) +
  geom_segment(data = stat_seg,
               aes(x=x, y=y, xend=xend, yend=yend),
               color = "red", linewidth = 1.2,
               inherit.aes = FALSE) +
  labs(
    title = "LDA Decision Boundaries",
    subtitle = "Blue = geometric (equal priors), Red = statistical (observed priors)",
    caption = "Note how boundaries shift toward more common classes"
  )
```

---

## Files Included

1. **lda_2D.R** - Main package with three functions
2. **test_wine_lda.R** - Complete test with Wine Quality data
3. **demo_priors_effect.R** - Demonstrates effect of priors with synthetic data
4. **README_lda_2D.md** - User documentation
5. **DESIGN_SUMMARY.md** - This file

---

## Design Rationale

### Why Three Functions?

**Separation of Concerns**:
- Geometry ≠ Statistics ≠ Visualization
- Each does one thing well
- Easy to understand, test, and modify

**Pedagogical Value**:
- Students see concepts build progressively
- Can inspect intermediate results
- Understand *why* boundaries shift

**Flexibility**:
- Use subset of functions as needed
- Compare different approaches
- Extend easily (e.g., add covariance adjustment)

### Why Keep Geometric Separate?

In many textbooks (including yours!), it's valuable to:
1. First show the geometric intuition (midpoint, perpendicular)
2. Then introduce the statistical refinement (priors)
3. Finally discuss the full picture (covariance)

Combining Steps 1 and 2 would lose this pedagogical progression.

---

## Future Extensions

Possible additions to the package:

1. **Full covariance handling**: Compute w = Σ⁻¹(μᵢ - μⱼ) exactly
2. **Confidence regions**: Add uncertainty visualization
3. **Misclassification cost adjustment**: Extend beyond priors
4. **3D visualization**: For three features
5. **Animation**: Show boundary evolution as priors change

---

## Testing

Run the included test scripts:
```bash
Rscript test_wine_lda.R      # Real data example
Rscript demo_priors_effect.R # Prior comparison demo
```

Expected outputs:
- `wine_lda_boundaries.png` - 3-class wine quality
- `wine_color_boundary.png` - 2-class wine color
- `prior_effect_demo.png` - Shows geometric, equal prior, and observed prior boundaries

---

## Questions?

This design should work well for your Chapter 7 on Supervised Dimension Reduction. The modular structure lets you introduce concepts progressively, and the segment trimming creates clean, professional figures for your textbook.

Let me know if you need any adjustments or extensions!
