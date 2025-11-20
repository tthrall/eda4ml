# LDA 2D Quick Reference Card

## Three-Function Workflow

```r
source("lda_2D.R")

# Step 1: Geometric boundaries (through midpoints)
line_coefs <- l2D_get_xy_stats(lda_fit)

# Step 2: Adjust for priors (optional)
line_coefs <- l2D_adjust_for_priors(line_coefs, lda_fit)

# Step 3: Create drawable segments
segments <- l2D_make_segments(line_coefs, xy_data)

# Plot
ggplot(xy_data, aes(x, y, color = class)) +
  geom_point() +
  geom_segment(data = segments, 
               aes(x=x, y=y, xend=xend, yend=yend),
               color = "black", inherit.aes = FALSE)
```

---

## Function Quick Reference

| Function | Input | Output | Purpose |
|----------|-------|--------|---------|
| `l2D_get_xy_stats(lda_fit)` | LDA model | Line coefficients (c0, c1, c2) | Geometric boundaries |
| `l2D_adjust_for_priors(coefs, lda_fit)` | Coefs + model | Adjusted coefficients | Add prior adjustment |
| `l2D_make_segments(coefs, xy_data)` | Coefs + data | Segment endpoints | Create drawable segments |

---

## Common Patterns

### Equal Priors (Geometric Only)
```r
line_coefs <- l2D_get_xy_stats(lda_fit)
segments <- l2D_make_segments(line_coefs, xy_data)
```

### With Priors (Recommended)
```r
line_coefs <- l2D_get_xy_stats(lda_fit) |>
  l2D_adjust_for_priors(lda_fit)
segments <- l2D_make_segments(line_coefs, xy_data)
```

### Compare Both (Pedagogical)
```r
geom_seg <- l2D_get_xy_stats(lda_fit) |>
  l2D_make_segments(xy_data)

stat_seg <- l2D_get_xy_stats(lda_fit) |>
  l2D_adjust_for_priors(lda_fit) |>
  l2D_make_segments(xy_data)

# Plot both with different colors/linetypes
```

---

## Parameters

### `l2D_get_xy_stats(lda_fit, feature_names = NULL)`
- `lda_fit`: MASS::lda() model object
- `feature_names`: Optional, for documentation

### `l2D_adjust_for_priors(line_coefs, lda_fit)`
- `line_coefs`: Output from l2D_get_xy_stats()
- `lda_fit`: Same LDA model (to extract priors)

### `l2D_make_segments(line_coefs, xy_data, expand_factor = 0.1)`
- `line_coefs`: From either l2D_get_xy_stats() or l2D_adjust_for_priors()
- `xy_data`: data.frame with x, y columns
- `expand_factor`: How much to expand plot beyond data (default 0.1 = 10%)

---

## Tips

✅ **Do**: Pipe the workflow
```r
segments <- l2D_get_xy_stats(lda_fit) |>
  l2D_adjust_for_priors(lda_fit) |>
  l2D_make_segments(xy_data)
```

✅ **Do**: Inspect intermediate results
```r
line_coefs <- l2D_get_xy_stats(lda_fit)
print(line_coefs)  # Check geometric boundaries

line_coefs <- l2D_adjust_for_priors(line_coefs, lda_fit)
print(line_coefs)  # See how priors shift boundaries
```

✅ **Do**: Use informative colors
```r
geom_segment(data = segments, 
             color = "black",      # Boundary
             linewidth = 1.2,      # Visible
             linetype = "dashed",  # Distinct from data
             inherit.aes = FALSE)  # Don't inherit color from points
```

❌ **Don't**: Forget `inherit.aes = FALSE`
```r
# This will try to map segment color to class variable!
geom_segment(data = segments, aes(x=x, y=y, xend=xend, yend=yend))
```

---

## Troubleshooting

**Problem**: Segments look wrong
- Check that xy_data has x, y columns (not other names)
- Verify features match those used in LDA fit

**Problem**: No segments appear
- Check that line_coefs has valid coefficients
- Try larger expand_factor (e.g., 0.2)
- Verify bbox intersections with debugging

**Problem**: Want to show class labels on boundaries
```r
# Add midpoint annotations
midpoints <- line_coefs |>
  group_by(class1, class2) |>
  summarize(
    x = mean(c(segments$x[1], segments$xend[1])),
    y = mean(c(segments$y[1], segments$yend[1]))
  )

ggplot(...) + 
  ... +
  geom_text(data = midpoints,
            aes(x, y, label = paste(class1, "vs", class2)),
            inherit.aes = FALSE)
```

---

## Example Datasets

### Wine Quality (from tests)
```r
wine <- read_delim("winequality-red.csv", delim = ";")
wine <- wine |> 
  mutate(quality_class = factor(case_when(
    quality <= 5 ~ "low",
    quality == 6 ~ "medium", 
    quality >= 7 ~ "high"
  )))
lda_fit <- lda(quality_class ~ alcohol + volatile.acidity, data = wine)
```

### Iris (classic)
```r
lda_fit <- lda(Species ~ Sepal.Length + Sepal.Width, data = iris)
xy_data <- data.frame(x = iris$Sepal.Length, 
                      y = iris$Sepal.Width,
                      class = iris$Species)
```

---

## Mathematical Notes

**Geometric boundary** (Step 1):
- c₁x + c₂y + c₀ = 0 where (c₁, c₂) = μᵢ - μⱼ
- c₀ chosen so line passes through midpoint
- Assumes equal priors (π₁ = π₂)

**Prior adjustment** (Step 2):
- c₀ → c₀ + log(π₁/π₂)
- Boundary shifts toward more common class
- Implements Bayes-optimal rule

**Segment trimming** (Step 3):
- Finds intersections with other boundaries
- Draws from bbox to first intersection
- Creates cleaner visualizations
