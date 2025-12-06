# eda4mldata Package Updates for Part 2

## Overview

This document outlines the datasets needed for Part 2 chapters (Linear Regression, PCA, LDA) that should be added to or organized within the `eda4mldata` package.

## Current Status

Based on review of `la-intro.qmd` and `reduce-dim.qmd`, the chapters use data from:

1. **`HistData::GaltonFamilies`** — Heights of parents and children
2. **`datasets::USArrests`** — Violent crime rates per US state
3. **`ISLR2::College`** — US college statistics (already used in clustering chapter)
4. **`Rtsne` dry beans example** — Shape measurements of dry bean varieties
5. **Wine Quality** — Already in `eda4mldata::wine_quality`
6. **OECD Better Life Index** — Survey data (API issues noted)
7. **MNIST** — Handwritten digit images (via keras/tensorflow)
8. **NCI60** — Cancer genomics (via `ISLR2` or similar)

## Recommended Package Additions

### 1. `galton_families`

Cleaned version of `HistData::GaltonFamilies` with:
- One row per family (oldest child)
- Columns: `family`, `father`, `mother`, `child`, `gender`
- Helper function: `get_galton_heights()` returning both full and sons-only subsets

```r
# Usage
galton_data <- eda4mldata::galton_families
galton_sons <- galton_data |> dplyr::filter(gender == "male")
```

### 2. `us_arrests`

Cleaned version of `datasets::USArrests` with:
- State names as a column (not rownames)
- Standardized (z-score) version available
- Geographic coordinates for mapping

```r
# Usage
arrests <- eda4mldata::us_arrests
arrests_z <- eda4mldata::us_arrests_scaled
```

### 3. `dry_beans`

The UCI Dry Beans dataset (7 varieties, 16 shape features):
- Source: UCI ML Repository
- Already commonly used for classification/clustering examples

```r
# Usage
beans <- eda4mldata::dry_beans
```

### 4. `better_life_index`

Cached version of OECD Better Life Index data:
- Avoids API dependency issues
- Vintage: 2020 or most recent stable version
- Variables: Life Satisfaction + contributing factors per country

```r
# Usage
bli <- eda4mldata::better_life_index
```

### 5. Helper Functions

Consider adding helper functions that return pre-structured data:

```r
# For regression chapter
get_galton_3d()          # Returns list with data, counts, etc.
get_bli_data()           # Returns BLI with proper structure

# For PCA chapter
get_arrests_for_pca()    # Returns scaled, centered matrix
get_wine_for_pca()       # Returns numeric-only wine data

# For LDA chapter
get_wine_for_lda()       # Returns wine data with color as factor
get_beans_for_lda()      # Returns beans with variety as factor
```

## Documentation Standards

Each dataset should have:

1. **Description**: What the data represent
2. **Source**: Original source with citation
3. **Variables**: Column descriptions
4. **Dimensions**: Number of rows and columns
5. **Usage example**: Minimal code showing typical use
6. **References**: Academic citations if applicable

## Implementation Priority

| Dataset | Priority | Notes |
|---------|----------|-------|
| `galton_families` | HIGH | Used in linear regression chapter |
| `us_arrests` | HIGH | Used in both PCA examples |
| `dry_beans` | MEDIUM | Good multiclass example for LDA |
| `better_life_index` | MEDIUM | Resolves API issues, good international example |
| MNIST helpers | LOW | Complex dependency (keras), may keep external |

## Next Steps

1. Create `data-raw/` scripts for each new dataset
2. Add `.rda` files to `data/`
3. Create `.R` documentation files in `R/`
4. Update package DESCRIPTION (if new dependencies needed)
5. Run `devtools::document()` and `devtools::check()`
6. Update chapter code to use `eda4mldata::` prefix consistently
