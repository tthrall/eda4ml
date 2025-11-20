# lda_2D.R
# Prepare LDA output for scatter-plots with class-boundary line segments.
#
# Purpose: Convert LDA decision boundaries to drawable segments for ggplot2,
#          with segments trimmed at intersections with other boundaries.

library(tidyverse)

#' Calculate LDA decision boundaries in homogeneous coordinates
#' 
#' For each pair of classes in LDA, compute the geometric line coefficients 
#' (c0, c1, c2) such that the decision boundary is: c1*x + c2*y + c0 = 0
#' 
#' This function computes the GEOMETRIC boundary passing through the midpoint
#' between class means, perpendicular to the line connecting the means.
#' It does NOT account for unequal prior probabilities - use l2D_adjust_for_priors()
#' for that adjustment.
#' 
#' @param lda_fit LDA model from MASS::lda()
#' @param feature_names names of the two features used (for reference)
#' @return data.frame with columns: c0, c1, c2, class1, class2
l2D_get_xy_stats <- function(lda_fit, feature_names = NULL) {
  
  means <- lda_fit$means
  n_classes <- nrow(means)
  
  # For 2-class problem: single decision boundary
  if (n_classes == 2) {
    mean_diff <- means[1,] - means[2,]
    midpoint <- colMeans(means)
    
    c1 <- mean_diff[1]
    c2 <- mean_diff[2]
    c0 <- -(c1 * midpoint[1] + c2 * midpoint[2])
    
    return(data.frame(
      c0 = c0,
      c1 = c1,
      c2 = c2,
      class1 = rownames(means)[1],
      class2 = rownames(means)[2]
    ))
  }
  
  # For multi-class: boundary for each pair
  pairs <- combn(n_classes, 2)
  results <- vector("list", ncol(pairs))
  
  for (i in seq_len(ncol(pairs))) {
    idx1 <- pairs[1, i]
    idx2 <- pairs[2, i]
    
    mean1 <- means[idx1,]
    mean2 <- means[idx2,]
    midpoint <- (mean1 + mean2) / 2
    
    mean_diff <- mean1 - mean2
    c1 <- mean_diff[1]
    c2 <- mean_diff[2]
    c0 <- -(c1 * midpoint[1] + c2 * midpoint[2])
    
    results[[i]] <- data.frame(
      c0 = c0,
      c1 = c1,
      c2 = c2,
      class1 = rownames(means)[idx1],
      class2 = rownames(means)[idx2]
    )
  }
  
  bind_rows(results)
}

#' Adjust LDA boundary coefficients for prior probabilities
#' 
#' Takes basic geometric boundaries (passing through midpoints) and adjusts
#' them to account for unequal class priors by modifying the intercept term.
#' 
#' The statistical decision boundary is where delta_i(x) = delta_j(x), where:
#'   delta_k(x) = x^T Sigma^{-1} mu_k - (1/2) mu_k^T Sigma^{-1} mu_k + log(pi_k)
#' 
#' For the geometric boundary through the midpoint, this reduces to adding
#' log(pi_i/pi_j) to the intercept:
#'   c0_adjusted = c0_geometric + log(pi1/pi2)
#' 
#' When pi1 = pi2, log(pi1/pi2) = 0, so no adjustment occurs.
#' When pi1 > pi2, the boundary shifts toward class 1 (requiring stronger 
#' evidence to classify into class 1, favoring the more common class 2).
#' 
#' @param line_coefs data.frame from l2D_get_xy_stats() with c0, c1, c2, class1, class2
#' @param lda_fit LDA model object (to extract priors)
#' @return data.frame with adjusted c0, original c1, c2, and prior information
#' 
#' @examples
#' line_coefs <- l2D_get_xy_stats(lda_fit)
#' line_coefs_adjusted <- l2D_adjust_for_priors(line_coefs, lda_fit)
l2D_adjust_for_priors <- function(line_coefs, lda_fit) {
  
  priors <- lda_fit$prior
  class_names <- names(priors)
  
  line_coefs |>
    rowwise() |>
    mutate(
      prior1 = priors[class1],
      prior2 = priors[class2],
      c0_geometric = c0,
      c0 = c0 + log(prior1 / prior2)
    ) |>
    ungroup()
}

#' Find intersection point of two lines in homogeneous coordinates
#' 
#' Solves the system:
#'   c1_1*x + c2_1*y + c0_1 = 0
#'   c1_2*x + c2_2*y + c0_2 = 0
#' 
#' @return c(x, y) intersection point, or NULL if lines are parallel
line_intersection <- function(c0_1, c1_1, c2_1, c0_2, c1_2, c2_2) {
  det <- c1_1 * c2_2 - c1_2 * c2_1
  
  if (abs(det) < 1e-10) {
    return(NULL)  # Parallel lines
  }
  
  x <- (c2_1 * c0_2 - c2_2 * c0_1) / det
  y <- (c1_2 * c0_1 - c1_1 * c0_2) / det
  
  c(x, y)
}

#' Find where a line intersects a rectangular bounding box
#' 
#' @param c0,c1,c2 line coefficients: c1*x + c2*y + c0 = 0
#' @param bbox list with xmin, xmax, ymin, ymax
#' @return data.frame with x, y, xend, yend (the two bbox intersection points)
find_bbox_intersections <- function(c0, c1, c2, bbox) {
  
  intersections <- list()
  
  # Test all four edges of bounding box
  
  # Left edge: x = xmin
  if (abs(c2) > 1e-10) {
    y_left <- -(c0 + c1 * bbox$xmin) / c2
    if (y_left >= bbox$ymin && y_left <= bbox$ymax) {
      intersections <- append(intersections, list(c(bbox$xmin, y_left)))
    }
  }
  
  # Right edge: x = xmax
  if (abs(c2) > 1e-10) {
    y_right <- -(c0 + c1 * bbox$xmax) / c2
    if (y_right >= bbox$ymin && y_right <= bbox$ymax) {
      intersections <- append(intersections, list(c(bbox$xmax, y_right)))
    }
  }
  
  # Bottom edge: y = ymin
  if (abs(c1) > 1e-10) {
    x_bottom <- -(c0 + c2 * bbox$ymin) / c1
    if (x_bottom >= bbox$xmin && x_bottom <= bbox$xmax) {
      intersections <- append(intersections, list(c(x_bottom, bbox$ymin)))
    }
  }
  
  # Top edge: y = ymax
  if (abs(c1) > 1e-10) {
    x_top <- -(c0 + c2 * bbox$ymax) / c1
    if (x_top >= bbox$xmin && x_top <= bbox$xmax) {
      intersections <- append(intersections, list(c(x_top, bbox$ymax)))
    }
  }
  
  # A line crossing the box should have exactly 2 intersections
  if (length(intersections) >= 2) {
    p1 <- intersections[[1]]
    p2 <- intersections[[2]]
    return(data.frame(x = p1[1], y = p1[2], xend = p2[1], yend = p2[2]))
  }
  
  NULL
}

#' Create a single decision boundary segment, trimmed at intersections
#' 
#' Strategy: Start at bbox entry point, extend to the first intersection
#' with another decision boundary (or to bbox exit if no intersections).
#' This creates shorter, more relevant segments as in the Python example.
#' 
#' @param c0,c1,c2 line coefficients for this boundary
#' @param all_lines data.frame of all line coefficients
#' @param current_idx row index of current line in all_lines
#' @param bbox bounding box list
#' @return data.frame with x, y, xend, yend for geom_segment()
make_single_segment <- function(c0, c1, c2, all_lines, current_idx, bbox) {
  
  # Find where this line enters/exits the bounding box
  bbox_pts <- find_bbox_intersections(c0, c1, c2, bbox)
  if (is.null(bbox_pts)) return(NULL)
  
  # Collect all points that lie on this line
  points_on_line <- list(
    list(x = bbox_pts$x, y = bbox_pts$y, type = "bbox"),
    list(x = bbox_pts$xend, y = bbox_pts$yend, type = "bbox")
  )
  
  # Find intersections with all other decision boundaries
  for (i in seq_len(nrow(all_lines))) {
    if (i == current_idx) next
    
    pt <- line_intersection(
      c0, c1, c2,
      all_lines$c0[i], all_lines$c1[i], all_lines$c2[i]
    )
    
    # Keep only intersections within the bounding box
    if (!is.null(pt) && 
        pt[1] >= bbox$xmin && pt[1] <= bbox$xmax &&
        pt[2] >= bbox$ymin && pt[2] <= bbox$ymax) {
      points_on_line <- append(points_on_line, list(
        list(x = pt[1], y = pt[2], type = "intersection")
      ))
    }
  }
  
  # Check if we have any intersection points
  int_pts <- Filter(function(p) p$type == "intersection", points_on_line)
  
  if (length(int_pts) == 0) {
    # No intersections with other boundaries - use full bbox segment
    return(bbox_pts)
  }
  
  # Sort all points by distance along the line from first bbox point
  bbox_p1 <- points_on_line[[1]]
  
  for (i in seq_along(points_on_line)) {
    p <- points_on_line[[i]]
    p$dist <- sqrt((p$x - bbox_p1$x)^2 + (p$y - bbox_p1$y)^2)
    points_on_line[[i]] <- p
  }
  
  points_on_line <- points_on_line[order(sapply(points_on_line, `[[`, "dist"))]
  
  # Create segment from first bbox point to first intersection
  # This mimics: np.linspace(x_min, 0.272, 100) from Python example
  p1 <- points_on_line[[1]]  # Should be a bbox point
  
  # Find first intersection point
  first_int_idx <- which(sapply(points_on_line, `[[`, "type") == "intersection")[1]
  
  if (!is.na(first_int_idx)) {
    p2 <- points_on_line[[first_int_idx]]
    return(data.frame(x = p1$x, y = p1$y, xend = p2$x, yend = p2$y))
  }
  
  # Fallback: use full bbox segment
  return(bbox_pts)
}

#' Convert LDA decision boundaries to drawable segments
#' 
#' Main function: takes line coefficients and produces a data.frame
#' suitable for ggplot2::geom_segment(), with segments trimmed at
#' intersections.
#' 
#' @param line_coefs data.frame from l2D_get_xy_stats()
#' @param xy_data data.frame with x, y coordinates of data points
#' @param expand_factor how much to expand bbox beyond data range (default 0.1 = 10%)
#' @return data.frame with x, y, xend, yend, class1, class2 for geom_segment()
#' 
#' @examples
#' library(MASS)
#' iris_lda <- lda(Species ~ Sepal.Length + Sepal.Width, data = iris)
#' line_coefs <- l2D_get_xy_stats(iris_lda)
#' xy_data <- data.frame(x = iris$Sepal.Length, y = iris$Sepal.Width)
#' segments <- l2D_make_segments(line_coefs, xy_data)
#' 
#' ggplot(xy_data, aes(x, y, color = iris$Species)) +
#'   geom_point() +
#'   geom_segment(data = segments, 
#'                aes(x = x, y = y, xend = xend, yend = yend),
#'                color = "black", inherit.aes = FALSE)
l2D_make_segments <- function(line_coefs, xy_data, expand_factor = 0.1) {
  
  # Define bounding box from data range
  x_range <- range(xy_data$x, na.rm = TRUE)
  y_range <- range(xy_data$y, na.rm = TRUE)
  x_expand <- diff(x_range) * expand_factor
  y_expand <- diff(y_range) * expand_factor
  
  bbox <- list(
    xmin = x_range[1] - x_expand,
    xmax = x_range[2] + x_expand,
    ymin = y_range[1] - y_expand,
    ymax = y_range[2] + y_expand
  )
  
  # Create segment for each decision boundary
  segments <- line_coefs |>
    rowwise() |>
    mutate(
      segment = list(make_single_segment(
        c0, c1, c2, 
        line_coefs, 
        cur_group_id(),
        bbox
      ))
    ) |>
    ungroup() |>
    filter(!sapply(segment, is.null)) |>
    unnest(segment)
  
  segments
}
