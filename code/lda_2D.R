#####
###
#     lda_2D.R
#
#       Prepare LDA output for scatter-plots with 
#       class-boundary line segments.
###
#####

## 
#  l2D_select_xy_tbl()
#  
#    Given data frame df and 3 specified columns, 
#    return a tibble consisting of just those columns.
## 
l2D_select_xy_tbl <- function(
  df,     # <df>  a data frame containing (x_1, x_2, y_group)
  x_1,    # <id>  name of 1st predictor variable
  x_2,    # <id>  name of 2nd predictor variable
  y_group # <id>  name of grouping variable
) {
  xy_tbl <- df |> 
    tibble::as_tibble() |> 
    dplyr::select(
      !! enquo(x_1), 
      !! enquo(x_2), 
      !! enquo(y_group))
  
  return(xy_tbl)
}

## 
#  l2D_get_xy_stats()
#  
#    Given data frame df and 3 specified columns, 
#    return a list of statistics for the given columns.
## 
l2D_get_xy_stats <- function(
    df,     # <df>  a data frame containing (x_1, x_2, y_group)
    x_1,    # <id>  name of 1st predictor variable
    x_2,    # <id>  name of 2nd predictor variable
    y_group # <id>  name of grouping variable
) {
  # 3 selected columns from n-by-d df
  xy_tbl <- l2D_select_xy_tbl(
    df, 
    !! enquo(x_1), 
    !! enquo(x_2), 
    !! enquo(y_group))
  # names of 3 selected columns
  xy_names <- xy_tbl |> names()
  
  # K-by-3 tibble: (y_group, mean_1, mean_2)
  x_means <- xy_tbl |> 
    dplyr::summarise(
      .by = xy_names [[3]], 
      dplyr::across(
        .cols = tidyr::everything(), 
        .fns = ~ mean(.x, na.rm = TRUE)
      ))
  
  # K = number of categories (unique values of y_group)
  K <- nrow(x_means)
  assertthat::assert_that( K >= 2L )
  
  # 2-by-2 named matrix
  x_cov <- xy_tbl |> 
    dplyr::select(- xy_names [[3]] ) |> 
    cov(use = "complete.obs")
  
  # (count, proportion) per group
  grp_stats <- xy_tbl |> 
    dplyr::summarise(
      .by  = xy_names [[3]], 
      ct   = n()
    ) |> 
    dplyr::mutate(
      prop = ct / sum(ct, na.rm = TRUE))
  
  ## 
  #   linear discriminant coefficients per group
  ## 
  b_mat <- x_means |> 
    dplyr::select(- 1) |> 
    # K-by-2
    as.matrix() |> 
    # 2-by-K
    t()
  
  slope_mat <- solve(a = x_cov, b = b_mat) |> 
    # K-by-2
    t()
  
  # inner products
  xb_vec <- vector(mode = "numeric")
  for (k in 1:K) {
    xb_vec [[k]] <- pracma::dot(slope_mat[k, ], b_mat [, k])
  }
  
  coeff_tbl <- grp_stats |> 
    dplyr::mutate(
      const = log(prop) - xb_vec/2) |> 
    dplyr::select(- c(ct, prop)) |> 
    dplyr::bind_cols(
      slope_mat |> 
        tibble::as_tibble())
  
  ## 
  #   coefficient differences: grp_1 - grp_2
  ## 
  
  idx_tbl <- tibble::tibble(
    idx_1 = rep(1:K, each  = K), 
    idx_2 = rep(1:K, times = K)) |> 
    dplyr::filter( idx_1 < idx_2 )
  
  coeff_mat <- coeff_tbl |> 
    dplyr::select(-1) |> 
    as.matrix()
  g_diff_mat <- matrix(nrow = nrow(idx_tbl), ncol = 3L)
  rdx = 0L
  for (j in 1:(K - 1L)) {
    for (k in (j + 1L):K) {
      g_1_vec <- coeff_mat [j, ] |> 
        as.vector()
      
      g_2_vec <- coeff_mat [k, ] |> 
        as.vector()
      
      rdx <- rdx + 1L
      g_diff_mat[rdx, ] <- g_1_vec - g_2_vec
    }
  }
  
  # coeff_diff
  colnames(g_diff_mat) <- (colnames(coeff_tbl)) [-1]
  coeff_diff <- g_diff_mat |> 
    tibble::as_tibble() |> 
    dplyr::bind_cols(idx_tbl) |> 
    dplyr::mutate(
      g_1 = (coeff_tbl |> dplyr::pull(1)) [idx_1], 
      g_2 = (coeff_tbl |> dplyr::pull(1)) [idx_2]
    ) |> 
    dplyr::select(- c(idx_1, idx_2)) |> 
    dplyr::select(g_1, g_2, tidyr::everything())
  names(coeff_diff) [1:2] <- paste0(xy_names [[3]], "_", 1:2)
  
  return(list(
    x_means    = x_means, 
    x_cov      = x_cov, 
    grp_stats  = grp_stats, 
    coeff_tbl  = coeff_tbl, 
    coeff_diff = coeff_diff
  ))
}

## 
#  tt_intersect()
#  
#    Each of two lines is defined by a triplet, (c0, c1, c2), 
#    of homogeneous coefficients.  Return the point of 
#    intersection of the two lines.
#  
#    line: c0 + c1 x + c2 y = 0
## 
tt_intersect <- function(
    trip_1, # <dbl> 1st triplet
    trip_2  # <dbl> 2nd triplet
) {
  assertthat::assert_that(
    length(trip_1) == 3L, 
    length(trip_2) == 3L)
  
  c_mat <- matrix(
    data = c( 
      trip_1 [2:3], 
      trip_2 [2:3]), 
    nrow = 2, ncol = 2, byrow = TRUE)
  
  soln <- solve(
    a = c_mat, 
    b = -c( trip_1 [[1]], trip_2 [[1]] )
  ) |> 
    as.vector()
  
  names(soln) <- c("x", "y")
  return(soln)
}

## 
#  bt_list()
#  
#    Find the points of intersection of a bounding box and a line.
#  
#      bbox: (xmin, xmax, ymin, ymax)
#  
#      line: c0 + c1 x + c2 y = 0
#  
#    Return a list of 2 intersection points or else return NULL.
## 
bt_list <- function(
    bbox, # <lst> named list (xmin, xmax, ymin, ymax)
    trip, # <dbl> un-named triplet (c0, c1, c2)
    tol = 1e-10 # <dbl> lower bound for norm(c1, c2)
) {
  ## 
  # is trip valid?
  ## 
  assertthat::assert_that( length(trip) == 3L )
  
  nrm_2_3 <- sqrt(trip[2]^2 + trip[3]^2)
  assertthat::assert_that( nrm_2_3 > tol )
  
  ## 
  # is bbox valid?
  ## 
  assertthat::assert_that(
    is.list(bbox), 
    length(bbox) == 4L, 
    bbox$ xmin < bbox$ xmax, 
    bbox$ ymin < bbox$ ymax
  )
  
  # t_new = (const, cos(theta), sin(theta)), 0 <= theta <= pi
  t_new <- trip / nrm_2_3
  if ( t_new [[3]] < 0 ) t_new = - t_new
  c0 <- t_new [[1]]
  c1 <- t_new [[2]]
  c2 <- t_new [[3]]
  
  # initialize list of intersection points
  pt_lst <- list()
  i_pt   <- 0L
  
  ## 
  # Check each edge of the bounding box.
  # 
  #   Include corner intersections. 
  #   Return exactly 2 points or else NULL.
  ## 
  
  # Left edge: x = xmin
  if ( abs(c2) > tol ) {
    y_left <- -( c0 + c1 * bbox$ xmin ) / c2
    if (
      (y_left >= bbox$ ymin) && 
      (y_left <= bbox$ ymax)
    ) {
      i_pt <- i_pt + 1L
      pt_lst [[i_pt]] <- c( x = bbox$ xmin, y = y_left )
    }
  } else {
    # line: c0 + c1 * x = 0
    x_vline <- - (c0 / c1)
    if (
      (x_vline > bbox$ xmin) && 
      (x_vline < bbox$ xmax)
    ) {
      i_pt <- i_pt + 1L
      pt_lst [[i_pt]] <- c( x = x_vline, y = bbox$ ymin )
      i_pt <- i_pt + 1L
      pt_lst [[i_pt]] <- c( x = x_vline, y = bbox$ ymax )
      return(pt_lst)
    } else {
      return(NULL)
    }
  }
  
  # Right edge: x = xmax
  if ( abs(c2) > tol ) {
    y_right <- -( c0 + c1 * bbox$ xmax ) / c2
    if (
      (y_right >= bbox$ ymin) && 
      (y_right <= bbox$ ymax)
    ) {
      i_pt <- i_pt + 1L
      pt_lst [[i_pt]] <- c( x = bbox$ xmax, y = y_right )
    }
  } else {
    # line: c0 + c1 * x = 0
    # this case treated above
  }
  
  # Bottom edge: y = ymin
  if ( abs(c1) > tol ) {
    x_bottom <- -( c0 + c2 * bbox$ ymin ) / c1
    if (
      (x_bottom >= bbox$ xmin) && 
      (x_bottom <+ bbox$ xmax)
    ) {
      i_pt <- i_pt + 1L
      pt_lst [[i_pt]] <- c( x = x_bottom, y = bbox$ ymin )
    }
  } else {
    # line: c0 + c2 * y = 0
    y_hline <- - (c0 / c2)
    if (
      (y_hline > bbox$ ymin) && 
      (y_hline < bbox$ ymax)
    ) {
      i_pt <- i_pt + 1L
      pt_lst [[i_pt]] <- c( x = bbox$ xmin, y = y_hline )
      i_pt <- i_pt + 1L
      pt_lst [[i_pt]] <- c( x = bbox$ xmax, y = y_hline )
      return(pt_lst)
    } else {
      return(NULL)
    }
  }
  
  # Top edge: y = ymax
  if ( abs(c1) > tol ) {
    x_top <- -( c0 + c2 * bbox$ ymax ) / c1
    if (
      (x_top >= bbox$ xmin) && 
      (x_top <= bbox$ xmax)
    ) {
      i_pt <- i_pt + 1L
      pt_lst [[i_pt]] <- c( x = x_top, y = bbox$ ymax )
    }
  } else {
    # line: c0 + c2 * y = 0
    # this case treated above
  }
  
  if ( length( pt_lst ) >= 2L ) {
    # check the logic above
    assertthat::assert_that(
      length( pt_lst ) == 2L )
    return( pt_lst )
  } else {
    return(NULL)
  }
}

## 
#  bt_seg()
#  
#    Find the points of intersection of a bounding box and a line.
#  
#      bbox: (xmin, xmax, ymin, ymax)
#  
#      line: c0 + c1 x + c2 y = 0
#  
#    Return a directed segment in the form of a named 
#    vector (x, y, xend, yend) or else return NULL.
## 
bt_seg <- function(
    bbox, # <lst> named list (xmin, xmax, ymin, ymax)
    trip, # <dbl> un-named triplet (c0, c1, c2)
    tol = 1e-10 # <dbl> lower bound for norm(c1, c2)
) {
  pt_lst <- bt_list(bbox, trip, tol)
  if (is.null( pt_lst) ) {
    return(NULL)
  } else {
    xy_tbl <- tibble::tibble(
      x = c(
        pt_lst [[1]] ["x"], 
        pt_lst [[2]] ["x"]), 
      y = c(
        pt_lst [[1]] ["y"], 
        pt_lst [[2]] ["y"])
    ) |> 
      # prepare segment with non-negative delta-y
      dplyr::arrange(y, x)
    seg_vec <- c(
      x    = xy_tbl [[1, "x"]], 
      y    = xy_tbl [[1, "y"]], 
      xend = xy_tbl [[2, "x"]], 
      yend = xy_tbl [[2, "y"]]
    )
  }
  return(seg_vec)
}


##
#  EOF
##
