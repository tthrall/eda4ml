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
    df,      # <df>  a data frame containing (x_1, x_2, y_group)
    x_1,     # <id>  name of 1st predictor variable
    x_2,     # <id>  name of 2nd predictor variable
    y_group, # <id>  name of grouping variable
    tol = 1e-10 # <dbl> lower bound for abs(det())
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
  
  # K-by-5 tibble: (y_group, min_1, max_1, min_2, max_2)
  x_minmax <- xy_tbl |> 
    dplyr::summarise(
      .by = xy_names [[3]], 
      dplyr::across(
        .cols = tidyr::everything(), 
        .fns = list(
          min = ~ min(.x, na.rm = TRUE), 
          max = ~ max(.x, na.rm = TRUE) )
      ))
  
  # 1-by-5 tibble: ("ALL", min_1, max_1, min_2, max_2)
  #   -- may be used as default value for bounding box
  x_minmax_all <- xy_tbl |> 
    dplyr::select(- 3) |> 
    dplyr::summarise(
      dplyr::across(
        .cols = tidyr::everything(), 
        .fns = list(
          min = ~ min(.x, na.rm = TRUE), 
          max = ~ max(.x, na.rm = TRUE) )
      )) |> 
    dplyr::mutate( grp_level = "ALL" ) |> 
    dplyr::select( grp_level, tidyr::everything())
  names(x_minmax_all) [1] = xy_names [[3]]
  
  # 2-by-2 named matrix: cov(x_1, x_2)
  x_cov <- xy_tbl |> 
    dplyr::select(- xy_names [[3]] ) |> 
    cov(use = "complete.obs")
  
  assertthat::assert_that(
    abs( det( cov2cor( x_cov ) ) ) > tol )
  
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
  
  # right side of x_cov %*% t(slope_mat) = b_mat
  b_mat <- x_means |> 
    dplyr::select(- 1) |> 
    # K-by-2
    as.matrix() |> 
    # 2-by-K
    t()
  
  # solution: x_cov %*% t(slope_mat) = b_mat
  slope_mat <- solve(a = x_cov, b = b_mat) |> 
    # K-by-2
    t()
  
  # const coefficient via inner products
  xb_vec <- vector(mode = "numeric")
  for (k in 1:K) {
    xb_vec [[k]] <- pracma::dot(slope_mat[k, ], b_mat [, k])
  }
  
  # per group: (const, c1, c2)
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
  
  # ig_tbl: ig_1, ig_2, g_1, g_2
  ig_tbl <- tibble::tibble(
    # group index
    ig_1 = rep(1:K, each  = K), 
    ig_2 = rep(1:K, times = K)) |> 
    dplyr::filter( ig_1 < ig_2 ) |> 
    # group name
    dplyr::mutate(
      g_1 = (coeff_tbl |> dplyr::pull(1)) [ig_1], 
      g_2 = (coeff_tbl |> dplyr::pull(1)) [ig_2]
    )
  names(ig_tbl) [3:4] <- paste0(xy_names [[3]], "_", 1:2)
  
  coeff_mat <- coeff_tbl |> 
    dplyr::select(-1) |> 
    as.matrix()
  
  grp_diff_mat <- matrix(nrow = nrow(ig_tbl), ncol = 3L)
  rdx = 0L
  for (j in 1:(K - 1L)) {
    for (k in (j + 1L):K) {
      g_1_vec <- coeff_mat [j, ] |> 
        as.vector()
      
      g_2_vec <- coeff_mat [k, ] |> 
        as.vector()
      
      rdx <- rdx + 1L
      grp_diff_mat[rdx, ] <- g_1_vec - g_2_vec
    }
  }
  colnames(grp_diff_mat) <- (colnames(coeff_tbl)) [-1]
  
  # coeff_diff: ig_1, ig_2, g_1, g_2, c0_diff, c1_diff, c2_diff
  coeff_diff <- ig_tbl |> 
    dplyr::bind_cols(
      grp_diff_mat |> 
        tibble::as_tibble() )
  
  return(list(
    x_means      = x_means, 
    x_minmax     = x_minmax, 
    x_minmax_all = x_minmax_all, 
    x_cov        = x_cov, 
    grp_stats    = grp_stats, 
    coeff_tbl    = coeff_tbl, 
    coeff_diff   = coeff_diff
  ))
}

## 
#  trip_valid()
#  
#    Validate a purported triplet of homogeneous coefficients, 
#    which is a vector of the following form.
#  
#      trip = c( c0, c1, c2 )
## 
trip_valid <- function(
    trip, # <dbl> vector c( c0, c1, c2 )
    tol = 1e-10 # <dbl> lower bound for norm(c1, c2)
) {
  trip_tst_1 <- assertthat::validate_that(
    is.numeric( trip ), 
    is.vector ( trip ), 
    length(trip) >= 3L)
  if ( is.character( trip_tst_1 ) ) {
    return(FALSE) }
  
  nrm_2_3 <- sqrt( ( trip [2] ^2 ) + ( trip [3] ^2 ) )
  trip_tst_2 <- assertthat::validate_that( nrm_2_3 > tol )
  
  if ( is.character( trip_tst_2 ) ) {
    return(FALSE) 
  } else {
    return(TRUE)
  }
}

## 
#  get_std_trip()
#  
#    Ensure that a given numeric triplet, (c0, c1, c2), 
#    of homogeneous coefficients defines a line in the 
#    (x, y) plane.
#  
#    line: c0 + c1 x + c2 y = 0
#  
#    Return an equivalent scaled version of the triplet, 
#    such that (c1, c2) is a 2-vector of unit norm, with 
#    c2 >= 0.
## 
get_std_trip <- function(
    trip, # <dbl> triplet c(c0, c1, c2)
    tol = 1e-10 # <dbl> lower bound for norm(c1, c2)
) {
  if (! trip |> trip_valid (tol = tol)) {
    return(NULL) }
  
  # t_std = (const, cos(theta), sin(theta)), 0 <= theta <= pi
  nrm_2_3 <- sqrt( ( trip [2] ^2 ) + ( trip [3] ^2 ) )
  t_std <- trip [1:3] / nrm_2_3
  if ( t_std [[3]] < 0 ) t_std = - t_std
  
  return(t_std)
}

## 
#  get_fn_trip_line()
#  
#    Given a triplet, (c0, c1, c2), of homogeneous 
#    coefficients, return the following function 
#    of (x, y):
#  
#    f(x, y) =  c0 + c1 x + c2 y
## 
get_fn_trip_line <- function(
    trip # <dbl> triplet c(c0, c1, c2)
) {
  t_std <- trip |> get_std_trip()
  c0 <- t_std [[1]]
  c1 <- t_std [[2]]
  c2 <- t_std [[3]]
  
  trip_line <- function(x, y) {
    c0 + (c1 * x) + (c2 * y) }
  
  return(trip_line)
}

## 
#  trip_meets_seg()
#  
#    Determine whether a line defined by a triplet, (c0, c1, c2), 
#    of homogeneous coefficients intersects a line segment defined 
#    by a quartet of end-point coordinates (x, y, xend, yend).
## 
trip_meets_seg <- function(
    trip, # <dbl> triplet c(c0, c1, c2)
    seg   # <dbl> quartet c(x, y, xend, yend)
) {
  
  # line: (x, y) such that trip_line(x, y) = 0
  trip_line <- trip |> get_fn_trip_line()
  
  # extract seg elements
  x    <- seg [[1]]
  y    <- seg [[2]]
  xend <- seg [[3]]
  yend <- seg [[4]]
  
  # Evaluate trip_line() at the 2 segment end-points.
  # If those 2 values have the same non-zero sign, 
  # (equivalently, if the product of the 2 values is 
  # positive) then the line does not meet the segment.
  t_meets_s <- 
    trip_line(x, y) * 
    trip_line(xend, yend) <= 0
  
  return(t_meets_s)
}

## 
#  seg_valid()
#  
#    Validate a purported directed segment of the following form.
#  
#      seg = c( x, y, xend, yend )
## 
seg_valid <- function(
    seg, # <dbl> vector c( x, y, xend, yend ) 
    tol = 1e-10 # <dbl> lower bound for norm(c1, c2)
) {
  seg_tst_1 <- assertthat::validate_that(
    is.numeric( seg ), 
    is.vector ( seg ), 
    length( seg ) >= 4L)
  if ( is.character( seg_tst_1 ) ) {
    return(FALSE) }
  
  # extract seg elements
  x    <- seg [[1]]
  y    <- seg [[2]]
  xend <- seg [[3]]
  yend <- seg [[4]]
  
  seg_nrm <- sqrt( (x - xend)^2 + (y - yend)^2 )
  seg_tst_2 <- assertthat::validate_that( seg_nrm > tol )
  if ( is.character( seg_tst_2 ) ) {
    return(FALSE) }
  
  return(TRUE)
}

## 
#  seg_to_trip()
#  
#    Given a segment defined by a quartet of end-point coordinates 
#    (x, y, xend, yend), return the line containing the segment as 
#    a triplet, (c0, c1, c2), of homogeneous coefficients.
## 
seg_to_trip <- function(
    seg, # <dbl> quartet c(x, y, xend, yend)
    tol = 1e-10 # <dbl> lower bound for norm(seg)
) {
  # extract seg elements
  x    <- seg [[1]]
  y    <- seg [[2]]
  xend <- seg [[3]]
  yend <- seg [[4]]
  
  seg_nrm <- sqrt((x - xend)^2 + (y - yend)^2)
  assertthat::assert_that( seg_nrm > tol )
  
  # segment: (u, v) satisfying
  #   (u, v) = (1 - t)*(x, y) + t*(xend, yend)
  # so that 
  #   t = (u - x)/(xend - x) = (v - y)/(yend - y)
  c0 = (yend - y)*x - (xend - x)*y
  c1 = -(yend - y)
  c2 =  (xend - x)
  
  t_std <- get_std_trip(c(c0 = c0, c1 = c1, c2 = c2))
  
  return(t_std)
}

## 
#  ss_point()
#  
#    Return intersection point of 2 given line segments.
#    Return NULL if segments do not intersect.
#    
#    Each segment is defined by a quartet of 
#    end-point coordinates (x, y, xend, yend).
## 
ss_point <- function(
    seg_1, # <dbl> quartet c(x, y, xend, yend)
    seg_2, # <dbl> quartet c(x, y, xend, yend)
    tol = 1e-10 # <dbl> upper bound of effective vector equality
) {
  # determine line containing each segment
  trip_1 <- seg_1 |> seg_to_trip(tol = tol)
  trip_2 <- seg_2 |> seg_to_trip(tol = tol)
  
  # determine segment-line intersections, if any
  pt_1 <- seg_1 |> st_point(trip_2)
  pt_2 <- seg_2 |> st_point(trip_1)
  
  if (
    is.null( pt_1 ) || 
    is.null( pt_2 ) ) {
    return(NULL)
  } else {
    pt_diff  <- pt_1 - pt_2
    nrm_diff <- sqrt( pt_diff [[1]]^2 + pt_diff [[2]]^2 )
    
    assertthat::assert_that(
      nrm_diff < 2 * tol )
    
    pt_avg <- ( pt_1 + pt_2 ) / 2
    names( pt_avg ) <- c("x", "y")
  }
  return( pt_avg )
}

## 
#  st_point()
#  
#    Determine the point (x, y) [if it exists] where a segment, 
#    defined by a quartet of end-point coordinates (x, y, xend, yend), 
#    meets a line defined by a triplet, (c0, c1, c2), of 
#    homogeneous coefficients.
## 
st_point <- function(
    seg, # <dbl> quartet c(x, y, xend, yend)
    trip # <dbl> triplet c(c0, c1, c2)
) {
  if (! trip_meets_seg( trip, seg ) ) {
    return(NULL)
  } else {
    # s_trip: triplet of line containing seg
    s_trip <- seg |> seg_to_trip()
    pt <- tt_point( trip, s_trip )
  }
  return(pt)
}

## 
#  tt_point()
#  
#    Each of two lines is defined by a triplet, (c0, c1, c2), 
#    of homogeneous coefficients.  Return the point of 
#    intersection of the two lines.
#  
#    line: c0 + c1 x + c2 y = 0
## 
tt_point <- function(
    trip_1, # <dbl> 1st triplet
    trip_2, # <dbl> 2nd triplet
    tol = 1e-10 # <dbl> lower bound for abs(det())
) {
  assertthat::assert_that(
    length(trip_1) == 3L, 
    length(trip_2) == 3L)
  
  c_mat <- matrix(
    data = c( 
      trip_1 [2:3], 
      trip_2 [2:3]), 
    nrow = 2, ncol = 2, byrow = TRUE)
  
  # is c_mat singular?
  c_det <- det(c_mat)
  c_tst <- ( abs( c_det ) > tol )
  assertthat::validate_that(
    abs( det( c_mat ) ) > tol )
  if (! c_tst ) {
    return(NULL)
  }
  
  soln <- solve(
    a = c_mat, 
    b = -c( trip_1 [[1]], trip_2 [[1]] )
  ) |> 
    as.vector()
  
  names(soln) <- c("x", "y")
  return(soln)
}

## 
#  bbox_valid()
#  
#    Validate a purported bounding box, 
#    which is a list of the following form.
#  
#      bbox = list (xmin, xmax, ymin, ymax)
## 
bbox_valid <- function(
    bbox # <lst> named list (xmin, xmax, ymin, ymax)
) {
  # is bbox valid?
  bb_tst <- assertthat::validate_that(
    is.list( bbox ), 
    length( bbox ) == 4L, 
    bbox$ xmin < bbox$ xmax, 
    bbox$ ymin < bbox$ ymax )
  if ( is.character( bb_tst ) ) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

## 
#  bbox_diag_to_seg()
#  
#    Given bounding box 
#  
#      bbox = list(xmin, xmax, ymin, ymax) 
#  
#    return segment 
#  
#      seg = c(x = xmin, y = ymin, xend = xmax, yend = ymax)
## 
bbox_diag_to_seg <- function(
    bbox = NULL # <lst> named list(xmin, xmax, ymin, ymax)
) {
  # bbox default value
  if ( is.null( bbox ) ) {
    bbox <- list(xmin = -1, xmax = 1, ymin = -1, ymax = 1) }
  
  if (! bbox |> bbox_valid() ) {
    return(NULL) }
  
  seg <- c(
    x    = bbox$ xmin, y    = bbox$ ymin, 
    xend = bbox$ xmax, yend = bbox$ ymax )
  
  return(seg)
}

## 
#  seg_to_bbox()
#  
#    Given a segment defined by a quartet of end-point coordinates 
#  
#      seg = c(x, y, xend, yend)
#    
#    return a bounding box, which is a list of the form 
#  
#      bbox = list(xmin, xmax, ymin, ymax)
#  
#    Map seg end-points to opposite bbox corners.
## 
seg_to_bbox <- function(
    seg  = NULL, # <dbl> quartet c(x, y, xend, yend)
    tol  = 1e-10 # <dbl> lower bound for (x_nrm, y_nrm)
) {
  # seg default value
  if ( is.null( seg ) ) {
    seg <- c(x = -1, y = -1, xend = 1, yend = 1) }
  
  if (! seg |> seg_valid(tol = tol) ) {
    return(NULL) }
  
  # extract seg elements
  x    <- seg [[1]]
  y    <- seg [[2]]
  xend <- seg [[3]]
  yend <- seg [[4]]
  
  sx_min <- min(x, xend)
  sx_max <- max(x, xend)
  x_nrm  <- sx_max - sx_min
  
  sy_min <- min(y, yend)
  sy_max <- max(y, yend)
  y_nrm  <- sy_max - sy_min
  
  # ensure seg not parallel to x-axis or y-axis
  seg_tst <- assertthat::validate_that(
    x_nrm > tol, 
    y_nrm > tol )
  if ( is.character( seg_tst ) ) {
    return(NULL) }
  
  bbox = list(
    xmin = sx_min, 
    xmax = sx_max, 
    
    ymin = sy_min, 
    ymax = sy_max )
  
  return(bbox)
}

## 
#  xy_in_bbox()
#  
#    Determine whether point(s) (x, y) belongs to 
#    the closed rectangle defined by a bounding box, 
#    which is a list of the following form.
#  
#      bbox = list (xmin, xmax, ymin, ymax)
## 
xy_in_bbox <- function(
  x,   # <dbl> 1st coordinate of point (x, y) 
  y,   # <dbl> 2nd coordinate of point (x, y)  
  bbox # <lst> named list (xmin, xmax, ymin, ymax)
) {
  # is bbox valid?
  if (! bbox_valid( bbox ) ) {
    return(NULL) }
  xy_tst <- assertthat::validate_that(
    length(x) > 0, 
    length(x) == length(y) )
  if ( is.character( xy_tst ) ) {
    return(NULL) }
  
  tf_x <- (x >= bbox$ xmin) & (x <= bbox$ xmax)
  tf_y <- (y >= bbox$ ymin) & (y <= bbox$ ymax)
  return( tf_x & tf_y )
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
    trip, # <dbl> triplet (c0, c1, c2)
    tol = 1e-10 # <dbl> lower bound for norm(c1, c2)
) {
  if (! bbox_valid( bbox )) {
    return(NULL) }
  
  # scale trip coefficients, if possible
  t_std <- trip |> get_std_trip(tol = tol)
  
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
  s_left_edge <- c(
    x    = bbox$ xmin, 
    y    = bbox$ ymin, 
    xend = bbox$ xmin, 
    yend = bbox$ ymax
  )
  tst_pt <- st_point( s_left_edge, t_std )
  if (! is.null( tst_pt ) ) {
    i_pt <- i_pt + 1L
    pt_lst [[i_pt]] <- tst_pt
  }
  
  # Right edge: x = xmax
  s_right_edge <- c(
    x    = bbox$ xmax, 
    y    = bbox$ ymin, 
    xend = bbox$ xmax, 
    yend = bbox$ ymax
  )
  tst_pt <- st_point( s_right_edge, t_std )
  if (! is.null( tst_pt ) ) {
    i_pt <- i_pt + 1L
    pt_lst [[i_pt]] <- tst_pt
  }
  
  # Bottom edge: y = ymin
  s_bottom_edge <- c(
    x    = bbox$ xmin, 
    y    = bbox$ ymin, 
    xend = bbox$ xmax, 
    yend = bbox$ ymin
  )
  tst_pt <- st_point( s_bottom_edge, t_std )
  if (! is.null( tst_pt ) ) {
    i_pt <- i_pt + 1L
    pt_lst [[i_pt]] <- tst_pt
  }
  
  # Top edge: y = ymax
  s_top_edge <- c(
    x    = bbox$ xmin, 
    y    = bbox$ ymax, 
    xend = bbox$ xmax, 
    yend = bbox$ ymax
  )
  tst_pt <- st_point( s_top_edge, t_std )
  if (! is.null( tst_pt ) ) {
    i_pt <- i_pt + 1L
    pt_lst [[i_pt]] <- tst_pt
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
#    Find the 2 points of intersection of a bounding box and a line.
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
#  get_bb_segs()
#  
#    Given: data frame df, 2 specified predictor columns, 
#    (x_1, x_2), 1 specified grouping column (y_group), 
#    and a bounding box (bbox) for a scatter-plot.
#    
#    Return: a list of statistical tables culminating in 
#    a table of bounding box segments, one directed segment 
#    for each pair of distinct groups.
## 
get_bb_segs <- function(
    df,         # <df>  a data frame containing (x_1, x_2, y_group)
    x_1,        # <id>  name of 1st predictor variable
    x_2,        # <id>  name of 2nd predictor variable
    y_group,    # <id>  name of grouping variable
    bbox = NULL # <lst> named list (xmin, xmax, ymin, ymax)
) {
  # list of stats tables from 3 columns of n-by-d df
  xy_stats_lst <- l2D_get_xy_stats(
    df, 
    !! enquo(x_1), 
    !! enquo(x_2), 
    !! enquo(y_group))
  
  # bbox default
  if ( is.null( bbox ) ) {
    bb_def_tbl  <- 
      xy_stats_lst$ 
      x_minmax_all |> 
      dplyr::select(-1)
    bbox <- list(
      xmin = bb_def_tbl [[1, 1]], 
      xmax = bb_def_tbl [[1, 2]], 
      ymin = bb_def_tbl [[1, 3]], 
      ymax = bb_def_tbl [[1, 4]]
    )
  }
  
  # line coefficients for each pair of distinct groups
  coeff_diff <- xy_stats_lst$ coeff_diff
  
  # K-by-3 numeric matrix of line coefficients c0, c1, c2
  i_c2 <- ncol(coeff_diff)
  i_c0 <- i_c2 - 2L
  trip_mat   <- coeff_diff  |> 
    dplyr::select( i_c0:i_c2 ) |> 
    as.matrix()
  
  # bbox_tbl: convert bbox list to 1-row tibble (for reference)
  bbox_tbl <- tibble::tibble(
    xmin = bbox$ xmin, 
    xmax = bbox$ xmax, 
    ymin = bbox$ ymin, 
    ymax = bbox$ ymax)
  
  # template for output from bt_seg()
  tst_bb <- list(
    xmin = -1, xmax = 1, 
    ymin = -1, ymax = 1)
  tst_trip <- c(c0 = 0, c1 = -2, c2 = 1)
  seg_vec  <- bt_seg( tst_bb, tst_trip )
  seg_tbl  <- seg_vec |> tibble::as_tibble_row()
  
  # initialize segs_mat as K-by-4 matrix filled with NA
  segs_mat <- matrix(
    nrow = nrow(coeff_diff), 
    ncol = ncol(seg_tbl) )
  
  # replace NA's in each segs_mat row with segment end-points
  for (i in 1:nrow(trip_mat)) {
    seg_tmp <- bt_seg( bbox, trip_mat [i, ] )
    if (! is.null( seg_tmp )) {
      segs_mat[i, ] <- seg_tmp }
  }
  colnames(segs_mat) <- names(seg_vec)
  
  # columns: ig_1, ig_2, g_1, g_2, c0, c1, c2, x, y, xend, yend
  bb_segs_tbl <- coeff_diff |> 
    dplyr::bind_cols(
      segs_mat |> tibble::as.tibble())
  
  bb_segs_lst <- 
    xy_stats_lst |> 
    append(list(
      bbox_tbl    = bbox_tbl, 
      bb_segs_tbl = bb_segs_tbl))
  
  return(bb_segs_lst)
}
# test 1:
# wq_z |> get_bb_segs(
#   density, res_sugar, color,
#   bbox = seg_to_bbox() )
# 
# test 2:
# wqual_z  |> get_bb_segs(
#   alcohol, res_sugar, q_level,
#   bbox = seg_to_bbox() )

## 
#  get_igroup_tbl()
#  
#    Generate a tibble of integer indices.
## 
get_igroup_tbl <- function(
    n_groups = 4L # <int> determines group indices 1:n_groups
) {
  if ( n_groups < 2L ) {
    return(NULL) }
  
  igroup_tbl <- tibble::tibble(
    ig_1 = rep( 1:(n_groups - 1L), each = (n_groups - 1L) ), 
    ig_2 = rep( 2:n_groups,        times = (n_groups - 1L) ) ) |> 
    dplyr::filter(ig_1 < ig_2)
  
  return(igroup_tbl)
}

## 
#  get_iseg_tbl()
#  
#    Given igroup_tbl: containing the indices (ig_1, ig_2) 
#    of pairs of distinct groups having a bbox line segment.
#    
#    Return iseg_tbl: (is_1, is_2, ig_1, ig_2, ig_3, ig_4), 
#    consisting of the indices of selected pairs of distinct 
#    segments and their corresponding group indices.
#    
#    Selection criteria: 
#      (1) index pair (ig_1, ig_2) has exactly one integer 
#        value in common with (ig_3, ig_4).
#      (2) each possible segment combination (is_1, is_2) 
#        occurs no more than once in the returned tibble.
## 
get_iseg_tbl <- function(
    igroup_tbl # <tbl> starts with columns (ig_1, ig_2, ...)
) {
  if ( nrow( igroup_tbl ) < 2L ) {
    return(NULL) }
  
  ig_tbl <- igroup_tbl |> 
    dplyr::select(1:2) |> 
    dplyr::mutate(is_1 = 1:nrow(igroup_tbl)) |> 
    dplyr::select(is_1, tidyr::everything())
  
  iseg_tbl <- tibble::tibble(
    is_1 = rep( 1:nrow(igroup_tbl), each = nrow(igroup_tbl) ), 
    is_2 = rep( 1:nrow(igroup_tbl), times = nrow(igroup_tbl) ) ) |> 
    dplyr::filter(is_1 < is_2) |> 
    dplyr::mutate(
      ig_1 = ( ig_tbl |> dplyr::pull(2) ) [is_1], 
      ig_2 = ( ig_tbl |> dplyr::pull(3) ) [is_1], 
      ig_3 = ( ig_tbl |> dplyr::pull(2) ) [is_2], 
      ig_4 = ( ig_tbl |> dplyr::pull(3) ) [is_2] ) |> 
    dplyr::rowwise() |> 
    dplyr::filter(
      ( ig_1 %in% c(ig_3, ig_4) ) || ( ig_2 %in% c(ig_3, ig_4) )
    )
  return(iseg_tbl)
}

## 
#  l2D_get_boundaries()
#  
#    Given: data frame df, 2 specified predictor columns, 
#    (x_1, x_2), 1 specified grouping column (y_group), 
#    and a bounding box (bbox) for a scatter-plot.
#    
#    Return: a list of statistical tables culminating in 
#    a table of bounding box segments, one directed segment 
#    for each pair of distinct groups.
## 
l2D_get_boundaries <- function(
    df,      # <df>  a data frame containing (x_1, x_2, y_group)
    x_1,     # <id>  name of 1st predictor variable
    x_2,     # <id>  name of 2nd predictor variable
    y_group, # <id>  name of grouping variable
    bbox     # <lst> named list (xmin, xmax, ymin, ymax)
) {
  # TODO
  return(NULL)
}


##
#  EOF
##
