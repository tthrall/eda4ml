#####
###
#     pgram_grid.R
#
#       Tessellate (2D, 3D) with (parallelograms, parallelepipeds).
###
#####

##
#  get_hist_lst()
#
#    List break-points, mid-points, ... from hist(x).
##
get_hist_lst <- function(
    x # <dbl> numeric vector
) {
  assertthat::assert_that(
    is.numeric(x), 
    purrr::is_vector(x), 
    length(x) >= 2L
  )
  
  hist_lst <- hist(
    x = x, breaks = "FD", plot = FALSE, 
    include.lowest = TRUE, right = TRUE)
  # "FD": n = (k+1) determined by Freedman-Diaconis formula
  
  # returns list of the following 6 elements
  # $breaks:   <num> [1:(k + 1)]
  # $counts:   <int> [1:k]
  # $density:  <num> [1:k]
  # $mids:     <num> [1:k]
  # $xname:    <chr>
  # $equidist: <lgl>
  
  # - attr(*, "class) = <chr> "histogram"
  
  return(hist_lst)
}

##
#  get_hist_numbers()
#
#    Call hist(x) and return a tibble of numeric variables.
#
#    NB re numeric elements of hist() list:
#    number of breaks = 1 + number of counts, etc.
#    
#    In the returned tibble, first value of count etc set to zero.
##
get_hist_numbers <- function(
    x # <dbl> numeric vector
) {
  assertthat::assert_that(
    is.numeric(x), 
    purrr::is_vector(x), 
    length(x) >= 2L
  )
  
  hist_lst <- hist(
    x = x, breaks = "FD", plot = FALSE, 
    include.lowest = TRUE, right = TRUE)
  # "FD": n = (k+1) determined by Freedman-Diaconis formula
  # intervals = [b_0, b_1], (b_1, b_2], ..., (b_(n-1), b_n]
  
  # extract numeric info from the following 6 elements
  # $breaks:   <num> [1:(k + 1)]
  # $counts:   <int> [1:k]
  # $density:  <num> [1:k]
  # $mids:     <num> [1:k]
  # $xname:    <chr>
  # $equidist: <lgl>
  # - attr(*, "class) = <chr> "histogram"
  
  # extrapolate mid_0
  mid_tail <- hist_lst$ mids
  mid_0    <- 2 * mid_tail [[1]] - mid_tail [[2]]
  
  hist_num_tbl <- tibble::tibble(
    # b_0, b_1, ..., b_n
    breaks = hist_lst$ breaks, 
    # map c_k etc to b_k for k = 1:n
    counts  = c(0, hist_lst$ counts), 
    density = c(0, hist_lst$ density), 
    mids    = c(mid_0, mid_tail)
  )
  
  return(hist_num_tbl)
}

##
#  ob_grid_2d()
#
#    Grid-points along two oblique axes (u, v), where 
#    (u, v) are linearly independent unit vectors.
#
#    Return (wide, long, xy) tibble of Cartesian coordinates
#      wide: (u_x, u_y, v_x, v_y)
#      long: (uv, xy, pts)
##
ob_grid_2d <- function(
    u_theta  = 0,    # <dbl> u_base = (cos, sin)(u_theta)
    v_theta  = pi/2, # <dbl> v_base = (cos, sin)(v_theta)
    n_pts    = 5L,   # <int> generates ((1:n_pts) - 1) / (n_pts - 1)
    u_name = "u",    # <chr> desired name for vector u
    v_name = "v"     # <chr> desired name for vector v
) {
  # Cartesian coordinates of (u, v) basis vectors
  u_base <- c(cos(u_theta), sin(u_theta))
  v_base <- c(cos(v_theta), sin(v_theta))
  
  # multiples of (u_base, v_base)
  pts <- ((1:n_pts) - 1) / (n_pts - 1)
  
  # Cartesian coordinates of grid-points along (u, v)_base
  
  # wide: n_pts obs of 4 vars
  uv_wide <- tibble::tibble(
    u_x = pts * u_base [[1]], 
    u_y = pts * u_base [[2]], 
    
    v_x = pts * v_base [[1]], 
    v_y = pts * v_base [[2]]
  )
  names(uv_wide) <- paste0(
    c(u_name, u_name, v_name, v_name), 
    c("_x", "_y"))
  
  # long: (4 * n_pts) obs of 4 vars
  # keys (uv, xy, c_idx) identify a single coordinate value
  uv_long <- tibble::tibble(
    uv    = rep(c(u_name, v_name), each = 2 * n_pts), 
    xy    = rep(c("x", "y"), each = n_pts) |> rep(times = 2), 
    c_idx = rep(1:n_pts, times = 4), 
    coord = c(
      (pts %o% u_base) |> as.vector(),
      (pts %o% v_base) |> as.vector())
  )
  
  # xy: (2 * n_pts) obs of 4 vars
  # keys (uv, c_idx) identify an (x, y) coordinate pair
  uv_xy <- tibble::tibble(
    uv    = rep(c(u_name, v_name), each = n_pts), 
    c_idx = rep(1:n_pts, times = 2), 
    x     = c(uv_wide$ u_x, uv_wide$ v_x), 
    y     = c(uv_wide$ u_y, uv_wide$ v_y)
  )
  
  return(list(
    uv_wide = uv_wide, 
    uv_long = uv_long, 
    uv_xy   = uv_xy
  ))
}

##
#  ob_2d_basis()
#    matrix of (x, y) coordinates of
#    2 points on unit circle
##
ob_2d_basis <- function(
    u_theta  = 0,    # <dbl> u_base = (cos, sin)(u_theta)
    v_theta  = pi/2, # <dbl> v_base = (cos, sin)(v_theta)
    u_name = "u",    # <chr> desired name for vector u
    v_name = "v"     # <chr> desired name for vector v
) {
  # Cartesian coordinates of (u, v) basis vectors
  u_base <- c(cos(u_theta), sin(u_theta))
  v_base <- c(cos(v_theta), sin(v_theta))
  
  # vector c(u_x, u_y, u_x, v_y)
  uv_xy_vec <- c(u_base, v_base)
  names(uv_xy_vec) <- paste0(
    c(u_name, u_name, v_name, v_name), 
    c("_x", "_y")
  )
  
  # matrix: row = (u, v), col = (x, y)
  uv_xy_mat <- uv_xy_vec |> 
    matrix(
      nrow = 2, ncol = 2, byrow = TRUE, 
      dimnames = list(
        uv = c(u_name, v_name), 
        xy = c("x", "y"))
    )
  
  return(list(
    uv_xy_vec = uv_xy_vec, 
    uv_xy_mat = uv_xy_mat
  ))
}

##
#  ob_segs_2d()
#
#    Construct an oblique grid composed of two sets 
#    of line segments parallel to two respective 
#    linearly independent unit vectors (u, v) 
#    defined by their respective angles on the 
#    unit circle.
#
#    Return a tibble that represents each segment by 
#    its initial and terminal points, from (x_0, y_0)
#    to (x_1, y_1), in Cartesian coordinates.
##
ob_segs_2d <- function(
    u_theta  = 0,    # <dbl> u_base = (cos, sin)(u_theta)
    v_theta  = pi/2, # <dbl> v_base = (cos, sin)(v_theta)
    n_pts    = 5L,   # <int> generates ((1:n_pts) - 1) / (n_pts - 1)
    u_name = "u",    # <chr> desired name for vector u
    v_name = "v"     # <chr> desired name for vector v
) {
  # basis vectors in Cartesian (x, y) coordinates
  u_base <- c( x = cos(u_theta), y = sin(u_theta) )
  v_base <- c( x = cos(v_theta), y = sin(v_theta) )
  
  
  # multiples of (u_base, v_base)
  # note: mu_vec [[n_pts]] = 1
  # therefore basis vectors can be 
  # recovered from returned tibble
  mu_vec <- ((1:n_pts) - 1) / (n_pts - 1)
  
  # u break-points
  u_axis_x <- u_base [["x"]] * mu_vec
  u_axis_y <- u_base [["y"]] * mu_vec
  
  # v break-points
  v_axis_x <- v_base [["x"]] * mu_vec
  v_axis_y <- v_base [["y"]] * mu_vec
  
  # tibble of segment-defining end-points
  seg_tbl <- tibble::tibble(
    # (u, v) swap roles after the first n_pts rows
    anchor    = rep(c(u_name, v_name), each = n_pts), 
    direction = rep(c(v_name, u_name), each = n_pts), 
    # multiplier index
    mu_idx = rep(1:n_pts, times = 2), 
    # initial point (x_0, y_0)
    x_0 = c(u_axis_x, v_axis_x), 
    y_0 = c(u_axis_y, v_axis_y), 
    # terminal point (x_1, y_1)
    x_1 = c(
      u_axis_x + v_base [["x"]], 
      v_axis_x + u_base [["x"]]), 
    y_1 = c(
      u_axis_y + v_base [["y"]], 
      v_axis_y + u_base [["y"]])
  )
  return(seg_tbl)
}

##
#  EOF
##
