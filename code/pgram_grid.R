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
    # 0:n
    idx     = 0:length (hist_lst$ counts), 
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
  # basis vectors in Cartesian (x, y) coordinates
  u_base <- c( x = cos(u_theta), y = sin(u_theta) )
  v_base <- c( x = cos(v_theta), y = sin(v_theta) )
  
  # list the two basis vectors separately
  uv_base_lst <- list(
    u_base = u_base, 
    v_base = v_base)
  
  # single 4-vector: c(u_x, u_y, u_x, v_y)
  uv_xy_b_vec <- c(u_base, v_base)
  names(uv_xy_b_vec) <- paste0(
    c(u_name, u_name, v_name, v_name), 
    c("_x", "_y")
  )
  
  # matrix: row = (u, v), col = (x, y)
  uv_xy_b_mat <- uv_xy_b_vec |> 
    matrix(
      nrow = 2, ncol = 2, byrow = TRUE, 
      dimnames = list(
        uv = c(u_name, v_name), 
        xy = c("x", "y"))
    )
  
  basis_lst <- list(
    uv_base_lst = uv_base_lst, 
    uv_xy_b_vec = uv_xy_b_vec, 
    uv_xy_b_mat = uv_xy_b_mat)
  
  return(basis_lst)
}

##
#  ob_2d_segs()
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
ob_2d_segs <- function(
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
#  ob_2d_grob_prep()
#
#    Given: two linearly independent unit vectors 
#    (u, v) defined by their respective angles on the 
#    unit circle.
#
#    Return the following list of objects: 
#      - basis vectors (u_base, v_base)
#      - segment endpoints defining an oblique grid
#      - plotting bounds (x_min, y_min), (x_max, y_max)
##
ob_2d_grob_prep <- function(
    u_theta  = 0,    # <dbl> u_base = (cos, sin)(u_theta)
    v_theta  = pi/2, # <dbl> v_base = (cos, sin)(v_theta)
    n_pts    = 5L,   # <int> generates ((1:n_pts) - 1) / (n_pts - 1)
    u_name = "u",    # <chr> desired name for vector u
    v_name = "v"     # <chr> desired name for vector v
) {
  # basis vectors in Cartesian (x, y) coordinates
  u_base <- c( x = cos(u_theta), y = sin(u_theta) )
  v_base <- c( x = cos(v_theta), y = sin(v_theta) )
  
  # list the two basis vectors separately
  uv_base_lst <- list(
    u_base = u_base, 
    v_base = v_base)
  
  # tibble of segment endpoints (oblique grid-lines)
  seg_tbl <- ob_2d_segs(
    u_theta = u_theta, 
    v_theta = v_theta, 
    n_pts   = n_pts, 
    u_name  = u_name, 
    v_name  = v_name)
  
  # (x, y) plotting limits (bounding box)
  xy_lim_lst <- list(
    x_min_plt = min(c( seg_tbl$ x_0, seg_tbl$ x_1 )), 
    x_max_plt = max(c( seg_tbl$ x_0, seg_tbl$ x_1 )), 
    
    y_min_plt = min(c( seg_tbl$ y_0, seg_tbl$ y_1 )), 
    y_max_plt = max(c( seg_tbl$ y_0, seg_tbl$ y_1 ))
  )
  
  grob_prep_lst <- list(
    uv_base_lst = uv_base_lst, 
    seg_tbl     = seg_tbl, 
    xy_lim_lst  = xy_lim_lst)
  
  return(grob_prep_lst)
}

## 
#  sc_xyz()
#
#    Given spherical coordinates (theta, phi) 
#    return a tibble of (x, y, z) coordinates
#    
#      x = cos(theta) * sin(phi)
#      y = sin(theta) * sin(phi)
#      z = cos(phi)
## 
sc_xyz <- function(
    # (theta, phi) default values yield (1, 0, 0)
  theta = 0,   # <dbl> atan2(y, x)
  phi   = pi/2 # <dbl> acos(z)
) {
  assertthat::assert_that(
    is.vector(theta), 
    is.vector(phi), 
    length(phi) > 0, 
    length(phi) == length(theta) 
  )
  
  coord_tbl <- tibble::tibble(
    theta = theta, 
    phi   = phi, 
    x     = cos(theta) * sin(phi), 
    y     = sin(theta) * sin(phi), 
    z     = cos(phi))
  
  return(coord_tbl)
}

##
#  xyz_sc()
#
#    Given the (x, y, z) coordinates of a non-zero vector, v, 
#    return spherical coordinates (theta, phi) of u = normalized v 
#    that satisfy:
#    
#      u [[1]] = cos(theta) * sin(phi)
#      u [[2]] = sin(theta) * sin(phi)
#      u [[3]] = cos(phi)
##
xyz_sc <- function(
    x, # <dbl> v [[1]]
    y, # <dbl> v [[2]]
    z  # <dbl> v [[3]]
) {
  xyz_norm_2 <- x^2 + y^2 + z^2
  assertthat::assert_that(xyz_norm_2 > 0)
  
  # normalize (x, y, z)
  u <- c(x, y, z) / sqrt( xyz_norm_2 )
  
  phi <- acos(u [[3]])
  
  if (abs(u [[3]]) < 1){
    theta <- atan2(u [[2]], u [[1]])
  } else {
    # theta undefined
    theta <- NaN
  }
  
  return(c(
    theta = theta, 
    phi   = phi
  ))
}

## 
#  ob_3d_basis()
#    
#    Given (theta, phi) coordinates of unit vectors (u, v, w),
#    return a list indexed by (u, v, w) of their respective 
#    (x, y, z) coordinates.
#    
#    Call function sc_xyz() for the following mapping:
#    
#      x = cos(theta) * sin(phi)
#      y = sin(theta) * sin(phi)
#      z = cos(phi)
##
ob_3d_basis <- function(
    # (theta, phi) default values yield Euclidean basis
  u_theta  = 0,    # <dbl> u_base: (u_theta, u_phi)
  u_phi    = pi/2, # <dbl> u_base: (u_theta, u_phi)
  v_theta  = pi/2, # <dbl> v_base: (v_theta, v_phi)
  v_phi    = pi/2, # <dbl> v_base: (v_theta, u_phi)
  w_theta  = pi/2, # <dbl> w_base: (w_theta, w_phi)
  w_phi    = 0,    # <dbl> w_base: (w_theta, w_phi)
  u_name = "u",    # <chr> desired name for vector u
  v_name = "v",    # <chr> desired name for vector v
  w_name = "w"     # <chr> desired name for vector w
) {
  # calculate (x, y, z) coordinates
  u_coord_tbl <- sc_xyz( u_theta, u_phi )
  v_coord_tbl <- sc_xyz( v_theta, v_phi )
  w_coord_tbl <- sc_xyz( w_theta, w_phi )
  
  # basis vectors: (x, y, z) coordinates
  u_base <- u_coord_tbl [1, ] |> 
    dplyr::select(x, y, z) |> 
    as.vector() |> 
    purrr::list_simplify()
  v_base <- v_coord_tbl [1, ] |> 
    dplyr::select(x, y, z) |> 
    as.vector() |> 
    purrr::list_simplify()
  w_base <- w_coord_tbl [1, ] |> 
    dplyr::select(x, y, z) |> 
    as.vector() |> 
    purrr::list_simplify()
  
  # list the three basis vectors separately
  uvw_base_lst <- list(
    u_base = u_base, 
    v_base = v_base, 
    w_base = w_base)
  
  return(uvw_base_lst)
}

##
#  ob_3d_segs()
#
#    Construct an oblique grid composed of three sets 
#    of line segments parallel to three respective 
#    linearly independent unit vectors (u, v, w) 
#    defined by their respective spherical coordinates.
#
#    Return a tibble that represents each segment by 
#    its initial and terminal points, from (x_0, y_0, z_0)
#    to (x_1, y_1, z_1), in Cartesian coordinates.
##
ob_3d_segs <- function(
    # (theta, phi) default values yield Euclidean basis
  u_theta  = 0,    # <dbl> u_base: (u_theta, u_phi)
  u_phi    = pi/2, # <dbl> u_base: (u_theta, u_phi)
  v_theta  = pi/2, # <dbl> v_base: (v_theta, v_phi)
  v_phi    = pi/2, # <dbl> v_base: (v_theta, u_phi)
  w_theta  = pi/2, # <dbl> w_base: (w_theta, w_phi)
  w_phi    = 0,    # <dbl> w_base: (w_theta, w_phi)
  n_pts    = 5L,   # <int> generates ((1:n_pts) - 1) / (n_pts - 1)
  u_name = "u",    # <chr> desired name for vector u
  v_name = "v",    # <chr> desired name for vector v
  w_name = "w"     # <chr> desired name for vector w
) {
  ## 
  # basis vectors
  ## 
  uvw_base_lst <- ob_3d_basis(
    u_theta, u_phi, 
    v_theta, v_phi, 
    w_theta, w_phi, 
    u_name, 
    v_name, 
    w_name)
  u_base = uvw_base_lst$ u_base
  v_base = uvw_base_lst$ v_base
  w_base = uvw_base_lst$ w_base
  
  ## 
  # break-points
  ## 
  
  # mu_vec: fractional multiples of base vectors
  # default values: (0:4)/4 in closed interval [0, 1]
  mu_vec <- ((1:n_pts) - 1) / (n_pts - 1)
  
  # break-points on (u, v, w) axes:
  # represent each axis as an (n_pts * 3) matrix, 
  # with row corresponding to mu_vec[], 
  # and column corresponding to (x, y, z) coordinate
  
  u_axis <- mu_vec %o% u_base
  v_axis <- mu_vec %o% v_base
  w_axis <- mu_vec %o% w_base
  
  ## 
  # index patterns
  ## 
  offset_1 = rep(1:n_pts, each = 2 * n_pts)
  offset_2 = rep(1:n_pts, each = 2) |> rep(times = n_pts)
  pointer  = rep(c(1L, n_pts), times = n_pts^2)
  
  ## 
  # segment tibble
  ## 
  seg_tbl <- tibble::tibble(
    u_idx = c( offset_1, offset_1, pointer ), 
    ur_dx = c(rep.int(1L, 4 * n_pts^2), 
              rep.int(3L, 2 * n_pts^2)), 
    
    v_idx = c( offset_2, pointer, offset_1 ), 
    vr_dx = c(rep.int(2L, 2 * n_pts^2), 
              rep.int(3L, 2 * n_pts^2), 
              rep.int(1L, 2 * n_pts^2)),
    
    w_idx = c( pointer, offset_2, offset_2 ), 
    wr_dx = c(rep.int(3L, 2 * n_pts^2), 
              rep.int(2L, 4 * n_pts^2)), 
    
    # identify the basis vector serving as "pointer"
    p_nm  = rep(
      c(w_name, v_name, u_name), each = 2 * n_pts^2), 
    
    # oscillate from initial to terminal segment point
    p_bin = rep(0:1, times = 3 * n_pts^2), 
    
    # (x, y, z): Cartesian coordinates
    x     = u_axis[u_idx, "x"] + v_axis[v_idx, "x"] + w_axis[w_idx, "x"],
    y     = u_axis[u_idx, "y"] + v_axis[v_idx, "y"] + w_axis[w_idx, "y"],
    z     = u_axis[u_idx, "z"] + v_axis[v_idx, "z"] + w_axis[w_idx, "z"]
  ) |> 
    dplyr::select(
      u_idx, v_idx, w_idx, 
      ur_dx, vr_dx, wr_dx, 
      dplyr::everything())
  
  return(seg_tbl)
}

##
#  ob_3d_grob_prep()
#
#    Given: three linearly independent unit vectors 
#    (u, v, w) defined by their respective angles 
#    (theta, phi) on the unit sphere, and whose 
#    (x, y, z) coordinates are: 
#    
#      x = cos(theta) * sin(phi)
#      y = sin(theta) * sin(phi)
#      z = cos(phi)
# 
#    Return the following list of objects: 
#      - basis vectors (u_base, v_base, w_base)
#      - segment endpoints defining an oblique grid
#      - xyz (min, max) plotting bounds
##
ob_3d_grob_prep <- function(
    # (theta, phi) default values yield Euclidean basis
  u_theta  = 0,    # <dbl> u_base: (u_theta, u_phi)
  u_phi    = pi/2, # <dbl> u_base: (u_theta, u_phi)
  v_theta  = pi/2, # <dbl> v_base: (v_theta, v_phi)
  v_phi    = pi/2, # <dbl> v_base: (v_theta, u_phi)
  w_theta  = pi/2, # <dbl> w_base: (w_theta, w_phi)
  w_phi    = 0,    # <dbl> w_base: (w_theta, w_phi)
  n_pts    = 5L,   # <int> generates ((1:n_pts) - 1) / (n_pts - 1)
  u_name = "u",    # <chr> desired name for vector u
  v_name = "v",    # <chr> desired name for vector v
  w_name = "w"     # <chr> desired name for vector w
) {
  ## 
  #   basis vectors: (x, y, z) coordinates
  ## 
  uvw_base_lst <- ob_3d_basis(
    u_theta = u_theta, u_phi = u_phi, 
    v_theta = v_theta, v_phi = v_phi, 
    w_theta = w_theta, w_phi = w_phi, 
    u_name = u_name, 
    v_name = v_name, 
    w_name = w_name)
  
  ## 
  # tibble of segment endpoints (oblique grid-lines)
  ## 
  seg_tbl <- ob_3d_segs(
    u_theta = u_theta, u_phi = u_phi, 
    v_theta = v_theta, v_phi = v_phi, 
    w_theta = w_theta, w_phi = w_phi, 
    n_pts = n_pts, 
    u_name = u_name, 
    v_name = v_name, 
    w_name = w_name)
  
  ## 
  #   (x, y, z) plotting limits (bounding box)
  ## 
  xyz_minmax_tbl <- seg_tbl |> 
    dplyr::summarise(dplyr::across(
      .cols = c(x, y, z), 
      .fns  = c(min, max)
    )) |> 
    dplyr::rename(
      x_seg_min = x_1, 
      x_seg_max = x_2, 
      
      y_seg_min = y_1, 
      y_seg_max = y_2, 
      
      z_seg_min = z_1, 
      z_seg_max = z_2)
  xyz_minmax_vec <- xyz_minmax_tbl |> 
    # as.vector returns a 6-component list
    as.vector() |> 
    # convert list to named 6-component vector
    purrr::list_simplify()
  
  ## 
  #   return list of components
  ## 
  grob_prep_lst <- list(
    uvw_base_lst    = uvw_base_lst, 
    seg_tbl         = seg_tbl, 
    xyz_minmax_vec  = xyz_minmax_vec)
  return(grob_prep_lst)
}

##
#  cor_3d_check()
#
#    Given: off-diagonal elements of a 3D correlation matrix.
#
#    Determine whether the input values are consistent with a 
#    3D correlation matrix.
#
#    Return a list of test results.
##
cor_3d_check <- function(
    cor_12, # <dbl> cor(var_1, var_2)
    cor_13, # <dbl> cor(var_1, var_3)
    cor_23  # <dbl> cor(var_2, var_3)
) {
  # test: individual correlation coefficients in range
  max_abs_cor <- max(abs(cor_12), abs(cor_13), abs(cor_23))
  max_abs_test <- tibble::tibble(
    max_abs_cor = max_abs_cor, 
    ref_value   = 1, 
    is_OK       = (max_abs_cor < 1))
  
  # construct putative correlation matrix
  r_mat <- matrix(
    data = c(
      1, cor_12, cor_13, 
      cor_12, 1, cor_23, 
      cor_13, cor_23, 1), 
    nrow = 3, ncol = 3)
  
  # test: det(r_mat) > 0
  r_mat_det <- det(r_mat)
  mat_det_test <- tibble::tibble(
    r_mat_det = r_mat_det, 
    ref_value = 0, 
    is_OK     = (r_mat_det > 0))
  
  check_list <- list(
    max_abs_test = max_abs_test, 
    r_mat        = r_mat, 
    mat_det_test = mat_det_test)
  
  return(check_list)
}

##
#  cor_xyz()
#
#    Given: off-diagonal elements of a 3D correlation matrix.
#
#    Return: XYZ coordinates of vectors (u, v, w) on the unit sphere 
#    whose pairwise angles have cosines matching the correlations. 
#    
#    Calculated XYZ coordinates of (u, v, w) are constrained: 
#      
#      XYZ(u) = (1,   0,   0)
#      XYZ(v) = (v_x, v_y, 0)   with v_y >= 0
#      XYZ(w) = (w_x, w_y, w_z) with w_z >= 0
##
cor_xyz <- function(
    cor_12,         # <dbl> cor(var_1, var_2)
    cor_13,         # <dbl> cor(var_1, var_3)
    cor_23,         # <dbl> cor(var_2, var_3)
    r_check = FALSE # <lgl> check validity of cor matrix
) {
  if (r_check) {
    check_list <- cor_3d_check(cor_12, cor_13, cor_23)
    
    assertthat::assert_that(
      check_list$ max_abs_test$ is_OK == TRUE, 
      check_list$ mat_det_test$ is_OK == TRUE)
  }
  
  # calculate angles between vectors
  theta_12 <- acos( cor_12 )
  theta_13 <- acos( cor_13 )
  theta_23 <- acos( cor_23 )
  
  # calculate XYZ(u, v)
  u <- c(1, 0, 0)
  v <- c(cor_12, sin(theta_12), 0)
  
  # calculate XYZ(w)
  w_x <- cor_13
  #   - now cor_23 = v_x*w_x + v_y*w_y
  w_y <- (cor_23 - (cor_12 * cor_13)) / v[[2]]
  #   - check that norm(w_x, w_y, 0) < 1
  w_norm2_xy <- w_x^2 + w_y^2
  assertthat::assert_that( w_norm2_xy < 1 )
  w_z <- (1 - w_norm2_xy) |> sqrt()
  w   <- c(w_x, w_y, w_z)
  
  # name the (x, y, z) components of each vector
  xyz_names <- c("x", "y", "z")
  names(u)  <- xyz_names
  names(v)  <- xyz_names
  names(w)  <- xyz_names
  
  uvw_list <- list(
    u = u, 
    v = v, 
    w = w)
  
  return(uvw_list)
}

##
#  cor_xyz_sc()
#
#    Given off-diagonal elements of a 3D correlation matrix, 
#    determine three unit vectors (u, v, w) whose pairwise 
#    inner products are equal to the corresponding correlation 
#    coefficients.  Express (u, v, w) in both Cartesian 
#    (x, y, z) and spherical (theta, phi) coordinates.
#
#    The problem must be further constrained to yield a unique 
#    solution.  The constraints imposed can be expressed in XYZ 
#    coordinates as follows.
#      
#      XYZ(u) = (1,   0,   0)
#      XYZ(v) = (v_x, v_y, 0)   with v_y >= 0
#      XYZ(w) = (w_x, w_y, w_z) with w_z >= 0
##
cor_xyz_sc <- function(
    cor_12,         # <dbl> cor(var_1, var_2)
    cor_13,         # <dbl> cor(var_1, var_3)
    cor_23,         # <dbl> cor(var_2, var_3)
    r_check = FALSE # <lgl> check validity of cor matrix
) {
  if (r_check) {
    check_list <- cor_3d_check(cor_12, cor_13, cor_23)
    
    assertthat::assert_that(
      check_list$ max_abs_test$ is_OK == TRUE, 
      check_list$ mat_det_test$ is_OK == TRUE)
  }
  
  # determine XYZ coordinates of (u, v, w)
  uvw_list <- cor_xyz(cor_12, cor_13, cor_23, r_check)
  
  # extract vectors
  u <- uvw_list$ u
  v <- uvw_list$ v
  w <- uvw_list$ w
  
  # determine (theta, phi) coordinates of each vector
  u_sc_vec <- xyz_sc( u [["x"]], u [["y"]], u [["z"]] )
  v_sc_vec <- xyz_sc( v [["x"]], v [["y"]], v [["z"]] )
  w_sc_vec <- xyz_sc( w [["x"]], w [["y"]], w [["z"]] )
  
  ## 
  #   oops, xyz_sc returns c(theta, phi), rather than a tibble
  ## 
  
  # form a 3-row tibble from the 3 1-row tibbles
  # uvw_sc_tbl <- dplyr::bind_rows(
  #   u_sc_tbl |> dplyr::mutate(vec = "u"), 
  #   v_sc_tbl |> dplyr::mutate(vec = "v"), 
  #   w_sc_tbl |> dplyr::mutate(vec = "w")
  # ) |> dplyr::select(vec, dplyr::everything())
  
  # build tibble of (x, y, z, theta, phi) per vec
  uvw_6c_tbl <- tibble::tibble(
    vec   = c("u", "v", "w"), 
    x     = c(u [["x"]], v [["x"]], w [["x"]]), 
    y     = c(u [["y"]], v [["y"]], w [["y"]]), 
    z     = c(u [["z"]], v [["z"]], w [["z"]]), 
    theta = c(
      u_sc_vec [["theta"]], 
      v_sc_vec [["theta"]], 
      w_sc_vec [["theta"]]), 
    phi   = c(
      u_sc_vec [["phi"]], 
      v_sc_vec [["phi"]], 
      w_sc_vec [["phi"]])
  )
  
  return(uvw_6c_tbl)
}

##
#  r_mat_3d()
#
#    Return a 3D auto-correlation matrix with coefficient r.
##
r_mat_3d <- function(
    r # <dbl> auto-correlation coefficient 
) {
  r_mat <- matrix(data = c(
    1, r, r^2, 
    r, 1, r, 
    r^2, r, 1), 
    nrow = 3, ncol = 3)
  return(r_mat)
}

##
#  EOF
##
