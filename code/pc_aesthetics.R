#####
###
#     pc_aesthetics.R
#
#       Principal component aesthetics
###
#####

##
#  get_pc_colors()
#    Standard, named colors for 
#    principal component axes
##
get_pc_colors <- function() {
  pc_colors_tbl <- tibble::tibble(
    idx = 1:3, 
    # plot color
    color = c("darkorange", "purple", "darkgreen"), 
    # narrative reference
    ref   = c("orange",     "purple", "green")
  )
  return(pc_colors_tbl)
}

##
#  pc_rot_sum()
#    Repair rotation matrix returned by stats:prcomp().
#    Require sum(PC_1) > 0.
##
pc_rot_sum <- function(
    prcomp_obj # <lst> object returned by stats::prcomp()
  ) {
  # are there at least 2 principal components?
  assertthat::assert_that(
    length( prcomp_obj$ sdev ) >= 2L
  )
  
  # change signs of PC coefficients if needed, 
  # but maintain positive rotation determinant
  rot_mat <- prcomp_obj$ rotation
  if ( rot_mat [, "PC1"] |> sum() < 0 ) {
    # rotation coefficient matrix
    prcomp_obj$ rotation [, c("PC1", "PC2")] <- 
      - prcomp_obj$ rotation [, c("PC1", "PC2")]
    # scores
    prcomp_obj$ x [, c("PC1", "PC2")] <- 
      - prcomp_obj$ x [, c("PC1", "PC2")]
  }
  return(prcomp_obj)
}

##
#  pc_rot_diag()
#    Repair rotation matrix returned by stats:prcomp().
#    Require diag(rot) > 0.
##
pc_rot_diag <- function(
    prcomp_obj # <lst> object returned by stats::prcomp()
) {
  # are there at least 2 principal components?
  n_components <- length( prcomp_obj$ sdev )
  assertthat::assert_that(
    n_components >= 2L
  )
  
  # original rotation matrix prior to any changes
  old_rot_mat  <- prcomp_obj$ rotation
  old_rot_diag <- diag(old_rot_mat)
  
  # multiply each rot column by the sign of its diagonal element
  rot_det <- 1L
  for (k in 1:n_components) {
    if ( sign(old_rot_diag [[k]] ) < 0 ) {
      
      # rotation column
      prcomp_obj$ rotation [, k] <- 
        - prcomp_obj$ rotation [, k]
      
      # scores
      prcomp_obj$ x [, k] <- 
        - prcomp_obj$ x [, k]
      
      rot_det <- - rot_det
    }
  }
  
  # ensure rot_det > 0
  if ( rot_det <= 0L ) {
    
    # rotation column
    prcomp_obj$ rotation [, n_components] <- 
      - prcomp_obj$ rotation [, n_components]
    
    # scores
    prcomp_obj$ x [, n_components] <- 
      - prcomp_obj$ x [, n_components]
    
    rot_det <- - rot_det
  }
  
  return(prcomp_obj)
}

##
#  get_pc_segs()
#    Return segment endpoint coordinates
#    for each principal component.
#    
#    Use mu_sd if specified.
#    If not, use mu_max (specified or default).
##
get_pc_segs <- function(
    prcomp_obj,     # <lst> object returned by stats::prcomp()
    mu_max = NULL,  # <dbl> multiple of max score
    mu_sd  = NULL   # <dbl> multiple of sd score
) {
  # are there at least 2 principal components?
  n_components <- length( prcomp_obj$ sdev )
  assertthat::assert_that(
    n_components >= 2L)
  
  # adopt rotation matrix format
  rot_mat    <- prcomp_obj$ rotation
  max_mat    <- prcomp_obj$ rotation
  end_pt_mat <- prcomp_obj$ rotation
  
  # end_pt_mat: use mu_sd or else mu_max
  if (! is.null(mu_sd)) {
    assertthat::assert_that(
      mu_sd > 0)
    # sd score per component
    end_pt_mat <- rot_mat %*% (diag(rcomp_obj$ sdev) * mu_sd)
  } else {
    if (! is.null(mu_max)) {
      assertthat::assert_that(
        mu_max > 0)
    } else {
      # default value
      mu_max <- 0.9
    }
    # max score per component
    max_per_pc <- apply(prcomp_obj$ x, 2, max)
    end_pt_mat <- rot_mat %*% (diag(max_per_pc) * mu_max)
  }
  
  # restore names of (rows, columns)
  rownames(end_pt_mat) <- rownames(rot_mat)
  colnames(end_pt_mat) <- colnames(rot_mat)
  
  return(end_pt_mat)
}

##
#  get_pc_stats()
#    Return the standard deviation and variance 
#    of each principal component.
##
get_pc_stats <- function(
    prcomp_obj # <lst> object returned by stats::prcomp()
) {
  stats_tbl <- tibble::tibble(
    idx     = 1:ncol(prcomp_obj$ rotation), 
    id      = paste0("PC", idx), 
    # standard deviation
    sd      = prcomp_obj$ sdev, 
    # variance
    var     = sd^2, 
    var_pct = 100 * var / sum(var)
  )
  return(stats_tbl)
}

##
#  get_rot_tbl_lst()
#    Return the rotation matrix as a tibble
#    in both wide and long configurations.
##
get_rot_tbl_lst <- function(
    prcomp_obj # <lst> object returned by stats::prcomp()
) {
  rot_wide <- prcomp_obj$ rotation |> 
    tibble::as_tibble(rownames = "var")
  
  rot_long <- rot_wide |> 
    tidyr::pivot_longer(
      cols = - var, 
      names_to = "i_pc", 
      names_prefix = "PC", 
      values_to = "coeff"
    ) |> 
    # label each component as both integer and name
    dplyr::mutate(
      i_pc = as.integer(i_pc), 
      pc   = paste0("PC", i_pc)
    ) |> 
    dplyr::select(var, i_pc, pc, everything())
    
  return(list(
    rot_wide = rot_wide, 
    rot_long = rot_long
  ))
}


##
#  EOF
##
