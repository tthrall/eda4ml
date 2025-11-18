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
#  EOF
##
