#####
###
#     f_divergence.R
#
#       Record some f-divergence kernel functions.
#
#       From:
#       Distances Between Probability Distributions of Different Dimensions
#       https://www.stat.uchicago.edu/~lekheng/work/probdist.pdf
###
#####

##
#  gen_fn_lst()
##
gen_fn_lst <- function() {
  fn_lst <- list()

  # Kullback-Liebler divergence
  fn_lst [[ "KL" ]] <-
    function(t) {
      value <- dplyr::if_else(
        dplyr::near(t, 0), 0, t * log(t))
      return(value)
    }

  # exponential divergence
  fn_lst [[ "exp" ]] <-
    function(t) {
      value <- dplyr::if_else(
        dplyr::near(t, 0), 0, t * log(t)^2)
      return(value)
    }

  # Pearson chi-squared divergence
  fn_lst [[ "chi_sq" ]] <-
    function(t){ (t - 1)^2 }

  fn_lst [[ "Hellinger" ]] <-
    function(t){ (sqrt(t) - 1)^2 }

  # Jeffreys divergence
  fn_lst [[ "Jeffreys" ]] <-
    function(t){ (t - 1) * log(t) }

  # total variation
  fn_lst [[ "total_var" ]] <-
    function(t){ abs(t - 1)/2 }

  return(fn_lst)
}

##
#  gen_fn_tbl()
#
#  return tibble that evaluates fn_lst on given t vector
##
gen_fn_tbl <- function(t) {
  fn_lst  <- gen_fn_lst()
  fn_name <- names(fn_lst)
  val_mat <- matrix(
    nrow = length(t),
    ncol = length(fn_name),
    dimnames = list(NULL, fn_name)
  )

  for (f_n in fn_name) {
    val_mat[, f_n] <- fn_lst [[f_n]] (t)
  }

  fn_tbl <- val_mat |> tibble::as_tibble() |>
    mutate(t = t) |>
    dplyr::select(t, dplyr::everything())

  return(fn_tbl)
}

# gen_fn_tbl( (0:7)/2 )
# # A tibble: 8 × 7
#     t     KL   exp chi_sq Hellinger Jeffreys total_var
# <dbl>  <dbl> <dbl>  <dbl>     <dbl>    <dbl>     <dbl>
# 1   0    0     0       1       1       Inf          0.5
# 2   0.5 -0.347 0.240   0.25    0.0858    0.347      0.25
# 3   1    0     0       0       0         0          0
# 4   1.5  0.608 0.247   0.25    0.0505    0.203      0.25
# 5   2    1.39  0.961   1       0.172     0.693      0.5
# 6   2.5  2.29  2.10    2.25    0.338     1.37       0.75
# 7   3    3.30  3.62    4       0.536     2.20       1
# 8   3.5  4.38  5.49    6.25    0.758     3.13       1.25


##
#  EOF
##
