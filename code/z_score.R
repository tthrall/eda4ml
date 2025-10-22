#####
###
#     z_score.R
#       standardized and robust scaling
#       with transformation to unit interval
###
#####

##
#  z_score()
#    center = mean(), scale = sd()
##
z_score <- function(
    x, # <dbl> data vector
    na.rm = TRUE # <lgl> remove missing values to calculate mean, sd
) {
  ctr   <- mean(x, na.rm = na.rm)
  scale <- sd(x, na.rm = na.rm)
  x_std <- (x - ctr) / scale
  return(x_std)
}

##
#  robust_score()
#    center = median(), scale = iqr()
##
robust_score <- function(
    x, # <dbl> data vector
    na.rm = TRUE # <lgl> remove missing values to calculate ctr, scale
) {
  ctr   <- median(x, na.rm = na.rm)
  q_1   <- quantile(x, prob = 0.25, na.rm = na.rm)
  q_3   <- quantile(x, prob = 0.75, na.rm = na.rm)
  scale <- q_3 - q_1
  x_std <- (x - ctr) / scale
  return(x_std)
}

##
#  p_robust()
#    inverse normal transformation of robust_score()
##
p_robust <- function(
    x, # <dbl> data vector
    na.rm = TRUE # <lgl> remove missing values to calculate ctr, scale
) {
  # 2 * qnorm(0.75) is approximately 1.35
  z_approx <- 1.35 * robust_score(x, na.rm = na.rm)
  p_x      <- pnorm(z_approx)
  return(p_x)
}

##
#  p_z()
#    inverse normal transformation of z_score()
##
p_z <- function(
    x, # <dbl> data vector
    na.rm = TRUE # <lgl> remove missing values to calculate ctr, scale
) {
  z_approx <- z_score(x, na.rm = na.rm)
  p_x      <- pnorm(z_approx)
  return(p_x)
}

##
#  test_z_score()
#    test z_score() against standard normal quantiles
##
test_z_score <- function(
    n_x = 100L # <int> desired length of generated vector
) {
  p_vec <- (2L * (1:n_x) - 1L) / (2L * n_x)
  q_vec <- qnorm(p_vec)
  
  # use q_vec as the data input for z_score
  x_std <- z_score(q_vec)
  
  tst_tbl <- tibble::tibble(
    q_vec = q_vec, 
    x_std = x_std, 
    resid = x_std - q_vec)
  
  bias <- (tst_tbl$ resid)   |> mean()
  mse  <- (tst_tbl$ resid^2) |> mean()
  rmse <- sqrt(mse)
  
  return(list(
    tst_tbl = tst_tbl, 
    bias    = bias, 
    rmse    = rmse
  ))
}

##
#  test_robust_score()
#    test robust_score() against standard normal quantiles
##
test_robust_score <- function(
    n_x = 100L # <int> desired length of generated vector
) {
  p_vec <- (2L * (1:n_x) - 1L) / (2L * n_x)
  q_vec <- qnorm(p_vec)
  
  # use q_vec as the data input for z_score
  x_std <- robust_score(q_vec)
  
  tst_tbl <- tibble::tibble(
    q_vec = q_vec, 
    x_std = x_std, 
    resid = x_std - q_vec)
  
  bias <- (tst_tbl$ resid)   |> mean()
  mse  <- (tst_tbl$ resid^2) |> mean()
  rmse <- sqrt(mse)
  
  return(list(
    tst_tbl = tst_tbl, 
    bias    = bias, 
    rmse    = rmse
  ))
}

##
#  test_p_robust()
#    test p_robust() against standard normal quantiles
##
test_p_robust <- function(
    n_x = 100L # <int> desired length of generated vector
) {
  p_vec <- (2L * (1:n_x) - 1L) / (2L * n_x)
  q_vec <- qnorm(p_vec)
  
  # use q_vec as the data input for p_robust
  p_x <- p_robust(q_vec)
  
  tst_tbl <- tibble::tibble(
    p_vec = p_vec, 
    p_x   = p_x, 
    resid = p_x - p_vec)
  
  bias <- (tst_tbl$ resid)   |> mean()
  mse  <- (tst_tbl$ resid^2) |> mean()
  rmse <- sqrt(mse)
  
  return(list(
    tst_tbl = tst_tbl, 
    bias    = bias, 
    rmse    = rmse
  ))
}

##
#  test_p_z()
#    test p_z() against standard normal quantiles
##
test_p_z <- function(
    n_x = 100L # <int> desired length of generated vector
) {
  p_vec <- (2L * (1:n_x) - 1L) / (2L * n_x)
  q_vec <- qnorm(p_vec)
  
  # use q_vec as the data input for p_robust
  p_x <- p_z(q_vec)
  
  tst_tbl <- tibble::tibble(
    p_vec = p_vec, 
    p_x   = p_x, 
    resid = p_x - p_vec)
  
  bias <- (tst_tbl$ resid)   |> mean()
  mse  <- (tst_tbl$ resid^2) |> mean()
  rmse <- sqrt(mse)
  
  return(list(
    tst_tbl = tst_tbl, 
    bias    = bias, 
    rmse    = rmse
  ))
}


##
#  EOF
##
