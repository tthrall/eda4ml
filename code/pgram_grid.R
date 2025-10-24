#####
###
#     pgram_grid.R
#
#       Tessellate (2D, 3D) with (parallelogram, parallelepiped).
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
  
  hist_lst <- hist(x, breaks = "FD", plot = FALSE)
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
#  EOF
##
