#####
###
#     interval_grid.R
#
#       Partition the real line into congruent,
#       finite segments of the form (a, b] or [a, b).
###
#####

##
#  mid_grid_0()
#
#    Return a function that calculates the
#    mid-point of the segment containing x.
#
#    Base segment is (0, upr] or [0, upr).
##
mid_grid_0 <- function(
    upr,          # <dbl> 0 to upr is base interval
    upr_in = TRUE # <lgl> include upr in base interval?
) {
  assertthat::assert_that(upr > 0)
  if (upr_in) {
    ivl_mid_pt <- function(x) {
      return((upr * ceiling(x/upr)) - (upr/2))
    }
  } else {
    ivl_mid_pt <- function(x) {
      return((upr * floor(x/upr)) + (upr/2))
    }
  }
  return(ivl_mid_pt)
}
# example: upr == 2L
# tibble::tibble(
#   x    = (0:9)/2,
#   u_in = mid_grid_0(2) (x),
#   u_out = mid_grid_0(2, FALSE) (x)
# )
# A tibble: 10 × 3
#      x    u_in u_out
#  <dbl>   <dbl> <dbl>
#  1   0      -1     1
#  2   0.5     1     1
#  3   1       1     1
#  4   1.5     1     1
#  5   2       1     3
#  6   2.5     3     3
#  7   3       3     3
#  8   3.5     3     3
#  9   4       3     5
# 10   4.5     5     5




##
#  EOF
##
