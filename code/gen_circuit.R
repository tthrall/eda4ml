#####
###
#     gen_circuit.R
#
#       Construct a simple closed curve in the (x, y) plane
#       whose interior area is to be estimated via
#       Monte Carlo simulation.
###
#####

##
#  gen_wave_norm()
#
#       return a sine wave function f(t) > 0
#       to serve as the varying norm of points
#       (c(t), s(t)) along a curve
##
gen_wave_norm <- function(
    f_0 = 1/2,    # <dbl> positive constant
    f_1 = 1/4,    # <dbl> amplitude (f_1 < f_0)
    period = 1/2, # <dbl> period of the sine wave
    phase = 0     # <dbl> phase of the sine wave (mod 1)
) {
  f <- function(t) {
    f_0 +
      (f_1 * sin(
        2 * pi * (t - phase) / period
      ))
  }
  return(f)
}

##
#  gen_circuit_tbl()
#
#       return a tibble of points (c_j, s_j) along
#       a simple closed curve in the (x, y) plane,
#       namely the unit circle deformed by norm f(t)
##
gen_circuit_tbl <- function(
    n_pts = 200L, # <int> desired number of points
    f_0 = 1/2,    # <dbl> positive constant
    f_1 = 1/4,    # <dbl> amplitude (f_1 < f_0)
    period = 1/2, # <dbl> period of the sine wave
    phase = 0     # <dbl> phase of the sine wave (mod 1)
) {
  f <- gen_wave_norm(f_0, f_1, period, phase)
  c_tbl <- tibble::tibble(
    t = (0:(n_pts - 1L))/n_pts,
    f = f(t),
    c = f * cos(2 * pi * t),
    s = f * sin(2 * pi * t)
  )
  return(c_tbl)
}


##
#  EOF
##
