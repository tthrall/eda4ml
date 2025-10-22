#####
###
#     spectrum_helpers.R
#
#       Explore and streamline use of spectrum estimation functions.
###
#####

##
#  extract spectral density from astsa::arma.spec()
##
spec_arma_spec <- function(
    ar = 0,       # <dbl> AR coefficients of z^j, j > 0
    ma = 0,       # <dbl> MA coefficients of z^k, k > 0
    wn_sd = 1,    # <dbl> white noise standard deviation
    n_sim = 1024L # <int> length of simulated ARMA time series
) {
  spec_lst <- astsa::arma.spec(
    ar        = ar,
    ma        = ma,
    var.noise = wn_sd^2,
    n.freq    = n_sim/2L,
    plot      = FALSE)
  return(spec_lst$ spec)
}

##
#  fmt_spec_pgram()
#
#    construct two tibbles from stats::spec.pgram() output
#
#    1.  (freq, spec)
#    2.  stats arranged in a single row
#
#    TODO: treat multivariate time series
##
fmt_spec_pgram <- function(
    x,     # <ts> univariate or multivariate time series
    taper, # <dbl> proportion of data to taper
    spans  # <dbl> widths of modified Daniell kernel
) {
  spec_lst <- stats::spec.pgram(
    x       = x,
    taper   = taper,
    spans   = spans,
    demean  = TRUE,
    detrend = FALSE,
    plot    = FALSE
  )

  freq_spec_tbl <- tibble::tibble(
    freq = spec_lst$ freq,
    spec = spec_lst$ spec)

  spec_scalars <- tibble::tibble(
    f_len = spec_lst$ freq |> length(),
    f_min = spec_lst$ freq |> min(),
    f_max = spec_lst$ freq |> max(),

    k_m   = spec_lst$ kernel$ m,
    k_len = spec_lst$ kernel$ coef |> length(),
    k_min = spec_lst$ kernel$ coef |> min(),
    k_max = spec_lst$ kernel$ coef |> max(),

    df    = spec_lst$ df,

    bw    = spec_lst$ bandwidth,

    n_in  = spec_lst$ orig.n,
    n_out = spec_lst$ n.used,
    n_pad = n_out - n_in,

    taper = spec_lst$ taper)

  return(list(
    freq_spec_tbl = freq_spec_tbl,
    spec_scalars  = spec_scalars))
}


##
#  EOF
##
