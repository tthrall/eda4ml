#####
###
#     rng_tbl.R
#
#       List functions from package stats that simulate
#       random samples from well-known statistical distributions.
###
#####

##
#  gen_rng_tbl()
##
gen_rng_tbl <- function() {
  rng_tbl <- tibble::tribble(
    ~fn, ~value, ~distribution,
    "rbeta", "dbl", "Beta",
    "rcauchy", "dbl", "Cauchy",
    "rchisq", "dbl", "(non-central) Chi-Squared",
    "rexp", "dbl", "Exponential",
    "rf", "dbl", "F",
    "rgamma", "dbl", "Gamma",
    "rlnorm", "dbl", "Log Normal",
    "rlogis", "dbl", "Logistic",
    "rnorm", "dbl", "Normal",
    "rt", "dbl", "Student t",
    "runif", "dbl", "Uniform",
    "rweibull", "dbl", "Weibull",
    "rbinom", "int", "Binomial",
    "rgeom", "int", "Geometric",
    "rhyper", "int", "Hypergeometric",
    "rnbinom", "int", "Negative Binomial",
    "rpois", "int", "Poisson",
  )
  return(rng_tbl)
}


##
#  EOF
##
