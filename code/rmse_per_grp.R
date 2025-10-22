#####
###
#     rmse_per_grp.R
#
#       Assume input df consists of numeric variables, with the
#       exception of one grouping variable.
#
#       Partition the rows of df into groups.  Within each group
#       calculate the mean of each numeric variable, and then
#       calculate the deviation of the variable from its mean.
#       Finally for each observation (row) in a group, calculate the
#       Euclidean norm of the deviations across all numeric variables.
#
#       Value: a list of 2 data frames whose rows match those of df.
#
#         - grp_deviations has column names matching those of df.
#
#         - grp_norms has 2 columns: (grp, norm)
###
#####

##
#  pt_ctr_dist()
##
pt_ctr_dist <- function(
    df,      # <df> data frame
    grp_name # <chr> name of grouping variable
) {
  assertthat::validate_that(
    is.data.frame((df)),
    is.character(grp_name),
    grp_name %in% names(df)
  )

  grp_deviations <- df |>
    dplyr::mutate(
      .by = grp_name,
      dplyr::across(
        .cols = everything(),
        .fns  = ~ .x - mean(.x, na.rm = TRUE)
      )
    )

  # transpose deviations
  grp_dev_mat <- grp_deviations |>
    dplyr::select(- grp_name) |> t()
  colnames(grp_dev_mat) <- paste0("r_", 1:nrow(grp_deviations))

  grp_norms <- grp_dev_mat |>
    tibble::as_tibble() |>
    # calculate row-wise norm of grp_deviations
    summarise(across(
      .cols = everything(),
      .fns  = ~ norm(.x, "2")
    )) |>
    # transpose back to match rows in df
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "row_idx",
      names_prefix = "r_",
      values_to = "norm"
    ) |>
    # restore grouping variable
    dplyr::mutate(gp_tmp = grp_deviations |> pull(var = grp_name)) |>
    dplyr::select(gp_tmp, norm)
  names(grp_norms) <- c(grp_name, "norm")

  return(list(
    grp_deviations = grp_deviations,
    grp_norms      = grp_norms
  ))
}

## example:
# iris |> pt_ctr_dist(grp_name = "Species") |> str()
# List of 2
# $ grp_deviations:'data.frame':	150 obs. of  5 variables:
#   ..$ Sepal.Length: num [1:150] 0.094 -0.106 -0.306 -0.406 -0.006 ...
# ..$ Sepal.Width : num [1:150] 0.072 -0.428 -0.228 -0.328 0.172 ...
# ..$ Petal.Length: num [1:150] -0.062 -0.062 -0.162 0.038 -0.062 ...
# ..$ Petal.Width : num [1:150] -0.046 -0.046 -0.046 -0.046 -0.046 0.154 0.054 -0.046 -0.046 -0.146 ...
# ..$ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ grp_norms_tbl : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
# ..$ Species: Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
# ..$ norm   : num [1:150] 0.141 0.448 0.417 0.525 0.189 ...


##
#  EOF
##
