#####
###
#     gen_ds_tbl.R
#
#     compile a tibble of data set names
###
#####

###
#   get_all_ds_per_pkg()
#
#     return tibble of all data set names per package
#
#     note: named packages are assumed to installed
###
get_all_ds_per_pkg <- function(
    pkgs # <chr> package name(s)
) {
  # all_ds_tbl: all data sets per package
  all_ds_tbl <- tibble::tibble()

  # identify distinct package names
  unique_pkg_names <- unique(pkgs)

  # accumulate names and titles of data sets
  for (p in unique_pkg_names) {
    pkg_lst <- data(package = p)
    pkg_tbl <- pkg_lst$ results [, 3:4] |>
      tibble::as_tibble() |>
      dplyr::rename(ds = Item, title = Title) |>
      dplyr::mutate(pkg = p) |>
      dplyr::select(pkg, ds, title)

    all_ds_tbl <- bind_rows(all_ds_tbl, pkg_tbl)
  }
  return(all_ds_tbl)
}

###
#   select_ds_per_pkg()
#
#     return tibble of selected data set names per package
#
#     "pkgs" notes
#     1: named packages are assumed to installed
#     2: length must be consistent with "datasets"
###
select_ds_per_pkg <- function(
    pkgs,    # <chr> package name(s)
    datasets # <chr> names of data sets per package
) {
  # initialize ds_tbl
  ds_tbl <- tibble::tibble(
    pkg = pkgs,
    ds  = datasets) |>
    dplyr::arrange(pkg, ds)

  # fetch data set title
  ds_title_vec <- vector(mode = "character")
  for (p in ds_tbl$ pkg |> unique()) {
    # names of specified data sets within this package
    ds_target_names <- ds_tbl |>
      dplyr::filter(pkg == p) |>
      dplyr::pull(ds)

    # tibble of all data sets within this package
    pkg_lst <- data(package = p)
    pkg_tbl <- pkg_lst$ results [, 3:4] |>
      tibble::as_tibble() |>
      dplyr::rename(ds = Item, title = Title) |>
      dplyr::mutate(pkg = p) |>
      dplyr::select(pkg, ds, title)

    # sub-tibble of specified data sets
    sub_tbl <- pkg_tbl |>
      dplyr::filter(
        ds %in% ds_target_names)

    # update vector of ds titles
    ds_title_vec <- c(ds_title_vec, sub_tbl$ title)
  }

  # append ds titles as new column in ds_tbl
  ds_tbl <- ds_tbl |>
    dplyr::mutate(title = ds_title_vec)

  return(ds_tbl)
}

###
# EOF
###
