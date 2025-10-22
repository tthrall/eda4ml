#####
###
#     tibble_transpose.R
#
#     TODO: error - mutate not applicable to type matrix
#
#       Swap the rows and columns of a tibble, with the
#       possible exception of a designated column to be
#       used in the transposed tibble as column names.
###
#####

##
#  tbl_trnsp()
#
#       Input:  df, a data frame
#       Output: df_t, a tibble, the transpose of df
#
#       Note:
#       1.  df column names appear in df_t col 1
#       2.  new_row_hdr:  desired name for df_t col 1
#       3.  new_col_names_from: the df col giving df_t col names
##
tbl_trnsp <- function(
    df, # <tbl> matrix, data frame, or tibble
    new_row_hdr = "var", # <chr> name of df_t col 1
    new_col_names_from = NULL # <chr> df col of df_t col names
  ) {
  # remove df row-names, if any, to be restored later
  df_tbl <- df |> as_tibble()

  # new row names
  if (is.null(colnames(df))) {
    new_row_names <- paste0("c_", 1:ncol(df))
  } else {
    new_row_names <- colnames(df)
  }

  # remove identified naming column, if any, from df_tbl
  if (is.null(new_col_names_from)) {
    # transpose all df columns
    new_col_names <- paste0("r_", 1:nrow(df))
  } else {
    # remove the identified column from df_tbl prior to transpose
    new_col_names <- df_tbl |> pull(new_col_names_from)
    df_tbl <- df_tbl |>
      dplyr::select(- new_col_names_from)
    # update new row names
    new_row_names <- colnames(df_tbl)
  }

  df_t <- df_tbl |> t() |>
    mutate(
      new_col_1 = new_row_names
    ) |>
    dplyr::select(new_col_1, everything())
  colnames(df_t) <- new_col_names

  return(df_t)
}


##
#  EOF
##
