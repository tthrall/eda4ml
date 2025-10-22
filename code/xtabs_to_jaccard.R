#####
###
#     xtabs_to_jaccard.R
#
#       Render each cell of xtabs output as Jaccard similarity measure.
#       Return the resulting matrix of Jaccard measures, along with
#       their weighted average value.
###
#####

##
#  xtabs_to_jaccard()
##
xtabs_to_jaccard <- function(
    xt_counts # xtabs() output, named matrix of counts
    ) {
  # render xtabs output as a long tibble
  xt_ct_long <- xt_counts |> as_tibble()

  # sum of cell counts
  n_total <- xt_ct_long$ n |> sum(na.rm = TRUE)

  # express counts as relative frequencies
  xt_freq_long <- xt_ct_long |>
    mutate(freq = n / n_total)

  # marginal count of first grouping variable
  ct_1_tbl <- xt_ct_long |>
    summarise(
      .by = 1,
      n   = sum(n, na.rm = TRUE)
    )

  # marginal count of second grouping variable
  ct_2_tbl <- xt_ct_long |>
    summarise(
      .by = 2,
      n   = sum(n, na.rm = TRUE)
    )

  # outer sum of marginal counts
  outer_sum_mat <- outer(
    X   = ct_1_tbl$ n,
    Y   = ct_2_tbl$ n,
    FUN = "+"
  )
  rownames(outer_sum_mat) <- ct_1_tbl |> pull(1)
  colnames(outer_sum_mat) <- ct_2_tbl |> pull(1)

  # render outer sum as long tibble
  outer_sum_long <- outer_sum_mat |>
    tibble::as_tibble(
      rownames = names(ct_1_tbl) [[1]]
    ) |>
    pivot_longer(
      cols = -1,
      names_to = names(ct_2_tbl) [[1]],
      values_to = "outer_sum"
    )

  # include outer_sum as new xt_sim_long
  xt_sim_long <- xt_freq_long |>
    left_join(
      y  = outer_sum_long,
      by = names(outer_sum_long) [1:2]
    ) |>
    mutate(
      union   = outer_sum - n,
      jaccard = n / union
    )

  # weighted average of jaccard cell values
  sim_wt_avg <-
    (xt_sim_long$freq * xt_sim_long$jaccard) |>
    sum(na.rm = TRUE)

  return(list(
    xt_sim_long = xt_sim_long,
    sim_wt_avg  = sim_wt_avg
  ))
}


##
#  EOF
##
