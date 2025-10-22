#####
###
#     text_helpers.R
#
#       Support tidytext functions.
###
#####

##
#  para2token()
#    input:  a text file whose "lines" are actually paragraphs
#    return list of tibbles whose distinctive columns are:
#      - input_tbl [para]: original input paragraphs
#      - sentence_tbl [sntc]: sentences stripped of punctuation
#      - token_tbl [word]: words stripped of stop-words
#      - para_tbl [para]: paragraphs reconstructed from token_tbl
##
para2token <- function(
    file,                   # passed to readr::read_lines()
    skip_empty_rows = TRUE, # <lgl> ignore blank rows
    remove_stopword = TRUE, # <lgl> remove stop-words
    strip_punct     = TRUE  # <lgl> remove punctuation
) {

  # vector of input paragraphs
  input_vec <- readr::read_lines(
    file = file,
    skip_empty_rows = skip_empty_rows
  )

  # index input paragraphs in a tibble
  input_tbl <- tibble::tibble(
    pdx  = 1:length(input_vec),
    para = input_vec
  )

  # break each paragraph into sentences
  sentence_tbl <- input_tbl |>
    tidytext::unnest_sentences(
      output      = sntc,
      input       = para,
      strip_punct = strip_punct,
      format      = "text"
    ) |>
    dplyr::mutate(sdx = 1:length(sntc)) |>
    dplyr::select(pdx, sdx, sntc)

  # break each sentence into tokens
  token_tbl <- sentence_tbl |>
    tidytext::unnest_tokens(
      output = word,
      input  = sntc,
      format = "text"
    ) |>
    dplyr::mutate(wdx = 1:length(word)) |>
    dplyr::select(pdx, sdx, wdx, word)

  # remove stop-words
  if (remove_stopword) {
    token_tbl <- token_tbl |>
      dplyr::anti_join(
        tidytext::get_stopwords()
      )
  }

  # construct cleaned paragraphs from token_tbl
  para_tbl <- token_tbl |>
    dplyr::group_by(pdx) |>
    dplyr::summarise(
      para = stringr::str_flatten(word, collapse = " "))


  return(list(
    input_tbl    = input_tbl,
    sentence_tbl = sentence_tbl,
    token_tbl    = token_tbl,
    para_tbl     = para_tbl
  ))
}

##
#  list_lda_tbls()
#    return a list of tibbles from an LDA_XXX object
#
#    input:  object of class "LDA_VEM" or "LDA_Gibbs"
#    return: list of component tibbles
##
list_lda_tbls <- function(
    lda_xxx # <lst> output from topicmodels:LDA()
) {

  # determine class of input object
  lda_class <- lda_xxx |> class()
  assertthat::assert_that(
    lda_class %in% c("LDA_VEM", "LDA_Gibbs")
  )

  # z: topic assignment of successive words across corpus
  if (lda_class == "LDA_Gibbs") {
    z_tbl <- lda_xxx@ z |>
      tibble::as_tibble_col(column_name = "z")
  } else {
    z_tbl <- tibble::tibble()
  }

  # conc: initial Dirichlet concentration parameters
  if (lda_class == "LDA_Gibbs") {
    delta_ref <- lda_xxx@ control@ delta
  } else {
    delta_ref <- NULL
  }
  conc_tbl <- tibble::tribble(
    ~param, ~value,
    "alpha", lda_xxx@ alpha,
    "delta", delta_ref)

  # terms: unique words across the corpus
  terms_tbl <- tibble::tibble(
    tdx  = 1:length(lda_xxx@ terms),
    term = lda_xxx@ terms)

  # topic_labels: <chr> topic_idx
  K_topics  <- lda_xxx@ k
  K_digits  <- (1 + floor(log10(K_topics))) |>
    as.integer()
  topic_idx <- (1:K_topics) |>
    eeptools::leading_zero(digits = K_digits)
  topic_labels <- paste0("topic_", topic_idx)

  # beta: topic-specific ln(probability) across unique terms
  # Note: tibble is transpose of the K-by-N beta matrix
  beta_wide <- lda_xxx@ beta |>
    t() |> tibble::as_tibble()
  names(beta_wide) <- topic_labels

  # beta_terms:
  #   - append "terms" as new column
  #   - pivot to longer configuration
  beta_terms <- beta_wide |>
    dplyr::mutate(term = lda_xxx@ terms) |>
    tidyr::pivot_longer(
      cols         = - term,
      names_to     = "topic",
      names_prefix = "topic_",
      values_to    = "ln_prob"
    ) |>
    dplyr::mutate(topic = as.integer(topic)) |>
    dplyr::arrange(term)

  # gamma: prob(topic | doc), a D-by-K matrix
  gamma_tbl <- lda_xxx@ gamma |>
    tibble::as_tibble()
  names(gamma_tbl) <- paste0("topic_", 1:2)

  # dw_assign: topic assignment per (doc, term)
  dw_assign_tbl <- tibble::tibble(
    doc_idx   = lda_xxx@ wordassignments$ i,
    trm_idx   = lda_xxx@ wordassignments$ j,
    topic_idx = lda_xxx@ wordassignments$ v |> as.integer()
  )

  return(list(
    z_tbl         = z_tbl,
    conc_tbl      = conc_tbl,
    terms_tbl     = terms_tbl,
    beta_wide     = beta_wide,
    beta_terms    = beta_terms,
    gamma_tbl     = gamma_tbl,
    dw_assign_tbl = dw_assign_tbl
  ))
}


##
#  EOF
##
