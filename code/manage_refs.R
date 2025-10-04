#####
###
#     manage_refs.R
#       manage the listing of reference materials
###
#####

###
#   bib2tbl()
#     create a tibble from a BibTex file
###
bib2tbl <- function(
    file,        # <chr> path to input BibTex file 
    x,           # <tbl> tibble containing (title, link, auth)
    t_idx = 1L,  # <int> index of column containing title
    l_idx = 2L,  # <int> index of column containing link
    a_idx = NULL # <int> index of column containing author
) {
  # read each line of the file as one element of a string vector
  line_vec <- file |> readr::read_lines(skip_empty_rows = TRUE)
  
  # extract those lines containing the BibTex citation key
  has_key   <- line_vec |> stringr::str_detect("^@")
  key_lines <- line_vec[has_key]
  
  # type of reference (article, book, misc, ...)
  ref_type <- key_lines |> stringr::str_extract("[a-z]+")
  
  # citation key (ref_key)
  a_pat <- stringr::regex("@article\\{")
  b_pat <- stringr::regex("@book\\{")
  m_pat <- stringr::regex("@misc\\{")
  ref_key <- key_lines
  ref_key <- ref_key |> stringr::str_remove(a_pat)
  ref_key <- ref_key |> stringr::str_remove(b_pat)
  ref_key <- ref_key |> stringr::str_remove(m_pat)
  ref_key <- ref_key |> stringr::str_remove(",")
  
  # extract author
  # author       <- rep(NA, length(key_lines))
  has_author   <- line_vec |> stringr::str_detect("author = ")
  author_lines <- line_vec[has_author]
  author_vec   <- author_lines
  author_vec   <- author_vec |> stringr::str_remove("author = \\{")
  author_vec   <- author_vec |> stringr::str_remove("\\},")
  author_vec   <- author_vec |> stringr::str_squish()
  # author[]
  
  bib_tbl <- tibble::tibble(
    type = ref_type, 
    tag  = ref_key
  )
  
  return(list(
    bib_tbl = bib_tbl, 
    authors = author_vec
  ))
}