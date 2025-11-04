#####
###
#     wine_quality_uci.R
#
#       wine quality data from UC Irvine: download and process.
###
#####

##
#  download_wine_quality()
#    Download wine quality data from UC Irvine.
##
download_wine_quality <- function() {
  # Create temporary directory for download
  temp_dir <- tempdir()
  zip_file <- file.path(temp_dir, "wine_quality.zip")
  
  # Download the zip file
  url <- "https://archive.ics.uci.edu/static/public/186/wine+quality.zip"
  download.file(url, zip_file, mode = "wb")
  
  # Unzip
  unzip(zip_file, exdir = temp_dir)
  
  # Read both red and white wine datasets
  red_wine   <- 
    file.path(temp_dir, "winequality-red.csv") |> 
    readr::read_delim(delim = ";")
  white_wine <- 
    file.path(temp_dir, "winequality-white.csv") |> 
    readr::read_delim(delim = ";")
  
  # Add wine color indicator
  red_wine$   color <- "red"
  white_wine$ color <- "white"
  
  # Combine datasets
  wine_quality <- rbind(red_wine, white_wine)
  
  # Clean up
  unlink(zip_file)
  
  return(wine_quality)
}

##
#  get_wine_quality()
#    Return wine quality data.
##
get_wine_quality <- function(
    cache_dir = NULL # <chr> file-path to directory containing wine data
) {
  ## 
  # set file name and file path
  ## 
  wq_fn <- "wine_quality_uci.rds"
  if (is.null(cache_dir)) {
    cache_dir <- here::here("data", "rds")
  }
  cache_file <- file.path(cache_dir, wq_fn)
  
  ## 
  # load or download file
  ## 
  if (file.exists(cache_file)) {
    # load cached version
    wine_quality <- cache_file |> readr::read_rds()
  } else {
    # download data from UC Irvine
    wine_quality <- download_wine_quality()
    
    # save for future use
    wine_quality |> readr::write_rds(cache_file)
  }
  return(wine_quality)
}

##
#  describe_wq_vars()
##
describe_wq_vars <- function() {
  wq_var_dscr <- tibble::tribble(
    ~var, ~unit, 
    "fixed acidity", "g(tartaric acid)/dm3", 
    "volatile acidity", "g(acetic acid)/dm3", 
    "citric acid", "g/dm3", 
    "residual sugar", "g/dm3", 
    "chlorides", "g(sodium chloride)/dm3", 
    "free sulfur dioxide", "mg/dm3", 
    "total sulfur dioxide", "mg/dm3", 
    "density", "g/cm3", 
    "pH", "", 
    "sulphates", "g(potassium sulphate)/dm3", 
    "alcohol", "% volume", 
    "quality", "0:10", 
    "color", "{red, white}"
  )
  return(wq_var_dscr)
}

##
#  get_rw_quality_corr_tbl()
#    return tibble of cor(feature, quality) for (red, white) wines
##
get_rw_quality_corr_tbl <- function(
    wq_tbl # <tbl> get_wine_quality() tibble output
) {
  ## 
  # (feature, q_red)
  ## 
  red_wq_corr_mat <- wq_tbl |> 
    dplyr::filter(color == "red") |> 
    dplyr::select(- color) |> 
    cor()
  red_quality_corr_tbl <- 
    red_wq_corr_mat [1:11, "quality"] |> 
    tibble::as_tibble(rownames = "feature") |> 
    dplyr::rename(q_red = value)
  
  ## 
  # (feature, q_white)
  ## 
  white_wq_corr_mat <- wq_tbl |> 
    dplyr::filter(color == "white") |> 
    dplyr::select(- color) |> 
    cor()
  white_quality_corr_tbl <- 
    white_wq_corr_mat [1:11, "quality"] |> 
    tibble::as_tibble(rownames = "feature") |> 
    dplyr::rename(q_white = value)
  
  ## 
  # (feature, q_red, q_white)
  ## 
  rw_quality_corr_tbl <- 
    red_quality_corr_tbl |> 
    dplyr::left_join(
      y  = white_quality_corr_tbl, 
      by = "feature")
  
  return(rw_quality_corr_tbl)
}


##
#  EOF
##
