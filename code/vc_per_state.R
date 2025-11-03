#####
###
#     vc_per_state.R
#
#       1970s violent crime per US state: construct tibbles.
###
#####

##
#  get_state_abb_nm()
#    Return tibble of (abbreviation, name) per state
##
get_state_abb_nm <- function() {
  st_abb_nm <- tibble::tibble(
    st_abb = datasets::state.abb, 
    st_nm  = datasets::state.name)
  return(st_abb_nm)
}

##
#  get_state_ctr()
#    Return tibble of (abbreviation, name) per state
##
get_state_ctr <- function() {
  st_ctr_df <- data.frame(
    datasets::state.center, 
    row.names = datasets::state.abb
  )
  ## Alaska and Hawaii are placed just off the West Coast (for compact map drawing):
  ## state.center2 := version of state.center with "correct" coordinates for AK & HI:
  ## From https://pubs.usgs.gov/gip/Elevations-Distances/elvadist.html#Geographic%20Centers
  ##   Alaska   63°50' N., 152°00' W., 60 miles northwest of Mount McKinley
  ##   Hawaii   20°15' N., 156°20' W., off Maui Island
  st_ctr_df ["AK",] <- c(-152.  , 63.83) # or  c(-152.11, 65.17)
  st_ctr_df ["HI",] <- c(-156.33, 20.25) # or  c(-156.69, 20.89)
  
  st_ctr_tbl <- st_ctr_df |> 
    tibble::as_tibble() |> 
    # include state abbreviation
    dplyr::mutate(
      st_abb = datasets::state.abb, 
      st_nm  = datasets::state.name) |> 
    dplyr::select(st_abb, st_nm,  dplyr::everything())
  
  return(st_ctr_tbl)
}

##
#  list_state_arrests()
#    Return tibbles of (assault, rape, murder) per state.
#    1973 violent crime stats from McNeil, 1977.
##
list_state_arrests <- function() {
  ## 
  #  Corrections:
  #    - per R documentation, correct errors in UrbanPop pct 
  ## 
  arrests_df <- datasets::USArrests
  arrests_df ["Maryland", "UrbanPop"] <- 76.6
  
  s_lo <- c("Colorado", "Florida", "Mississippi", "Wyoming")
  s_hi <- c("Nebraska", "Pennsylvania")
  
  ## 
  #  wide version
  ## 
  arrests_df [s_lo, "UrbanPop"] <- arrests_df [s_lo, "UrbanPop"] + 0.5
  arrests_df [s_hi, "UrbanPop"] <- arrests_df [s_hi, "UrbanPop"] - 0.5
  
  arrests_wide <- arrests_df |> 
    tibble::as_tibble(rownames = "st_nm") |> 
    # include state abbreviation
    dplyr::mutate(st_abb = datasets::state.abb) |> 
    dplyr::select(st_abb, st_nm,  dplyr::everything())
  
  ## 
  #  long version
  ## 
  arrests_long <- arrests_wide |> 
    tidyr::pivot_longer(
      cols = c(UrbanPop, Assault, Rape, Murder), 
      names_to = "var", 
      values_to = "rate"
    )
  
  return(list(
    arrests_wide = arrests_wide, 
    arrests_long = arrests_long
  ))
}

##
#  get_state_x77()
#    Return tibble of datasets::state.x77
##
get_state_x77 <- function() {
  # matrix
  st_77_mat <- datasets::state.x77
  
  # tibble
  st_77_tbl <- st_77_mat |> 
    tibble::as_tibble(rownames = "st_nm") |> 
    # 1976 non-negligent manslaughter per 100K
    dplyr::rename(Murder_76 = Murder) |> 
    
    dplyr::mutate(st_abb = datasets::state.abb) |> 
    dplyr::select(st_abb, st_nm,  dplyr::everything())
  
  return(st_77_tbl)
}

##
#  list_state_stats()
#    Join USArrests with other US state statistics
##
list_state_stats <- function() {
  
  ## 
  #  component tibbles
  ## 
  st_ctr_tbl   <- get_state_ctr()
  st_77_tbl    <- get_state_x77()
  arrest_lst   <- list_state_arrests()
  arrests_wide <- arrest_lst$ arrests_wide
  arrests_long <- arrest_lst$ arrests_long
  
  ## 
  #  compilation
  ## 
  st_wide <- 
    # (x, y) = (lat, long)
    st_ctr_tbl |> 
    # geographic labels
    dplyr::mutate(
      division = datasets::state.division, 
      region   = datasets::state.region) |> 
    # x77 statistics
    dplyr::left_join(
      y  = st_77_tbl |> dplyr::select(- st_nm), 
      by = "st_abb") |> 
    # 1973 arrests
    dplyr::left_join(
      y  = arrests_wide |> dplyr::select(- st_nm), 
      by = "st_abb")
  
  ## 
  #  st_wide: describe each variable
  ## 
  st_wide_dscr <- tibble::tribble(
    ~var, ~year, ~unit, ~dscr, 
    
    "st_abb", 1963L, "chr",      "2-letter abbreviation of st_nm", 
    "st_nm",  1959L, "chr",      "state name", 
    
    "x",      1959L, "lat-long", "longitude of state center", 
    "y",      1959L, "lat-long", "latitude of state center", 
    
    "division", 1959L, "fct",    "9-level geo partition", 
    "region  ", 1959L, "fct",    "4-level geo partition", 
    
    "Population", 1975L, "1000", "estimated population", 
    "Income",     1974L, "USD",  "average income", 
    "Illiteracy", 1970L, "PCT",  "illiterate percent of population", 
    "Life Exp",   1969L, "YR",   "life expetency in years", 
    "Murder_76",  1976L, "100K", "murder rate per 100K population", 
    "HS Grad",    1970L, "PCT",  "percent high-school graduates", 
    "Frost",      1960L, "DAY",  "avg number of days below freezing", 
    "Area",       1959L, "mi^2", "land area in square miles", 
    
    "Assualt",   1973L, "100K",  "assault arrests per 100K population", 
    "Rape",      1973L, "100K",  "rape arrests per 100K population", 
    "Murder",    1973L, "100K",  "murder arrests per 100K population", 
    "UrbanPop",  1973L, "PCT",   "percent of population in urban areas"
  )
  
  return(list(
    st_ctr_tbl   = st_ctr_tbl, 
    st_77_tbl    = st_77_tbl, 
    arrests_wide = arrests_wide, 
    arrests_long = arrests_long, 
    st_wide      = st_wide, 
    st_wide_dscr = st_wide_dscr
  ))
}


##
#  EOF
##
