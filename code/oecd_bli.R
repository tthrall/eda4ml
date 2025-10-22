#####
###
#     oecd_bli.R
#       meta-data for OECD Better Life Index (BLI)
#       
#       Two types of function in this file:
#       get_x():    data derived from OECD package
#       assert_x(): data entered by hand
###
#####

##
#  get_bli_long()
#       
#    # source data, using total measures per country, without 
#    # conditioning on gender, socio-economic status, etc.
##
get_bli_long <- function(
    ineq = "TOT" # <chr> use total versus conditioned measures
) {
  bli_long <- OECD::get_dataset("BLI")
  
  # timestamp
  # now()
  # [1] "2025-10-15 20:27:25 BST"
  
  # inspect structure
  # str(bli_long)
  # tibble [2,369 × 8] (S3: tbl_df/tbl/data.frame)
  #  $ INDICATOR   : chr [1:2369] "CG_SENG" "CG_VOTO" "EQ_AIRP" "EQ_WATER" ...
  #  $ INEQUALITY  : chr [1:2369] "TOT" "TOT" "TOT" "WMN" ...
  #  $ LOCATION    : chr [1:2369] "AUS" "AUS" "AUS" "AUS" ...
  #  $ MEASURE     : chr [1:2369] "L" "L" "L" "L" ...
  #  $ OBS_STATUS  : chr [1:2369] "A" "A" "A" "A" ...
  #  $ ObsValue    : chr [1:2369] "2.7" "92" "6.7" "91" ...
  #  $ UNIT_MEASURE: chr [1:2369] "AVSCORE" "PC" "MICRO_M3" "PC" ...
  #  $ UNIT_MULT   : chr [1:2369] "0" "0" "0" "0" ...
  
  # inspect values of INEQUALITY factor
  # 
  # bli_long$ INEQUALITY |> as_factor() |> summary()
  # TOT WMN  MN  LW HGH 
  # 935 558 558 159 159
  
  # convert (ObsValue, UNIT_MULT) to numeric values
  bli_long <- bli_long |> 
    dplyr::mutate(
      ObsValue = as.numeric(ObsValue), 
      UNIT_MULT = as.numeric(UNIT_MULT)
    )
  
  # default: use total measures per country, without 
  # conditioning on gender, socio-economic status, etc.
  if (! is.null( ineq ) ) {
    bli_long <- bli_long |>
      dplyr::filter(INEQUALITY == ineq)
  }
  
  return(bli_long)
}

##
#  get_bli_locations()
#    return tbl showing the correspondence of 
#    (INDICATOR, UNIT_MEASURE)
# 
##
get_bli_locations <- function(
    bli_long = get_bli_long() # <tbl> BLI tibble (long config)
) {
  bli_locations <- bli_long |> 
    dplyr::group_by(LOCATION) |> 
    dplyr::summarise(
      .groups = "keep", 
      # number of indicator components
      n = n()
    )
  return(bli_locations)
}

##
#  get_comp_units()
#    return tbl showing the correspondence of 
#    (INDICATOR, UNIT_MEASURE)
# 
##
get_comp_units <- function(
    bli_long = get_bli_long() # <tbl> BLI tibble (long config)
) {
  comp_units <- bli_long |> 
    dplyr::group_by(INDICATOR, UNIT_MEASURE) |> 
    dplyr::summarise(
      .groups = "keep", 
      # number of countries
      n = n()
    )
  return(comp_units)
}

##
#  assert_bli_wide()
#    # BLI data in wide configuration
##
assert_bli_wide <- function(
    bli_long = get_bli_long() # <tbl> BLI tibble (long config)
) {
  bli_wide <- bli_long |> 
    dplyr::select(INDICATOR, LOCATION, ObsValue) |> 
    tidyr::pivot_wider(
      id_cols    = LOCATION, 
      names_from = INDICATOR, 
      values_from = ObsValue)
  
  # include the location name (country)
  bli_wide <- bli_wide |> 
    dplyr::rename(code = LOCATION) |> 
    dplyr::left_join(
      y  = assert_bli_countries(), 
      by = "code") |> 
    dplyr::select(code, country, everything())
  
  return(bli_wide)
}

##
#  assert_bli_countries()
#    name and 3-letter code of BLI countries (locations)
#  
#    Link:
#      Classification des risques pays actuelle
#      <https://www.oecd.org/content/dam/oecd/en/topics/policy-sub-issues/country-risk-classification/cre-crc-current-english.pdf>
# 
##
assert_bli_countries <- function() {
  ctry_tbl <- tibble::tribble(
    ~code, ~country, 
    "AUS", "Australia",  
    "AUT", "Austria",  
    "BEL", "Belgium",  
    "BRA", "Brazil",  
    "CAN", "Canada",  
    "CHE", "Switzerland",  
    "CHL", "Chile",  
    "COL", "Colombia",  
    "CRI", "Costa Rica",  
    "CZE", "Czechia",  
    "DEU", "Germany",  
    "DNK", "Denmark",  
    "ESP", "Spain",  
    "EST", "Estonia",  
    "FIN", "Finland",  
    "FRA", "France",  
    "GBR", "United Kingdom",  
    "GRC", "Greece",  
    "HUN", "Hungary",  
    "IRL", "Ireland",  
    "ISL", "Iceland",  
    "ISR", "Israel",  
    "ITA", "Italy",  
    "JPN", "Japan",  
    "KOR", "Korea",  
    "LTU", "Lithuania",  
    "LUX", "Luxembourg",  
    "LVA", "Latvia",  
    "MEX", "Mexico",  
    "NLD", "Netherlands",  
    "NOR", "Norway",  
    "NZL", "New Zealand",  
    "OECD", "OECD",  
    "POL", "Poland",  
    "PRT", "Portugal",  
    "RUS", "Russia",  
    "SVK", "Slovak Republic",  
    "SVN", "Slovenia",  
    "SWE", "Sweden",  
    "TUR", "Türkiye",  
    "USA", "United States",  
    "ZAF", "South Africa")
  return(ctry_tbl)
}

##
#  assert_bli_component_indicators()
#    return the code and description of each BLI indicator component
##
assert_bli_component_indicators <- function() {
  bli_component_tbl <- tibble::tribble(
    ~code, ~pre_name, ~post_name, 
    "CG_SENG", "Civic Engagement", "Stakeholder Engagement", 
    "CG_VOTO", "Civic Engagement", "Voter Turnout", 
    "EQ_AIRP", "Environmental Quality", "Air Pollution", 
    "EQ_WATER", "Environmental Quality", "Water Quality", 
    "ES_EDUA", "Education System", "Educational Attainment", 
    "ES_EDUEX", "Education System", "Expected Years of Education", 
    "ES_STCS", "Education System", "Student Cognitive Skills", 
    "HO_BASE", "Housing", "Dwellings w/o Basic Facilities", 
    "HO_HISH", "Housing", "Housing Expenditure", 
    "HO_NUMR", "Housing", "Rooms per Person", 
    "HS_LEB", "Health Status", "Life Expectancy at Birth", 
    "HS_SFRH", "Health Status", "Self-Reported Health", 
    "IW_HADI", "Income and Wealth", "Household Adjusted Disposable Income", 
    "IW_HNFW", "Income and Wealth", "Household Net Financial Wealth", 
    "JE_EMPL", "Jobs Employment", "Employment Rate", 
    "JE_LMIS", "Jobs Employment", "Labour Market Insecurity", 
    "JE_LTUR", "Jobs Employment", "Long-Term Unemployment Rate", 
    "JE_PEARN", "Jobs Employment", "Personal Earnings", 
    "PS_FSAFEN", "Personal Safety", "Feeling Safe Walking Alone at Night", 
    "PS_REPH", "Personal Safety", "Homicide Rate", 
    "SC_SNTWS", "Social Connections", "Support Network Quality", 
    "SW_LIFS", "Subjective Well-Being", "Life Satisfaction", 
    "WL_EWLH", "Work Life Balance", "Employees Working Long Hours", 
    "WL_TNOW", "Work Life Balance", "Time Devoted to Leisure and Personal Care", )
  
  # split the code into a prefix and suffix
  bli_component_tbl <- bli_component_tbl |> 
    dplyr::mutate(
      prefix = code |> stringr::str_split_i(pattern = "_", i = 1), 
      suffix = code |> stringr::str_split_i(pattern = "_", i = 2)
    ) |> 
    dplyr::select(code, prefix, suffix, everything())
  
  # add unit of measure of each component
  comp_units <- tibble::tribble(
    ~code, ~unit, 
    "CG_SENG",   "AVSCORE", 
    "CG_VOTO",   "PC", 
    "EQ_AIRP",   "MICRO_M3", 
    "EQ_WATER",  "PC", 
    "ES_EDUA",   "PC", 
    "ES_EDUEX",  "YR", 
    "ES_STCS",   "AVSCORE", 
    "HO_BASE",   "PC", 
    "HO_HISH",   "PC", 
    "HO_NUMR",   "RATIO", 
    "HS_LEB",    "YR", 
    "HS_SFRH",   "PC", 
    "IW_HADI",   "USD", 
    "IW_HNFW",   "USD", 
    "JE_EMPL",   "PC", 
    "JE_LMIS",   "PC", 
    "JE_LTUR",   "PC", 
    "JE_PEARN",  "USD", 
    "PS_FSAFEN", "PC", 
    "PS_REPH",   "RATIO", 
    "SC_SNTWS",  "PC", 
    "SW_LIFS",   "AVSCORE", 
    "WL_EWLH",   "PC", 
    "WL_TNOW",   "HOUR")
  
  bli_component_tbl <- bli_component_tbl |> 
    dplyr::left_join(
      y  = comp_units,
      by = "code"
    ) |> 
    dplyr::select(code, prefix, suffix, unit, everything())
  
  # add descriptions of each component
  comp_dscr <- tibble::tribble(
    ~code, ~dscr, 
    "CG_SENG",   "Extent to which people can engage with government in rule-making", 
    "CG_VOTO",   "Percent of registered voters who voted in recent elections", 
    "EQ_AIRP",   "Concentration of PM2.5 particulate matter (micrograms per cubic meter)", 
    "EQ_WATER",  "Percent satisfied with water quality", 
    "ES_EDUA",   "Percent aged 25-64 with at least upper-secondary education", 
    "ES_EDUEX",  "Expected years of schooling", 
    "ES_STCS",   "PISA scores in reading, mathematics, and science", 
    "HO_BASE",   "Percentage of dwellings that lack basic sanitary facilities", 
    "HO_HISH",   "Percentage of household gross adjusted disposable income spent on housing", 
    "HO_NUMR",   "Number of rooms per person in dwelling", 
    "HS_LEB",    "Average number of years a person can expect to live", 
    "HS_SFRH",   "Percentage who report being in good or very good health", 
    "IW_HADI",   "Average household income after taxes", 
    "IW_HNFW",   "Household net financial wealth (financial assets minus liabilities)", 
    "JE_EMPL",   "Percentage of people aged 15-64 in paid employment", 
    "JE_LMIS",   "Expected loss of earnings if someone becomes unemployed", 
    "JE_LTUR",   "Percentage unemployed for 12+ months", 
    "JE_PEARN",  "Average annual earnings per full-time employee", 
    "PS_FSAFEN", "Percentage who feel safe", 
    "PS_REPH",   "Deaths per 100,000 people", 
    "SC_SNTWS",  "Percentage who believe they have someone to rely on in times of need", 
    "SW_LIFS",   "Average self-evaluation on a scale from 0 to 10", 
    "WL_EWLH",   "Percentage of employees working 50+ hours per week", 
    "WL_TNOW",   "Hours per day spent on leisure, personal care, eating, and sleeping")
  
  bli_component_tbl <- bli_component_tbl |> 
    dplyr::left_join(
      y  = comp_dscr,
      by = "code"
    )
  return(bli_component_tbl)
}

##
#  assert_bli_comp_prefix()
#    return the prefix and name of each of the BLI indicators
##
assert_bli_comp_prefix <- function() {
  bli_component_tbl <- assert_bli_component_indicators()
  bli_comp_prefix   <- bli_component_tbl |> 
    dplyr::group_by(prefix) |> 
    dplyr::summarise(
      .groups  = "keep", 
      name     = pre_name[1], 
      n_comps  = n(), 
      comps    = stringr::str_flatten_comma(code)
    )
  return(bli_comp_prefix)
}


##
#  EOF
##
