#####
###
#     handedness_data.R
#
#       Record the handedness data reported in FPP (4e).
#
#       Statistics (4e) by Freedman, Pisani, and Purves.
#       Chapter 28: The Chi-Squared Test
#       Section  4: Testing Independence
###
#####

##
#  gen_handedness()
##
gen_handedness <- function() {
  hs_tbl <- tibble::tribble(
    ~sex, ~hnd, ~count,
    "male",   "right",  934L,
    "male",   "left",   113L,
    "male",   "ambi",    20L,
    "female", "right", 1070L,
    "female", "left",    92L,
    "female", "ambi",     8L
  )
  return(hs_tbl)
}


##
#  EOF
##
