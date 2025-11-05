#####
###
#     NCI60_helper.R
#
#       Manage NIC60 data from the ISLR2 package.
#       Note: this is NOT the colonCA micro-array data 
#       from Alon 1999.
###
#####

##
#  install_colonCA()
##
install_colonCA <- function() {
  if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  
  BiocManager::install("colonCA")
}


##
#  EOF
##
