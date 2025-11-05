#####
###
#     colonCA_helper.R
#
#       Download and process colonCA micro-array data from Alon 1999.
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
