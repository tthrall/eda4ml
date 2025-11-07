#####
###
#     galton_ht_data.R
#
#       Manipulate HistData::GaltonFamilies 
#       (adult heights per family)
###
#####

##
#  get_galton_3d()
#    Return list including tibble of 
#    (mother, father, oldest child).
##
get_galton_3d <- function() {
  galton_3d <- HistData::GaltonFamilies |> 
    tibble::as_tibble() |> 
    dplyr::group_by(family) |> 
    # Oldest child
    dplyr::filter(childNum == 1) |> 
    dplyr::select(father, mother, childHeight, gender) |> 
    dplyr::rename(child = childHeight) |> 
    dplyr::ungroup()
  
  counts_lst <- list(
    n_children_all      = nrow(GaltonFamilies), 
    n_families          = nrow(galton_3d), 
    n_child_1_daughters = sum(galton_3d$ gender == "female"), 
    n_child_1_sons      = sum(galton_3d$ gender == "male")
  )
  
  return(list(
    galton_3d  = galton_3d, 
    counts_lst = counts_lst
  ))
}


##
#  EOF
##
