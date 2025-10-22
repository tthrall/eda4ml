#####
###
#     olympic_running.R
#
#       Plot winning times of runners in the World Olympics.
###
#####

##
#  plot_olympic_running()
##
plot_olympic_running <- function() {
  library(tsibbledata)
  library(ggplot2)
  g_olympic_running <- olympic_running |>
    group_by(Length) |>
    ggplot(mapping = aes(
      x = Year, y = Time, colour = Sex)) +
    geom_line() +
    geom_point(size = 1) +
    facet_wrap(~ Length, scales = "free_y", nrow = 2) +
    theme_minimal() +
    scale_color_brewer(palette = "Dark2") +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ylab("Running time (seconds)")
  return(g_olympic_running)
}


##
#  EOF
##
