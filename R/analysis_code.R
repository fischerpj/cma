gg_ <- function(x= cum_()){
  x |>
    ggplot2::ggplot(mapping= ggplot2::aes(x= year, y=value, color= dc)) +
    ggplot2::geom_point()
}

cum_ <- function(x = nosolde_()){
  x |>
    dplyr::reframe(gt= sum(value), .by= c(dc,year)) |>
    dplyr::reframe(year, value= cumsum(gt), .by= dc)
  ##    dplyr::group_by(dc) |>
##    dplyr::summarise(value= cumsum(gt))
    
}