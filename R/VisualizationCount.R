#' narccapCount function generates a graph that compares the number of global points with the number of regional points
#'
#'
#'

#' @keywords Visualization, Climate data
#' @export
#'
#' @param data Data frame objet generated with NC2DF function
#' @param year A year of the dataset
#' @param month Number of the month
#' @examples
#' @import dplyr stringr  ncdf4 lubridate reshape2 sp ggmap ggplot2
#'



narccapCOUNT <- function(datos, year, month){

  str_detect(colnames(datos), "global")
  B <- datos %>% group_by(indicegrid, datos[,6]) %>% summarise(n = n())
  G5 <-
    ggplot(data = NULL) + geom_histogram(aes(x = indicegrid), bins = 500, color =
                                           "#E69F00") + geom_histogram(
                                             data = B,
                                             aes(x = indicegrid),
                                             color = "#56B4E9",
                                             bins = 500
                                           ) + theme_bw() + ylab("Cuenta") + xlab("Índice en el Espacio")
  return(G5)
}
