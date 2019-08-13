#' narccapMAP function generates a map of the chosen variable in a specific month and year
#'
#'
#'

#' @keywords Map, Visualization, Climate data
#' @export
#'
#' @param data Data frame objet generated with NC2DF function
#' @param var Name of the variable chosen to be visualized
#' @param year A year from 1968 to 1999
#' @param month Number of the month you want
#' @examples
#' @import dplyr stringr  ncdf4 lubridate reshape2 sp ggmap ggplot2

narccapMAP <- function(datos, var, year, month) {
  lat <- c(min(datos$lat), max(datos$lat))
  long <- c(min(datos$lon), max(datos$lon)) - 360
  bbox <- make_bbox(long, lat, f = 0.05)
  b <-
    get_map(bbox,
            maptype = "toner-lite",
            source = "stamen",
            color = "bw")
  ggmap(b)
  YY <- year
  MM <- month
  mes <- c(
    "enero",
    "febrero",
    "marzo",
    "abril",
    "mayo",
    "junio",
    "julio",
    "agosto",
    "setiembre",
    "agosto",
    "octubre",
    "noviembre",
    "diciembre"
  )
  G6 <-
    ggmap(b) + geom_point(data = datos[datos$Year == YY &
                                         datos$Month == MM, ],
                          aes(lon - 360, lat, color = eval(as.name(var))),
                          alpha = 0.9) +
    scale_color_viridis_c() +
    labs(
      x = "Longitud",
      y = "Latitud",
      title = paste("CRCM -", var, "en", mes[MM], "de", YY)
    ) +
    theme_classic()
  return(G6)
}
