#' VisualizeDATA function generates a map or a plot of the chosen variable in a specific month and year
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

VisualizeDATA <- function(datos, var, year, month, type) {
  if (type == "counts") {
    png(filename = "Gr5.png")
    B <- PRtot %>% group_by(indicegrid, PRglobal) %>% summarise(n = n())
    G5 <-
      ggplot(data = NULL) + geom_histogram(aes(x = indicegrid), bins = 500, color =
                                             "#E69F00") + geom_histogram(
                                               data = B,
                                               aes(x = indicegrid),
                                               color = "#56B4E9",
                                               bins = 500
                                             ) + theme_bw() + ylab("Cuenta") + xlab("Índice en el Espacio")
    G5
    dev.off()
  }
  else if (type == "map") {
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
    png(filename = "Gr6.png")
    G6 <-
      ggmap(b) + geom_point(data = datos[datos$Year == YY &
                                           datos$Month == MM, ],
                            aes(lon - 360, lat, color = variable),
                            alpha = 0.9) +
      scale_color_viridis_c(name = "Temp en K") +
      labs(
        x = "Longitud",
        y = "Latitud",
        title = paste("MCR - Temperatura en", mes[MM], "de", YY)
      ) +
      theme_classic()
    G6
    dev.off()
  }
}
