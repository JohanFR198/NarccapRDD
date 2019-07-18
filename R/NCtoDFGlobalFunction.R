#' This  function coverts the .nc downloaded  from NARCCAP  in a data.frame object.
#' This proccess may require high RAM capacity
#'

#' @keywords datasets, download, data
#' @export
#' @param PATH Is the directory where is located the .nc files of a same variable
#' @examples NC2DF("~/PrecipitationGlobalModelData")
#' @import dplyr stringr  ncdf4 lubridate reshape2 sp
#'


NC2DFG <- function(PATH){
  dirbase <- PATH
  listfilesg <-
    list.files(path = dirbase)[str_detect(list.files(path = dirbase), 'nc$')]
  var <- NULL

  c <- sub("\\_.*", "", listfilesg[1])
  fecha <- '1870-01-01'
  for (i in 1:length(listfilesg)) {
    show(paste0('Construccion datos mensuales-', i))
    vglobal <- nc_open(paste0(dirbase, listfilesg[i]))
    var_pre <- ncvar_get(vglobal, c)
    lonvar <- ncvar_get(vglobal, 'lon')
    latvar <- ncvar_get(vglobal, 'lat')
    timevar <- ncvar_get(vglobal, 'time')

    fechabase <- ymd(fecha)
    timevar <- fechabase + as.period(ddays(timevar))

    dimnames(var_pre)[[1]] <- lonvar
    dimnames(var_pre)[[2]] <- latvar
    dimnames(var_pre)[[3]] <- as.character(timevar)

    var_pre <- melt(var_pre)

    colnames(var_pre) <- c('lon', 'lat', 'Time', c)

    var_pre <-
      var_pre %>% filter(lat >= blatitude[1],
                         lat <= blatitude[2],
                         lon >= blongitude[1],
                         lon <= blongitude[2]) %>%
      mutate(Time = ymd(as.character(Time))) %>% filter(Time >= ymd('1968-01-01')) %>%
      mutate(Year = year(Time), Month = month(Time)) %>% group_by(Year, Month, lon, lat) %>%
      summarise(m = mean(c)) %>% ungroup()

    var <- bind_rows(var, var_pre)
  }
assign(paste(c, "Global"),var)
rm(var)

}
