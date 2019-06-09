#' DESCRIPTION OF THE FUNCTION
#'
#'
#'
#'
#'

#' @keywords datasets, download, data
#' @export
#' @param PATH Is the directory where is located the .nc files of a same variable
#' @examples
#' NC2DFG("~/PrecipitationGlobalModelData")
#'


NC2DFG <- function(PATH) {
  dirbaseglobal <- PATH
  listfilesglobal <-
    list.files(path = dirbaseglobal)[str_detect(list.files(path = dirbaseglobal), 'nc$')]
  var <- NULL

  c <- gsub("[^::A-Z::]", "", listfilesglobal[1])



  for (i in 1:length(listfilesglobal)) {
    show(paste0('Construccion datos mensuales-Global-', i))
    vglobal <- nc_open(paste0(dirbaseglobal, listfilesglobal[i]))
    var_pre <- ncvar_get(vglobal, c)
    lonvar <- ncvar_get(vglobal, 'lon')
    latvar <- ncvar_get(vglobal, 'lat')
    timevar <- ncvar_get(vglobal, 'time')

    fechabase <- ymd('1870-01-01')
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
}
