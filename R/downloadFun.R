#' Show Narccap datasets available to download
#'
#' This function downloads the specified datasets
#' @keywords datasets, download, data
#' @export
#' @examples
#' ShowData()

DownloadDataF <-
  function(table, path=" ",
           AbName,
           FromYear = 1968,
           ToYear = 2071) {
    cuadro1 <-
      table %>% filter(Ab.Name == AbName,
                       Year >= FromYear,
                       YearEnd <= ToYear,
                       Time == Time)
    for (i in 1:length(cuadro1$link)) {
      curl_fetch_disk(
        paste0(
          "https://tds.ucar.edu/thredds/fileServer/datazone/narccap/DATA/CRCM/",
          cuadro1$M3[i],
          "/",
          cuadro1$Table[i],
          "/",
          cuadro1$link[i]
        ),
        paste0("~/", cuadro1$link[i])
      )
    }
  }
