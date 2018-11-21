#' Get reanalysis meteorology data files
#' @description This function downloads reanalysis
#' meteorology data files from the NOAA FTP server and
#' saves them to a specified folder. Files can be
#' downloaded either by specifying a list of filenames
#' (in the form of \code{'RP[YYYY][MM].gbl'}) or
#' through bulk download of a year of files.
#' @param files a vector list of exact filenames for
#' the reanalysis files.
#' @param years a vector list of years for which
#' reanalysis files are to be obtained via FTP.
#' @param path_met_files a full path should be provided
#' for the location of the meteorological data files;
#' downloaded files will be saved in this location.
#' @import downloader
#' @export get_met_reanalysis

get_met_reanalysis <- function(files = NULL,
                               years = NULL,
                               path_met_files) { 
  
  reanalysis_dir <- 
    "ftp://arlftp.arlhq.noaa.gov/archives/reanalysis/"
  
  # Download list of reanalysis met files by name
  if (!is.null(files)) {
    
    for (i in 1:length(files)) {
        download(
          url = file.path(reanalysis_dir,
                       files[i]),
          destfile = file.path(path_met_files,
                            files[i]),
          method = "auto",
          quiet = FALSE,
          mode = "wb",
          cacheOK = FALSE) 
    }
  }
  
  # Download one or more years of reanalysis met files
  if (!is.null(years)) {
    for (i in 1:length(years)) {
      for (j in 1:12) {
          download(
            url = file.path(reanalysis_dir, 
              paste0(
                         "RP",
                         years[i],
                         formatC(j, width = 2,
                                 format = "d",
                                 flag = "0"),
                         ".gbl")),
            destfile = file.path(path_met_files, 
              paste0(
                              "RP",
                              years[i],
                              formatC(j, width = 2,
                                      format = "d",
                                      flag = "0"),
                              ".gbl")),
            method = "auto",
            quiet = FALSE,
            mode = "wb",
            cacheOK = FALSE)
      }
    }
  }
}
