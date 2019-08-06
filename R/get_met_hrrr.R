#' Get HRRR meteorology data files
#'
#' This function downloads High Resolution Rapid Refresh (HRRR) meteorology data
#' files from the NOAA FTP server and saves them to a specified folder. Files
#' can be downloaded only by specifying a list of filenames (in the form of
#' `'RP[YYYY][MM].gbl'`).
#' 
#' @param files a vector list of exact filenames for the HRRR files.
#' @param path_met_files a full path should be provided for the location of the
#'   meteorological data files; downloaded files will be saved in this location.
#' @export
get_met_hrrr <- function(files = NULL,
                         path_met_files) {
                           
  ftp_dir <- "ftp://arlftp.arlhq.noaa.gov/pub/archives/hrrr/"
  
  # Download list of reanalysis met files by name
  if (!is.null(files)) {
    
    for (i in 1:length(files)) {
        download(
          url = file.path(ftp_dir, files[i]),
          destfile = file.path(path_met_files, files[i]),
          method = "auto",
          quiet = FALSE,
          mode = "wb",
          cacheOK = FALSE) 
    }
  }
}
