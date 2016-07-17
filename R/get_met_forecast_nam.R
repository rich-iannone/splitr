#' Read NAM meteorological forecast data files
#' @description This function downloads NAM meteorology
#' forecast data files from the NOAA FTP server and
#' saves them to a specified folder.
#' @param path_met_files a full path should be provided
#' for the location of the meteorological data files;
#' downloaded files will be saved in this location.
#' @import downloader RCurl
#' @export get_met_forecast_nam

get_met_forecast_nam <- function(path_met_files) {
  
  # Establish which forecast dirs are currently 
  # available on the server
  forecast_dirs <-
    grep("^[0-9].*$",
         unlist(
           strsplit(
             RCurl::getURL(
               "ftp://arlftp.arlhq.noaa.gov/forecast/",
               dirlistonly = TRUE), "\n")), 
         value = TRUE)
  
  # Get today's date and write in format equivalent
  # to FTP directories 
  today <- gsub("-", "", Sys.Date())
  
  # Download today's `namf`` file
  # -- CONUS, 12 km, 3 hrly, pressure levels,
  #    48 h forecast
  
  if (today %in% forecast_dirs) {
    download(
      url = paste0(
        "ftp://arlftp.arlhq.noaa.gov/forecast/",
        today, "/hysplit.t00z.namf"),
      destfile = paste0(path_met_files, 
                        paste0(today, ".t00z.namf")),
      method = "auto",
      quiet = FALSE,
      mode = "w",
      cacheOK = TRUE)
  } 
}
