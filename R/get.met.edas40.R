#' Get EDAS40 meteorology data files
#' @description This function downloads EDAS40 meteorology data files from the NOAA FTP server and saves them to a specified folder. Files can be downloaded either by specifying a list of filenames (in the form of "RP[YYYY][MM].gbl") or through bulk download of a year of files.
#' @param files a vector list of exact filenames for the EDAS40 files.
#' @param years a vector list of years for which EDAS40 files are to be obtained via FTP.
#' @param months 
#' @param path_met_files  
#' @export get.met.edas40
#' @examples
#' # Download a year of EDAS40 data files
#' get.met.edas40(years = 2012)

get.met.edas40 <- function(files = NULL,
                           years = NULL,
                           months = NULL,
                           path_met_files){
  
  # Add require statements
  require(lubridate)
  
  # Download the 'listing' file from NOAA server
  # It contains a list of EDAS40 files currently available on the server
  download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/edas40/",
                            "listing", sep = ''),
                destfile = paste(path_met_files, "listing", sep = ''),
                method = "auto",
                quiet = TRUE,
                mode = "w",
                cacheOK = TRUE)
  
  
}