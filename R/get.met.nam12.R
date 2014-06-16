#' Get NAM12 meteorology data files
#' @description This function downloads NAM12 meteorology data files from the NOAA FTP server and saves them to a specified folder. Files can be downloaded either by specifying a list of filenames (in the form of "RP[YYYY][MM].gbl") or through bulk download of a year of files.
#' @param files 
#' @param years 
#' @param months 
#' @param path_met_files 
#' @export get.met.nam12

get.met.nam12 <- function(files = NULL,
                          years = NULL,
                          months = NULL,
                          path_met_files = "~/Documents/SplitR/Met/"){
 
  # Add require statements
  require(RCurl) 
  require(lubridate)
  
}