#' Get EDAS40 meteorology data files
#'
#' @param files 
#' @param years 
#' @param months 
#' @param path_met_files  
#' @export get.met.edas40
#' @examples
#' # Download a year of EDAS40 data files
#' get.met.edas40(years = 2012)

get.met.edas40 <- function(files = NULL,
                           years = NULL,
                           months = NULL,
                           path_met_files = "~/Documents/SplitR/Met/"){
  
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