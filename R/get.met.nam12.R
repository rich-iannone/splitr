#' Get NAM12 meteorology data files
#'
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