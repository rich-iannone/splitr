get.met.edas40 <- function(files = NULL,
                           years = NULL,
                           months = NULL,
                           path_met_files = "~/Documents/SplitR/Met/"){
  
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