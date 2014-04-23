get.met.forecast.nam <- function(path_met_files = "~/Documents/SplitR/Met/"){
  
  require("RCurl") 
  
  # Establish which forecast dirs are currently available on the server
  forecast_dirs <-
    grep("^[0-9].*$",
         unlist(strsplit(getURL("ftp://arlftp.arlhq.noaa.gov/forecast/",
                                dirlistonly = TRUE), "\n")), value = TRUE)
    
  
}
