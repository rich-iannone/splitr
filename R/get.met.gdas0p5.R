get.met.gdas0p5 <- function(files = NULL,
                            years = NULL,
                            months = NULL,
                            days = NULL
                            path_met_files = "~/Documents/SplitR/Met/"){
  
  # Download the 'listing' file from NOAA server
  # It contains a list of GDAS0p5 files currently available on the server
  download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/gdas0p5/",
                            "listing", sep = ''),
                destfile = paste(path_met_files, "listing", sep = ''),
                method = "auto",
                quiet = TRUE,
                mode = "w",
                cacheOK = TRUE)
  
  
  
}