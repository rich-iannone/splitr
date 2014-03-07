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
  
  # Create object 'listing' as a vector with all filenames available on the FTP server
  listing <- as.vector(read.table(paste(path_met_files, "listing", sep = ''),
                                  sep = "\n")[,1])
  
  
  
}