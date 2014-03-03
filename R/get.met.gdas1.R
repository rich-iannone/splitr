get.met.gdas1 <- function(files = NULL,
                          years = NULL,
                          months = NULL,
                          path_met_files = "~/Documents/SplitR/Met/"){
  
  # Download list of GDAS1 met files by name
  if (!is.null(files)){
    
    for (i in 1:length(files)){
      download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/gdas1/",
                                files[i], sep = ''),
                    destfile = paste(path_met_files, files[i], sep = ''),
                    method = "auto",
                    quiet = FALSE,
                    mode = "w",
                    cacheOK = TRUE)
    }
    
  }
  
  
}