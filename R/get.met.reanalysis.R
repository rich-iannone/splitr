get.met.reanalysis <- function(files = NULL,
                               years = NULL,
                               path_met_files = "~/Documents/SplitR/Met/"){ 
  
  
  # Download list of files by name
  if (!is.null(files)){
    
    for (i in 1:length(files)){
      download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/reanalysis/",
                                files[i], sep = ''),
                    destfile = paste(path_met_files, files[i], sep = ''),
                    method = "auto",
                    quiet = FALSE,
                    mode = "w",
                    cacheOK = TRUE)
    }
    
  }
  
}