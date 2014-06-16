#' Get reanalysis meteorology data files
#' @description This function downloads reanalysis meteorology data files from the NOAA FTP server and saves them to a specified folder. Files can be downloaded either by specifying a list of filenames (in the form of "RP[YYYY][MM].gbl") or through bulk download of a year of files.
#' @param files 
#' @param years 
#' @param path_met_files 
#' @export get.met.reanalysis

get.met.reanalysis <- function(files = NULL,
                               years = NULL,
                               path_met_files = "~/Documents/SplitR/Met/"){ 
  
  # Add require statements
  require(RCurl) 
  
  # Download list of reanalysis met files by name
  if (!is.null(files)){
    
    for (i in 1:length(files)){
      
      if (.Platform$OS.type == "windows"){
        download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/reanalysis/",
                                  files[i], sep = ''),
                      destfile = paste(path_met_files,
                                       "\\", files[i], sep = ''),
                      method = "auto",
                      quiet = FALSE,
                      mode = "wb",
                      cacheOK = FALSE)
      }
      
      if (.Platform$OS.type == "unix"){
        
        download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/reanalysis/",
                                  files[i], sep = ''),
                      destfile = paste(path_met_files,
                                       files[i], sep = ''),
                      method = "auto",
                      quiet = FALSE,
                      mode = "wb",
                      cacheOK = FALSE)
        
      }
      
    }
    
  }
  
  # Download one or more years of reanalysis met files
  if (!is.null(years)){
    
    for (i in 1:length(years)){
      
      for (j in 1:12){
        
        if (.Platform$OS.type == "unix"){
          
          download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/reanalysis/RP",
                                    years[i],
                                    formatC(j, width = 2, format = "d", flag = "0"),
                                    ".gbl",
                                    sep = ''),
                        destfile = paste(path_met_files,
                                         "RP",
                                         years[i],
                                         formatC(j, width = 2, format = "d", flag = "0"),
                                         ".gbl",
                                         sep = ''),
                        method = "auto",
                        quiet = FALSE,
                        mode = "wb",
                        cacheOK = FALSE)
          
        }
        
        if (.Platform$OS.type == "windows"){
          
          download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/reanalysis/RP",
                                    years[i],
                                    formatC(j, width = 2, format = "d", flag = "0"),
                                    ".gbl",
                                    sep = ''),
                        destfile = paste(path_met_files, "//",
                                         "RP",
                                         years[i],
                                         formatC(j, width = 2, format = "d", flag = "0"),
                                         ".gbl",
                                         sep = ''),
                        method = "auto",
                        quiet = FALSE,
                        mode = "wb",
                        cacheOK = FALSE)
          
        }
        
      }
  
    }
    
  }
  
}