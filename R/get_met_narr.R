#' Get NARR meteorology data files
#' @description This function downloads NARR
#' meteorology data files from the NOAA FTP server and
#' saves them to a specified folder. Files can be
#' downloaded either by specifying a list of filenames
#' (in the form of \code{'NARR[YYYY][MM]'}) or
#' through bulk download of a year of files.
#' @param files a vector list of exact filenames for
#' the NARR files.
#' @param years a vector list of years for which
#' NARR files are to be obtained via FTP.
#' @param path_met_files a full path should be provided
#' for the location of the meteorological data files;
#' downloaded files will be saved in this location.
#' @import downloader
#' @export get_met_narr

get_met_narr <- function(files = NULL,
                         years = NULL,
                         path_met_files) { 
  
  narr_dir <- 
    "ftp://arlftp.arlhq.noaa.gov/narr/"
  
  # Download list of NARR met files by name
  if (!is.null(files)) {
    
    for (i in 1:length(files)) {
      
      if (.Platform$OS.type == "windows") {
        download(
          url = paste0(narr_dir,
                       files[i]),
          destfile = paste0(path_met_files,
                            "\\", files[i]),
          method = "auto",
          quiet = FALSE,
          mode = "wb",
          cacheOK = FALSE)
      }
      
      if (.Platform$OS.type == "unix") {
        download(
          url = paste0(narr_dir,
                       files[i]),
          destfile = paste0(path_met_files,
                            files[i]),
          method = "auto",
          quiet = FALSE,
          mode = "wb",
          cacheOK = FALSE) 
      } 
    }
  }
  
  # Download one or more years of NARR met files
  if (!is.null(years)) {
    for (i in 1:length(years)) {
      for (j in 1:12) {
        if (.Platform$OS.type == "unix") {
          download(
            url = paste0(narr_dir,
                         "NARR",
                         years[i],
                         formatC(j, width = 2,
                                 format = "d",
                                 flag = "0")),
            destfile = paste0(path_met_files,
                              "NARR",
                              years[i],
                              formatC(j, width = 2,
                                      format = "d",
                                      flag = "0")),
            method = "auto",
            quiet = FALSE,
            mode = "wb",
            cacheOK = FALSE)
        }
        
        if (.Platform$OS.type == "windows") {
          download(
            url = paste0(narr_dir,
                         "NARR",
                         years[i],
                         formatC(j, width = 2,
                                 format = "d",
                                 flag = "0")),
            destfile = paste0(path_met_files, "//",
                              "NARR",
                              years[i],
                              formatC(j, width = 2,
                                      format = "d",
                                      flag = "0")),
            method = "auto",
            quiet = FALSE,
            mode = "wb",
            cacheOK = FALSE)
        }
      }
    }
  }
}
