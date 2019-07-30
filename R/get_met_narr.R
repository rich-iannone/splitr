#' Get filenames for the NARR meteorology files
#'
#' @noRd
get_narr_filenames <- function(days,
                               duration,
                               direction) {
  
  get_monthly_filenames(
    days = days,
    duration = duration,
    prefix = "NARR",
    extension = NULL
  )
}

#' Get NARR meteorology data files
#'
#' This function downloads NARR meteorology data files from the NOAA FTP server
#' and saves them to a specified folder. Files can be downloaded by specifying a
#' list of filenames (in the form of `'NARR[YYYY][MM]'`).
#' @inheritParams get_met_gdas1
#' 
#' @export
get_met_narr <- function(files = NULL,
                         path_met_files) { 
  
  ftp_dir <- "ftp://arlftp.arlhq.noaa.gov/archives/narr"
  
  files_in_path <- list.files()
  
  # Download list of NARR met files by name
  if (!is.null(files)) {
    
    for (file in files) {
      
      if (!(file %in% files_in_path)) {
        
        downloader::download(
          url = file.path(ftp_dir, file),
          destfile = path.expand(file.path(path_met_files, file)),
          method = "auto",
          quiet = FALSE,
          mode = "wb",
          cacheOK = FALSE
        ) 
      }
    }
  }
}
