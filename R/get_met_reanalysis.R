#' Get filenames for GDAS1 meteorology files
#'
#' @noRd
get_reanalysis_filenames <- function(days,
                                     duration,
                                     direction) {
  
  get_monthly_filenames(
    days = days,
    duration = duration,
    direction = direction,
    prefix = "RP",
    extension = ".gbl"
  )
}

#' Get reanalysis meteorology data files
#'
#' Downloads reanalysis meteorology data files from the NOAA FTP server and
#' saves them to a specified folder. Files can be downloaded by specifying a
#' list of filenames (in the form of `'RP[YYYY][MM].gbl'`).
#' 
#' @inheritParams get_met_gdas1
#' 
#' @export
get_met_reanalysis <- function(files = NULL,
                               path_met_files) { 
  
  ftp_dir <- "ftp://arlftp.arlhq.noaa.gov/archives/reanalysis"
  
  files_in_path <- list.files(path_met_files)
  
  # Download list of reanalysis met files by name
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
