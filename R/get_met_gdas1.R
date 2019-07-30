#' Get filenames for GDAS1 meteorology files
#'
#' @noRd
get_gdas1_filenames <- function(days,
                                duration,
                                direction) {
  
  # Determine the minimum month (as a `Date`) for the model run
  if (direction == "backward") {
    min_month <- 
      (lubridate::as_date(days[1]) - (duration / 24)) %>%
      lubridate::floor_date(unit = "month")
  } else if (direction == "forward") {
    min_month <- 
      (lubridate::as_date(days[1]) + (duration / 24)) %>%
      lubridate::floor_date(unit = "month")
  }
  
  # Determine the maximum month (as a `Date`) for the model run
  if (direction == "backward") {
    max_month <- 
      (lubridate::as_date(days[length(days)]) - (duration / 24)) %>%
      lubridate::floor_date(unit = "month")
  } else if (direction == "forward") {
    max_month <- 
      (lubridate::as_date(days[length(days)]) + (duration / 24)) %>%
      lubridate::floor_date(unit = "month")
  }
  
  met_months <- 
    seq(min_month, max_month, by = "1 month") %>%
    rep(each = 5)
  
  month_names <-
    met_months %>%
    lubridate::month(label = TRUE, abbr = TRUE) %>%
    as.character() %>%
    tolower()
  
  met_years <- 
    met_months %>%
    substr(3, 4)
  
  if (!all(lubridate::leap_year(lubridate::year(days)))) {
    
    not_leap_years_lgl <- !lubridate::leap_year(lubridate::ymd(days))
    
    exclusion_years_feb_5 <- 
      lubridate::year(days[not_leap_years_lgl]) %>%
      unique()
    
    excluded_files <- 
      paste0("gdas1.feb", substr(exclusion_years_feb_5, 3, 4), ".w5")
    
  } else {
    excluded_files <- character(0)
  }
  
  files <- paste0("gdas1.", month_names, met_years, ".w", 1:5)
  
  files %>% base::setdiff(excluded_files)
}


#' Get GDAS1 meteorology data files
#'
#' This function downloads GDAS1 meteorology data files from the NOAA FTP server
#' and saves them to a specified folder. Files can be downloaded by specifying a
#' list of filenames.
#' @param files A vector list of exact filenames for the meteorological model
#'   files.
#' @param path_met_files A full path should be provided for the location of the
#'   meteorological data files. Downloaded files will be saved in this location.
#'   
#' @export
get_met_gdas1 <- function(files = NULL,
                          path_met_files) {
  
  ftp_dir <- "ftp://arlftp.arlhq.noaa.gov/archives/gdas1.v1"
  
  files_in_path <- list.files()
  
  # Download list of GDAS1 met files by name
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
