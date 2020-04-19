#' Get GDAS1 meteorology data files
#'
#' Downloads GDAS1 meteorology data files from the NOAA FTP server and saves
#' them to a specified folder. Files can be downloaded by specifying a list of
#' filenames (in the form of `"gdas1.{month-abbrev}{year-2}.w{wk-num}"`).
#'
#' @inheritParams hysplit_trajectory
#' @param path_met_files A full directory path to which the meteorological data
#'   files will be saved.
#'   
#' @export
get_met_gdas1 <- function(days,
                          duration,
                          direction,
                          path_met_files) {
  
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
    lubridate::month(label = TRUE, abbr = TRUE, locale = "en_US.UTF-8") %>%
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
  
  files <- files %>% base::setdiff(excluded_files)
  
  get_met_files(
    files = files,
    path_met_files = path_met_files,
    ftp_dir = "ftp://arlftp.arlhq.noaa.gov/archives/gdas1"
  )
}
