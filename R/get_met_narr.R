#' Get NARR meteorology data files
#'
#' This function downloads NARR meteorology data files from the NOAA FTP server
#' and saves them to a specified folder. Files can be downloaded by specifying a
#' list of filenames (in the form of `"NARR{YYYY}{MM}"`).
#' @inheritParams get_met_gdas1
#' 
#' @export
get_met_narr <- function(days,
                         duration,
                         direction,
                         path_met_files) {
  
  get_monthly_filenames(
    days = days,
    duration = duration,
    direction = direction,
    prefix = "NARR"
  ) %>%
    get_met_files(
      path_met_files = path_met_files,
      ftp_dir = "ftp://arlftp.arlhq.noaa.gov/archives/narr"
    )
}
