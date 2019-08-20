#' Get NAM12 meteorology data files
#'
#' Downloads NAM12 meteorology data files from the NOAA FTP server and saves
#' them to a specified folder. Files can be downloaded by specifying a list of
#' filenames (in the form of `"{YYYY}{MM}{DD}_nam12"`).
#' 
#' @inheritParams get_met_gdas1
#' 
#' @export
get_met_nam12 <- function(days,
                          duration,
                          direction,
                          path_met_files) {
  
  get_daily_filenames(
    days = days,
    duration = duration,
    direction = direction,
    suffix = "_nam12"
  ) %>%
    get_met_files(
      path_met_files = path_met_files,
      ftp_dir = "ftp://arlftp.arlhq.noaa.gov/archives/nam12"
    )
}
