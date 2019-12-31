#' Get ERA5 meteorology data files
#'
#' Generates list of file names for ERA5 dataset. Cannot be 
#' downloaded from arlftp since it is not archived there, must
#' be downloaded/preprocessed from ECMWF via conversion tool 
#' provided w/ HYSPLIT distro. Comment out download portion,
#' assume data are present in path_met_files. -RPF 191230.
#' 
#' @inheritParams get_met_gdas1
#' 
#' @export
get_met_era5 <- function(days,
                            duration,
                            direction,
                            path_met_files) {
  
  get_daily_filenames(
    days = days,
    duration = duration,
    direction = direction,
    suffix = ".ARL",
    prefix = "ERA5_"
  ) #%>%
    #get_met_files(
    #  path_met_files = path_met_files,
    #  ftp_dir = "ftp://arlftp.arlhq.noaa.gov/archives/gdas0p5"
    #)
}
