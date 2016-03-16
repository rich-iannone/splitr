#' Get EDAS40 meteorology data files
#' @description This function downloads EDAS40
#' meteorology data files from the NOAA FTP server and
#' saves them to the working directory. Files can be
#' downloaded either by specifying a list of filenames
#' (in the form of "RP[YYYY][MM].gbl") or through bulk
#' download of months or years of files.
#' @param files a vector list of exact filenames for
#' the EDAS40 files.
#' @param years a vector list of years for which EDAS40
#' files are to be obtained via FTP.
#' @param months a vector list of months for which
#' EDAS40 files are to be obtained via FTP.
#' @export get_met_edas40
#' @examples
#' \dontrun{
#' # Download a year of EDAS40 data files
#' get_met_edas40(years = 2012)
#' }

get_met_edas40 <- function(files = NULL,
                           years = NULL,
                           months = NULL,
                           path_met_files){
  
  # Download the 'listing' file from NOAA server
  # It contains a list of EDAS40 files currently available on the server
  download.file(url = paste0("ftp://arlftp.arlhq.noaa.gov/archives/edas40/",
                            "listing"),
                destfile = paste0(path_met_files, "listing"),
                method = "auto",
                quiet = TRUE,
                mode = "w",
                cacheOK = TRUE)
}
