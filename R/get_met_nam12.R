#' Get NAM12 meteorology data files
#' @description This function downloads NAM12
#' meteorology data files from the NOAA FTP server and
#' saves them to a specified folder. Files can be
#' downloaded either by specifying a list of filenames
#' (in the form of \code{'RP[YYYY][MM].gbl'}) or
#' through bulk download of a year of files.
#' @param files a vector list of exact filenames for
#' the NAM12 files.
#' @param years a vector list of years for which NAM12
#' files are to be obtained via FTP.
#' @param months a vector list of months for which
#' NAM12 files are to be obtained via FTP.
#' @param path_met_files a full path should be provided
#' for the location of the meteorological data files;
#' downloaded files will be saved in this location.
#' @export get_met_nam12

get_met_nam12 <- function(files = NULL,
                          years = NULL,
                          months = NULL,
                          path_met_files) {
 
}