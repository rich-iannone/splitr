#' Get EDAS40 meteorology data files
#'
#' This function downloads EDAS40 meteorology data files from the NOAA FTP
#' server and saves them to the working directory. Files can be downloaded
#' either by specifying a list of filenames (in the form of
#' `RP[YYYY][MM].gbl`) or through bulk download of months or years of
#' files.
#' @param files a vector list of exact filenames for the EDAS40 files.
#' @param years a vector list of years for which EDAS40 files are to be obtained
#'   via FTP.
#' @param months a vector list of months for which EDAS40 files are to be
#'   obtained via FTP.
#' @param path_met_files a full path for the download location of the
#'   meteorological data files.
#' @examples
#' \dontrun{
#' # Download a year of EDAS40 data files to the
#' # working directory
#' get_met_edas40(
#'   years = 2012,
#'   path_met_files = getwd())
#'
#' # Download EDAS40 data files for March, April,
#' # and May of 2015 to the working directory
#' get_met_edas40(
#'   years = 2015,
#'   months = c(3, 4, 5),
#'   path_met_files = getwd())
#' }
#' @import downloader
#' @export
get_met_edas40 <- function(files = NULL,
                           years = NULL,
                           months = NULL,
                           path_met_files) {
  
  edas40_dir <- "ftp://arlftp.arlhq.noaa.gov/archives/edas40/"
  
  # Download the 'listing' file from NOAA server
  # It contains a list of EDAS40 files currently
  # available on the server
  download(
    url = paste0(edas40_dir, "listing"),
    destfile = paste0(getwd(), "/listing"),
    method = "auto",
    quiet = TRUE,
    mode = "wb",
    cacheOK = TRUE
  )
  
  edas40_listing <- readLines(paste0(getwd(), "/listing"))
  
  edas40_listing <- gsub(" ", "", edas40_listing)
  
  if (!is.null(years)) {
    if (length(years) > 1) {
      years <- seq.default(years[1], years[2])
    }
    
    years <- substr(years, 3, 4)
    
    for (i in 1:length(years)) {
      
      if (i == 1) {
        edas40_file_list <-vector(mode = "character")
      }
      
      edas40_file_list <-
        c(
          edas40_file_list,
          edas40_listing[
            which(
              grepl(
                paste0("[a-z][a-z][a-z]", years[i]),
                edas40_listing)
            )]
        )
    }
  }
  
  if (!is.null(months)) {
    
    months_3_letter <- 
      c("jan", "feb", "mar", "apr",
        "may", "jun", "jul", "aug",
        "sep", "oct", "nov", "dec")
    
    for (i in 1:length(months)) {
      if (i == 1) {
        edas40_file_list_month <- vector(mode = "character", length = 0)
      }
      
      edas40_file_list_month <-
        c(
          edas40_file_list_month,
          edas40_file_list[
            which(
              grepl(
                paste0("edas.", months_3_letter[months[i]]),
                edas40_file_list
              )
            )]
        )
    }
    
    edas40_file_list <- edas40_file_list_month
  }
  
  if (!is.null(files)) {
    edas40_file_list <- files
  }
  
  for (i in 1:length(edas40_file_list)) {
    download(
      url = paste0(edas40_dir, edas40_file_list[i]),
      destfile = paste0(path_met_files, edas40_file_list[i]),
      method = "auto",
      quiet = FALSE,
      mode = "wb",
      cacheOK = FALSE
    )
  }
}
