#' Get GDAS0.5 meteorology data files
#'
#' This function downloads GDAS0.5 meteorology data files from the NOAA FTP
#' server and saves them to a specified folder. Files can be downloaded either
#' by specifying a list of filenames (in the form of "RP[YYYY][MM].gbl") or
#' through bulk download of a year of files.
#' @param files a vector list of exact filenames for the GDAS0.5 files.
#' @param years a vector list of years for which GDAS0.5 files are to be
#'   obtained via FTP.
#' @param months a vector list of months for which GDAS0.5 files are to be
#'   obtained via FTP.
#' @param days a vector list of days for which GDAS0.5 files are to be obtained
#'   via FTP.
#' @param path_met_files a full path should be provided for the location of the
#'   meteorological data files; downloaded files will be saved in this location.
#' @import downloader
#' @export
get_met_gdas0p5 <- function(files = NULL,
                            years = NULL,
                            months = NULL,
                            days = NULL,
                            path_met_files) {
  
  gdas0p5_dir <- "ftp://arlftp.arlhq.noaa.gov/archives/gdas0p5"
  
  url_of_listing <- file.path(gdas0p5_dir, "listing")
  
  # Download the 'listing' file from NOAA server
  # It contains a list of GDAS0p5 files currently
  # available on the server
  met_download(
    urls = url_of_listing,
    local_path = file.path(path_met_files, "listing")
  )
  
  # Create object 'listing' as a vector with all
  # filenames available on the FTP server
  listing <- read_listing_file(url_of_listing)
  
  # Download list of GDAS0p5 met files by name
  if (!is.null(files)) {
    
    # If not all files available on the server,
    # provide a warning
    if (!all(files %in% listing)) {
      
      warning("Not all files requested are available on the server:\n",
              " * ", paste(base::setdiff(files, listing), collapse = ", "),
              " not available.", call. = FALSE)
    }
    
    # Get a vector of the available files
    files_available <- base::intersect(listing, files)
    
    # Download the meteorology data files
    met_download(
      urls = file.path(gdas0p5_dir, files_available),
      local_path = file.path(path_met_files, files_available)
    )
  }
  
  # Download list of GDAS0p5 met files for one or more years
  if (!is.null(years)) {
    
    listing_years <- 
      listing %>%
      substring(1, 4)
    
    if (is.numeric(years)) {
      years <- as.character(years)
    }
    
    # Get a vector of the available files
    files_available <- listing[listing_years %in% years]
    
    # Download the meteorology data files
    met_download(
      urls = file.path(gdas0p5_dir, files_available),
      local_path = file.path(path_met_files, files_available)
    )
  }
  
  # Download list of GDAS0p5 met files for one or more months
  if (!is.null(months)) {
    
    listing_months <- 
      listing %>%
      substring(5, 6)
    
    if (is.numeric(months)) {
      
      months <- 
        formatC(
          x = months,
          width = 2,
          format = "d",
          flag = "0"
        )
    }
    
    # Get a vector of the available files
    files_available <- listing[listing_months %in% months]
    
    # Download the meteorology data files
    met_download(
      urls = file.path(gdas0p5_dir, files_available),
      local_path = file.path(path_met_files, files_available)
    )
  }
  
  # Download list of GDAS0p5 met files for one or days
  if (!is.null(days)) {
  
    listing_days <- 
      listing %>%
      substring(7, 8)
    
    if (is.numeric(days)) {
      
      days <- 
        formatC(
          x = days,
          width = 2,
          format = "d",
          flag = "0"
        )
    }
    
    # Get a vector of the available files
    files_available <- listing[listing_days %in% days]
    
    # Download the meteorology data files
    met_download(
      urls = file.path(gdas0p5_dir, files_available),
      local_path = file.path(path_met_files, files_available)
    )  
  }
  
}
