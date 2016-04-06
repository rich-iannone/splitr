#' Get GDAS1 meteorology data files
#' @description This function downloads GDAS1
#' meteorology data files from the NOAA FTP server and
#' saves them to a specified folder. Files can be
#' downloaded either by specifying a list of filenames
#' (in the form of "RP[YYYY][MM].gbl") or through bulk
#' download of a year of files.
#' @param files a vector list of exact filenames for
#' the GDAS1 files.
#' @param years a vector list of years for which GDAS1
#' files are to be obtained via FTP.
#' @param months a vector list of months for which 
#' GDAS1 files are to be obtained via FTP. 
#' @param path_met_files a full path for the download
#' location of the meteorological data files.
#' @import downloader
#' @export get_met_gdas1

get_met_gdas1 <- function(files = NULL,
                          years = NULL,
                          months = NULL,
                          path_met_files) {
  
  gdas1_dir <- 
    "ftp://arlftp.arlhq.noaa.gov/archives/gdas1/"
  
  # Download list of GDAS1 met files by name
  if (!is.null(files)) {
    
    for (i in 1:length(files)) {
      download(
        url = paste0(gdas1_dir,
                     files[i]),
        destfile = paste0(path_met_files,
                          files[i]),
        method = "auto",
        quiet = FALSE,
        mode = "wb",
        cacheOK = FALSE)
    }
  }
  
  if (!is.null(months)) {
    months_3_letter <- c("jan", "feb", "mar", "apr", "may", "jun",
                         "jul", "aug", "sep", "oct", "nov", "dec")
    
    for (i in 1:length(months)) {
      if (i == 1) {
        the_files <-
          vector(mode = "character", length = 0)
      }
      
      the_month <-
        months_3_letter[as.numeric(gsub("^(..).*",
                                        "\\1",
                                        months[i]))]
      the_year <- gsub("^..-(.*)$", "\\1", months[i])
      the_files_1 <-
        paste0("gdas1.", the_month,
               gsub("^..", "", the_year), ".w1")
      the_files_2 <-
        paste0("gdas1.", the_month,
               gsub("^..", "", the_year), ".w2")
      the_files_3 <-
        paste0("gdas1.", the_month,
               gsub("^..", "", the_year), ".w3")
      the_files_4 <-
        paste0("gdas1.", the_month,
               gsub("^..", "", the_year), ".w4")
      the_files_5 <-
        paste0("gdas1.", the_month,
               gsub("^..", "", the_year), ".w5")
      if (the_month == "feb" &
          as.numeric(the_year) %% 4 != 0) {
        the_files <-
          c(the_files, the_files_1, the_files_2,
            the_files_3, the_files_4) 
      } else {
        the_files <-
          c(the_files, the_files_1, the_files_2,
            the_files_3, the_files_4, the_files_5)
      }
    }
    
    for (i in 1:length(the_files)) {
      download(
        url = paste0(gdas1_dir,
                     files[i]),
        destfile = paste0(path_met_files,
                          the_files[i]),
        method = "auto",
        quiet = FALSE,
        mode = "wb",
        cacheOK = FALSE)
    }
  }
  
  if (!is.null(years)) {
    
    months_3_letter <- 
      c("jan", "feb", "mar", "apr",
        "may", "jun", "jul", "aug",
        "sep", "oct", "nov", "dec")
    
    for (i in 1:length(years)) {
      if (i == 1) {
        months <-
          vector(mode = "character", length = 0)
      }
      months_from_year <- 
        c(paste0("01-", years[i]),
          paste0("02-", years[i]),
          paste0("03-", years[i]),
          paste0("04-", years[i]),
          paste0("05-", years[i]),
          paste0("06-", years[i]),
          paste0("07-", years[i]),
          paste0("08-", years[i]),
          paste0("09-", years[i]),
          paste0("10-", years[i]),
          paste0("11-", years[i]),
          paste0("12-", years[i]))
      months <- c(months, months_from_year)
    }
    
    for (i in 1:length(months)) {
      if (i == 1) {
        the_files <-
          vector(mode = "character", length = 0)
      }
      the_month <-
        months_3_letter[as.numeric(gsub("^(..).*",
                                        "\\1",
                                        months[i]))]
      the_year <- gsub("^..-(.*)$", "\\1", months[i])
      the_files_1 <-
        paste0("gdas1.", the_month,
               gsub("^..", "", the_year), ".w1")
      the_files_2 <-
        paste0("gdas1.", the_month,
               gsub("^..", "", the_year), ".w2")
      the_files_3 <-
        paste0("gdas1.", the_month,
               gsub("^..", "", the_year), ".w3")
      the_files_4 <-
        paste0("gdas1.", the_month,
               gsub("^..", "", the_year), ".w4")
      the_files_5 <-
        paste0("gdas1.", the_month,
               gsub("^..", "", the_year), ".w5")
      if (the_month == "feb" &
          as.numeric(the_year) %% 4 != 0) {
        the_files <- 
          c(the_files, the_files_1, the_files_2,
            the_files_3, the_files_4) 
      } else {
        the_files <- 
          c(the_files, the_files_1, the_files_2,
            the_files_3, the_files_4, the_files_5)
      }
    }
    
    for (i in 1:length(the_files)) {
      download(
        url = paste0(gdas1_dir,
                     the_files[i]),
        destfile = paste0(path_met_files,
                          the_files[i]),
        method = "auto",
        quiet = FALSE,
        mode = "wb",
        cacheOK = FALSE)
    }
  }
}
