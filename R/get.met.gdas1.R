get.met.gdas1 <- function(files = NULL,
                          years = NULL,
                          months = NULL,
                          path_met_files = "~/Documents/SplitR/Met/"){
  
  # Download list of GDAS1 met files by name
  if (!is.null(files)){
    
    for (i in 1:length(files)){
      download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/gdas1/",
                                files[i], sep = ''),
                    destfile = paste(path_met_files, files[i], sep = ''),
                    method = "auto",
                    quiet = FALSE,
                    mode = "w",
                    cacheOK = TRUE)
    }
    
  }
  
  if (!is.null(months)){
    
    months_3_letter <- c("jan", "feb", "mar", "apr", "may", "jun",
                         "jul", "aug", "sep", "oct", "nov", "dec")
    
    for (i in 1:length(months)){
      if (i == 1) the_files <- vector(mode = "character", length = 0)
      the_month <- months_3_letter[as.numeric(gsub("^(..).*", "\\1", months[i]))]
      the_year <- gsub("^..-(.*)$", "\\1", months[i])
      the_files_1 <- paste("gdas1.", the_month, gsub("^..", "", the_year), ".w1", sep = '')
      the_files_2 <- paste("gdas1.", the_month, gsub("^..", "", the_year), ".w2", sep = '')
      the_files_3 <- paste("gdas1.", the_month, gsub("^..", "", the_year), ".w3", sep = '')
      the_files_4 <- paste("gdas1.", the_month, gsub("^..", "", the_year), ".w4", sep = '')
      the_files_5 <- paste("gdas1.", the_month, gsub("^..", "", the_year), ".w5", sep = '')
      if (the_month == "feb" & as.numeric(the_year) %% 4 != 0) {
        the_files <- c(the_files, the_files_1, the_files_2, the_files_3, the_files_4) 
      } else {
        the_files <- c(the_files, the_files_1, the_files_2,
                       the_files_3, the_files_4, the_files_5)
      }
    }
    
    for (i in 1:length(the_files)){
      download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/gdas1/",
                                files[i], sep = ''),
                    destfile = paste(path_met_files, the_files[i], sep = ''),
                    method = "auto",
                    quiet = FALSE,
                    mode = "w",
                    cacheOK = TRUE)
    }
    
  }
  

  
  # Download one or more years of GDAS1 met files
#   if (!is.null(years)){
#       
#     months_3_letter <- c("jan", "feb", "mar", "apr", "may", "jun",
#                            "jul", "aug", "sep", "oct", "nov", "dec")
#     for (i in 1:length(years)){
#       
#       # Construct list of files for the year
#       files_for_year <- vector(mode = "character",
#                                length = ifelse(years[i] %% 4 == 0, 60, 59))
# 
#       
#       for (j in 1:length(files_for_year)){
#         for (k in 1:length(months_3_letter)){
#           if (years[i] %% 4 != 0 & months_3_letter[k] == "feb"){
#             weeks_in_month <- seq(from = 1, to = 4, by = 1)
#           } else {
#             weeks_in_month <- seq(from = 1, to = 5, by = 1)
#           }
#           for (l in weeks_in_month){
#             files_for_year[j] <-
#             paste("gdas1.", months_3_letter[k], gsub("^..", "", years[i]), ".w",
#                   weeks_in_month[l], sep = '')
#           }
#           
#         }
#       }
#     }
#       
#         
#       for (k in 1:length(files_for_year)){
#         download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/gdas1/",
#                                   files_for_year[k], sep = ''),
#                       destfile = paste(path_met_files, files_for_year[k], sep = ''),
#                       method = "auto",
#                       quiet = FALSE,
#                       mode = "w",
#                       cacheOK = TRUE)
#       }
#       
#     }
#     
#   }


}
