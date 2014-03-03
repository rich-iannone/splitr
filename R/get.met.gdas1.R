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
