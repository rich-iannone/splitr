get.met.gdas0p5 <- function(files = NULL,
                            years = NULL,
                            months = NULL,
                            days = NULL
                            path_met_files = "~/Documents/SplitR/Met/"){
  
  # Download the 'listing' file from NOAA server
  # It contains a list of GDAS0p5 files currently available on the server
  download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/gdas0p5/",
                            "listing", sep = ''),
                destfile = paste(path_met_files, "listing", sep = ''),
                method = "auto",
                quiet = TRUE,
                mode = "w",
                cacheOK = TRUE)
  
  # Create object 'listing' as a vector with all filenames available on the FTP server
  listing <- as.vector(read.table(paste(path_met_files, "listing", sep = ''),
                                  sep = "\n")[,1])
  
  # Download list of GDAS0p5 met files by name
  if (!is.null(files)){
    
    # Determine whether all of the requested files are available on the server
    files_available <- ifelse(all(files %in% listing), TRUE, FALSE)
    
    # If all requested files are available on the server, download those files
    if (files_available == TRUE){
      
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
  }
  
  # Download list of GDAS0p5 met files for one or more years
  if (!is.null(years)){
    
    # Create a list of filenames to compare with those currently available
    # on the NOAA FTP server
    for (i in 1:length(years)){
      if (i == 1) the_list <- vector(mode = "character", length = 0)
      
      jan <- seq(1, 31, 1)
      for (j in 1:length(jan)){
        the_list <- c(the_list, paste(years[i], "01",
                                      formatC(j, width = 2, format = "d", flag = "0"),
                                      "_gdas0p5", sep = ""))
      }
      
      if (years[i] %% 4 != 0) feb <- seq(1, 29, 1)
      if (years[i] %% 4 == 0) feb <- seq(1, 28, 1)
      for (j in 1:length(feb)){
        the_list <- c(the_list, paste(years[i], "02",
                                      formatC(j, width = 2, format = "d", flag = "0"),
                                      "_gdas0p5", sep = ""))
      }
      
      mar <- seq(1, 31, 1)
      for (j in 1:length(mar)){
        the_list <- c(the_list, paste(years[i], "03",
                                      formatC(j, width = 2, format = "d", flag = "0"),
                                      "_gdas0p5", sep = ""))
      }
      
      apr <- seq(1, 30, 1)
      for (j in 1:length(apr)){
        the_list <- c(the_list, paste(years[i], "04",
                                      formatC(j, width = 2, format = "d", flag = "0"),
                                      "_gdas0p5", sep = ""))
      }
      
      may <- seq(1, 31, 1)
      for (j in 1:length(may)){
        the_list <- c(the_list, paste(years[i], "05",
                                      formatC(j, width = 2, format = "d", flag = "0"),
                                      "_gdas0p5", sep = ""))
      }
      
      jun <- seq(1, 30, 1)
      for (j in 1:length(jun)){
        the_list <- c(the_list, paste(years[i], "06",
                                      formatC(j, width = 2, format = "d", flag = "0"),
                                      "_gdas0p5", sep = ""))
      }
      
      jul <- seq(1, 31, 1)
      for (j in 1:length(jul)){
        the_list <- c(the_list, paste(years[i], "07",
                                      formatC(j, width = 2, format = "d", flag = "0"),
                                      "_gdas0p5", sep = ""))
      }
      
      aug <- seq(1, 31, 1)
      for (j in 1:length(aug)){
        the_list <- c(the_list, paste(years[i], "08",
                                      formatC(j, width = 2, format = "d", flag = "0"),
                                      "_gdas0p5", sep = ""))
      }
      
      sep <- seq(1, 30, 1)
      for (j in 1:length(sep)){
        the_list <- c(the_list, paste(years[i], "09",
                                      formatC(j, width = 2, format = "d", flag = "0"),
                                      "_gdas0p5", sep = ""))
      }
      
      oct <- seq(1, 31, 1)
      for (j in 1:length(oct)){
        the_list <- c(the_list, paste(years[i], "10",
                                      formatC(j, width = 2, format = "d", flag = "0"),
                                      "_gdas0p5", sep = ""))
      }
      
      nov <- seq(1, 30, 1)
      for (j in 1:length(nov)){
        the_list <- c(the_list, paste(years[i], "11",
                                      formatC(j, width = 2, format = "d", flag = "0"),
                                      "_gdas0p5", sep = ""))
      }
      
      dec <- seq(1, 31, 1)
      for (j in 1:length(dec)){
        the_list <- c(the_list, paste(years[i], "12",
                                      formatC(j, width = 2, format = "d", flag = "0"),
                                      "_gdas0p5", sep = ""))
      }
    }
    
    # Determine whether all of the requested files are available on the server
    files_available <- ifelse(all(the_list %in% listing), TRUE, FALSE)
    
    # Determine which files are missing
    
    # Download the requested files from the server
    
    for (i in 1:length(the_list)){
      download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/gdas1/",
                                the_list[i], sep = ''),
                    destfile = paste(path_met_files, the_list[i], sep = ''),
                    method = "auto",
                    quiet = FALSE,
                    mode = "w",
                    cacheOK = TRUE)
      
    }
    
  }
  
  # End of function
