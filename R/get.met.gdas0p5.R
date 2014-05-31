get.met.gdas0p5 <- function(files = NULL,
                            years = NULL,
                            months = NULL,
                            days = NULL,
                            path_met_files = "~/Documents/SplitR/Met/"){
  
  require(lubridate)
  
  # Download the 'listing' file from NOAA server
  # It contains a list of GDAS0p5 files currently available on the server
  download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/gdas0p5/",
                            "listing", sep = ''),
                destfile = paste(path_met_files, "listing", sep = ''),
                method = "auto",
                quiet = TRUE,
                mode = "wb",
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
                      mode = "wb",
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
    all_files_available <- ifelse(all(the_list %in% listing), TRUE, FALSE)
    
    # Determine which files are missing
    if (all_files_available == FALSE){
      for (i in 1:length(the_list)){
        if (i == 1) files_missing <- vector(mode = "character", length = 0)
        if (the_list[i] %in% listing){
          NULL
        } else {
          files_missing <- c(files_missing, the_list[i])
        }
      }
      
      # Generate report of missing files
      missing_report <- as.data.frame(mat.or.vec(nr = length(files_missing), nc = 2))
      colnames(missing_report) <- c("Filename", "Date")
      
      # Add names of missing files to the missing file report
      missing_report$Filename <- files_missing
      
      # Get dates for missing files
      for (i in 1:length(files_missing)){
        if (i == 1) dates <- mat.or.vec(nr = length(files_missing), nc = 1)
        dates[i] <- as.character(ymd(gsub("^([0-9]*).*", "\\1", files_missing[i])))
      }
      
      # Add dates of missing files to the missing file report
      missing_report$Date <- dates
      
      # Filter list of files to obtain
      the_list <- setdiff(the_list, files_missing)
      
    }
    
    # Download the requested files from the server
    for (i in 1:length(the_list)){
      download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/gdas1/",
                                the_list[i], sep = ''),
                    destfile = paste(path_met_files, the_list[i], sep = ''),
                    method = "auto",
                    quiet = FALSE,
                    mode = "wb",
                    cacheOK = TRUE)
      
    }
    
    # Write file that contains report of missing files
    if (exists("missing_report")){
      write.table(missing_report,
                  file = paste(path_met_files,
                               "Missing GDAS 0.5 Files - ", 
                               format(Sys.time(), "%B %d %Y %H_%M_%S"),
                               sep = ''),
                  quote = FALSE, row.names = FALSE)
    }
    
  }
  
  # Download list of GDAS0p5 met files for one or more months
  if (!is.null(months)){
    
    # Create a list of filenames to compare with those currently available
    # on the NOAA FTP server
    for (i in 1:length(months)){
      
      if (i == 1) the_list <- vector(mode = "character", length = 0)
      
      the_month <- as.numeric(gsub("^([0-9]*).*", "\\1", months[i]))
      the_year <- as.numeric(gsub("^[0-9][0-9]-([0-9]*)", "\\1", months[i]))
      
      if(the_month == 1){
        jan <- seq(1, 31, 1)
        for (j in 1:length(jan)){
          the_list <- c(the_list, paste(the_year, "01",
                                        formatC(j, width = 2, format = "d", flag = "0"),
                                        "_gdas0p5", sep = ""))
        }
      }
      
      if(the_month == 2){
        if (the_year %% 4 != 0) feb <- seq(1, 29, 1)
        if (the_year %% 4 == 0) feb <- seq(1, 28, 1)
        for (j in 1:length(feb)){
          the_list <- c(the_list, paste(the_year, "02",
                                        formatC(j, width = 2, format = "d", flag = "0"),
                                        "_gdas0p5", sep = ""))
        }
      }
      
      if(the_month == 3){
        mar <- seq(1, 31, 1)
        for (j in 1:length(mar)){
          the_list <- c(the_list, paste(the_year, "03",
                                        formatC(j, width = 2, format = "d", flag = "0"),
                                        "_gdas0p5", sep = ""))
        }
      }
      
      if(the_month == 4){
        apr <- seq(1, 30, 1)
        for (j in 1:length(apr)){
          the_list <- c(the_list, paste(the_year, "04",
                                        formatC(j, width = 2, format = "d", flag = "0"),
                                        "_gdas0p5", sep = ""))
        }
      }
      
      if(the_month == 5){
        may <- seq(1, 31, 1)
        for (j in 1:length(may)){
          the_list <- c(the_list, paste(the_year, "05",
                                        formatC(j, width = 2, format = "d", flag = "0"),
                                        "_gdas0p5", sep = ""))
        }
      }
      
      if(the_month == 6){
        jun <- seq(1, 30, 1)
        for (j in 1:length(jun)){
          the_list <- c(the_list, paste(the_year, "06",
                                        formatC(j, width = 2, format = "d", flag = "0"),
                                        "_gdas0p5", sep = ""))
        }
      }
      
      if(the_month == 7){
        jul <- seq(1, 31, 1)
        for (j in 1:length(jul)){
          the_list <- c(the_list, paste(the_year, "07",
                                        formatC(j, width = 2, format = "d", flag = "0"),
                                        "_gdas0p5", sep = ""))
        }
      }
      
      if(the_month == 8){
        aug <- seq(1, 31, 1)
        for (j in 1:length(aug)){
          the_list <- c(the_list, paste(the_year, "08",
                                        formatC(j, width = 2, format = "d", flag = "0"),
                                        "_gdas0p5", sep = ""))
        }
      }
      
      if(the_month == 9){
        sep <- seq(1, 30, 1)
        for (j in 1:length(sep)){
          the_list <- c(the_list, paste(the_year, "09",
                                        formatC(j, width = 2, format = "d", flag = "0"),
                                        "_gdas0p5", sep = ""))
        }
      }
      
      if(the_month == 10){
        oct <- seq(1, 31, 1)
        for (j in 1:length(oct)){
          the_list <- c(the_list, paste(the_year, "10",
                                        formatC(j, width = 2, format = "d", flag = "0"),
                                        "_gdas0p5", sep = ""))
        }
      }
      
      if(the_month == 11){
        nov <- seq(1, 30, 1)
        for (j in 1:length(nov)){
          the_list <- c(the_list, paste(the_year, "11",
                                        formatC(j, width = 2, format = "d", flag = "0"),
                                        "_gdas0p5", sep = ""))
        }
      }
      
      if(the_month == 12){
        dec <- seq(1, 31, 1)
        for (j in 1:length(dec)){
          the_list <- c(the_list, paste(the_year, "12",
                                        formatC(j, width = 2, format = "d", flag = "0"),
                                        "_gdas0p5", sep = ""))
        }
      }
      
    }
    
    # Determine whether all of the requested files are available on the server
    all_files_available <- ifelse(all(the_list %in% listing), TRUE, FALSE)
    
    # Determine which files are missing
    if (all_files_available == FALSE){
      for (i in 1:length(the_list)){
        if (i == 1) files_missing <- vector(mode = "character", length = 0)
        if (the_list[i] %in% listing){
          NULL
        } else {
          files_missing <- c(files_missing, the_list[i])
        }
      }
      
      # Generate report of missing files
      missing_report <- as.data.frame(mat.or.vec(nr = length(files_missing), nc = 2))
      colnames(missing_report) <- c("Filename", "Date")
      
      # Add names of missing files to the missing file report
      missing_report$Filename <- files_missing
      
      # Get dates for missing files
      for (i in 1:length(files_missing)){
        if (i == 1) dates <- mat.or.vec(nr = length(files_missing), nc = 1)
        dates[i] <- as.character(ymd(gsub("^([0-9]*).*", "\\1", files_missing[i])))
      }
      
      # Add dates of missing files to the missing file report
      missing_report$Date <- dates
      
      # Filter list of files to obtain
      the_list <- setdiff(the_list, files_missing)
      
    }
    
    # Download the requested files from the server
    for (i in 1:length(the_list)){
      download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/gdas1/",
                                the_list[i], sep = ''),
                    destfile = paste(path_met_files, the_list[i], sep = ''),
                    method = "auto",
                    quiet = FALSE,
                    mode = "wb",
                    cacheOK = TRUE)
      
    }
    
    # Write file that contains report of missing files
    if (exists("missing_report")){
      write.table(missing_report,
                  file = paste(path_met_files,
                               "Missing GDAS 0.5 Files - ", 
                               format(Sys.time(), "%B %d %Y %H_%M_%S"),
                               sep = ''),
                  quote = FALSE, row.names = FALSE)
    }
    
  }
  
  # Download list of GDAS0p5 met files for one or days
  if (!is.null(days)){
    
    # Create a list of filenames to compare with those currently available
    # on the NOAA FTP server
    for (i in 1:length(days)){
      if (i == 1) the_list <- vector(mode = "character", length = 0)
      
      the_day <- as.numeric(gsub("^[0-9]*-[0-9][0-9]-([0-9][0-9])", "\\1", days[i]))
      the_month <- as.numeric(gsub("^[0-9]*-([0-9][0-9])-.*", "\\1", days[i]))
      the_year <- as.numeric(gsub("^([0-9]*).*", "\\1", days[i]))
      
      # Determine whether this is a valid date
      
      # Construct the filename and add it to 'the_list'
      the_list <- c(the_list, paste(the_year,
                                    formatC(the_month, width = 2, format = "d", flag = "0"),
                                    formatC(the_day, width = 2, format = "d", flag = "0"),
                                    "_gdas0p5", sep = ""))
      
    }
    
    # Determine whether all of the requested files are available on the server
    all_files_available <- ifelse(all(the_list %in% listing), TRUE, FALSE)
    
    # Determine which files are missing
    if (all_files_available == FALSE){
      for (i in 1:length(the_list)){
        if (i == 1) files_missing <- vector(mode = "character", length = 0)
        if (the_list[i] %in% listing){
          NULL
        } else {
          files_missing <- c(files_missing, the_list[i])
        }
      }
      
      # Generate report of missing files
      missing_report <- as.data.frame(mat.or.vec(nr = length(files_missing), nc = 2))
      colnames(missing_report) <- c("Filename", "Date")
      
      # Add names of missing files to the missing file report
      missing_report$Filename <- files_missing
      
      # Get dates for missing files
      for (i in 1:length(files_missing)){
        if (i == 1) dates <- mat.or.vec(nr = length(files_missing), nc = 1)
        dates[i] <- as.character(ymd(gsub("^([0-9]*).*", "\\1", files_missing[i])))
      }
      
      # Add dates of missing files to the missing file report
      missing_report$Date <- dates
      
      # Filter list of files to obtain
      the_list <- setdiff(the_list, files_missing)
      
    }
    
    # Download the requested files from the server
    for (i in 1:length(the_list)){
      download.file(url = paste("ftp://arlftp.arlhq.noaa.gov/archives/gdas1/",
                                the_list[i], sep = ''),
                    destfile = paste(path_met_files, the_list[i], sep = ''),
                    method = "auto",
                    quiet = FALSE,
                    mode = "wb",
                    cacheOK = TRUE)
      
    }
    
    # Write file that contains report of missing files
    if (exists("missing_report")){
      write.table(missing_report,
                  file = paste(path_met_files,
                               "Missing GDAS 0.5 Files - ", 
                               format(Sys.time(), "%B %d %Y %H_%M_%S"),
                               sep = ''),
                  quote = FALSE, row.names = FALSE)
    }

  }

  # End of function
}


