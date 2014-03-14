hysplit.dispersion <- function(start_lat_deg = 49.289328,
                               start_long_deg = -123.117665,
                               start_height_m_AGL = 10,
                               simulation_duration_h = 24,
                               backward_running = FALSE,
                               met_type = "reanalysis",
                               vertical_motion_option = 0,
                               top_of_model_domain_m = 20000,
                               run_type = "day",
                               run_day = "2013-02-01",
                               run_range = c("2013-02-01", "2013-02-10"),
                               run_years = "2013",
                               daily_hours_to_start = "00",
                               emissions = c(1),
                               species = c(1),
                               grids = c(1),
                               path_met_files = "~/Documents/SplitR/Met/",
                               path_output_files = paste(getwd(), "/", sep = ''),
                               path_wd = "~/Documents/SplitR/Working/",
                               path_executable = "~/Documents/SplitR/Exec/"){ 
  
  # Define package requirements
  require(lubridate)
  require(maps)
  require(mapdata)
  
  # Set parameters
  run_type <- run_type
  run_day <- run_day
  
  # Set number of starting locations to 1 for this function
  no_starting_locations <- 1  
  
  #   # Determine whether the run_years input is a single year or a range
  #   if(exists("run_years")) run_years_single_range <-
  #     ifelse(nchar(run_years) == 4, "single", "range") 
  
  # Make a vector list of run days in POSIXct format
  if (exists("run_type") & run_type == "day" & exists("run_day")){
    list_run_days <- as.POSIXct(run_day, origin = "1970-01-01", tz = "UTC")}
  
  if (exists("run_type") & run_type == "range" & exists("run_range")){
    list_run_days <- seq(as.POSIXct(run_range[1], origin = "1970-01-01", tz = "UTC"),
                         as.POSIXct(run_range[2], origin = "1970-01-01", tz = "UTC"),
                         by = 86400)}
  
  if (exists("run_type") & run_type == "years" & exists("run_years")){
    run_years_single_range <-
      ifelse(nchar(run_years) == 4, "single", "range")
    
    list_run_days <- seq(as.POSIXct(paste(substr(run_years, 1, 4),"-01-01", sep = ''), 
                                    origin = "1970-01-01", tz = "UTC"),
                         as.POSIXct(ifelse(run_years_single_range == "single",
                                           paste(substr(run_years, 1, 4),"-12-31",sep = ''),
                                           paste(substr(run_years, 6, 9),"-12-31",sep = '')), 
                                    origin = "1970-01-01", tz = "UTC"),
                         by = 86400)}
  
  # Make loop with all run days
  for (i in 1:length(list_run_days)) {
    
    # Define starting time parameters
    start_year_GMT <- substr(as.character(year(list_run_days[i])),3,4)
    
    
    start_month_GMT <- formatC(as.numeric(month(list_run_days[i])),
                               width = 2, format = "d", flag = "0")
    
    
    start_day_GMT <- formatC(as.numeric(day(list_run_days[i])),
                             width = 2, format = "d", flag = "0")
    
    
    # Make nested loop with daily beginning hours
    for (j in daily_hours_to_start) {    
      
      start_hour_GMT <- j
      
      #--- Determine which met files are required for this run
      
      # Determine the start time of the model run
      start_time_GMT <- ymd_hms(paste(ifelse(start_year_GMT > 50,
                                             paste("19", start_year_GMT, sep = ''),
                                             start_year_GMT), "-",
                                      start_month_GMT, "-",
                                      start_day_GMT, " ",
                                      start_hour_GMT, ":00:00",
                                      sep = ''))
      # Determine the end time of the model run
      end_time_GMT <- as.POSIXct(ifelse(backward_running == TRUE, 
                                        start_time_GMT - (simulation_duration_h * 3600),
                                        start_time_GMT + (simulation_duration_h * 3600)),
                                 origin = "1970-01-01", tz = "UTC")
      
      # Determine whether the start year is a leap year
      leap_year <- leap_year(ymd(paste(start_year_GMT, "-",
                                       start_month_GMT, "-",
                                       start_day_GMT, sep = '')))
      
      # Determine whether the beginning and end of the current run
      # crosses over a calendar year
      number_of_calendar_years <- ifelse(year(start_time_GMT) == year(end_time_GMT), 1, 2)
      
      # Determine whether the beginning and end of the current run
      # crosses over a calendar month
      number_of_calendar_months <- ifelse(month(start_time_GMT) == month(end_time_GMT), 1, 2)
      
      #--- Divide different requirements for met files into different cases
      
      # Set the different cases to FALSE by default
      case_within_month <- FALSE
      case_over_year <- FALSE
      case_over_month <- FALSE
      
      # Determine which of the three cases is true
      if (number_of_calendar_years == 1 & number_of_calendar_months == 1) {
        case_within_month <- TRUE
      } else if (number_of_calendar_years > 1) {
        case_over_year <- TRUE
      } else if (number_of_calendar_months > 1) {
        case_over_month <- TRUE
      } else { NULL }
      
      #--- Get vector lists of met files applicable to run from GDAS 1-degree dataset
      
      # Trap leap-year condition of missing .w5 met file for February in a '0' list value
      if (case_within_month == TRUE &
            met_type == "gdas1") met <- 
        c(paste("gdas1.",
                substr(tolower(format(start_time_GMT, "%B")), 1, 3),
                substr(year(start_time_GMT), 3, 4), ".w1", sep = ''),                                      
          paste("gdas1.",
                substr(tolower(format(start_time_GMT, "%B")), 1, 3),
                substr(year(start_time_GMT), 3, 4), ".w2", sep = ''),
          paste("gdas1.",
                substr(tolower(format(start_time_GMT, "%B")), 1, 3),
                substr(year(start_time_GMT), 3, 4), ".w3", sep = ''),
          paste("gdas1.",
                substr(tolower(format(start_time_GMT, "%B")), 1, 3),
                substr(year(start_time_GMT), 3, 4), ".w4", sep = ''),
          ifelse(month(start_time_GMT) == 2 &
                   leap_year == TRUE, 0,
                 paste("gdas1.",
                       substr(tolower(format(start_time_GMT, "%B")), 1, 3),
                       substr(year(start_time_GMT), 3, 4), ".w5", sep = '')))
      
      if (case_over_year == TRUE &
            met_type == "gdas1") met <- 
        c(paste("gdas1.dec",
                substr(year(end_time_GMT), 3, 4), ".w3", sep = ''),                                      
          paste("gdas1.dec",
                substr(year(end_time_GMT), 3, 4), ".w4", sep = ''),
          paste("gdas1.dec",
                substr(year(end_time_GMT), 3, 4), ".w5", sep = ''),
          paste("gdas1.jan",
                substr(year(start_time_GMT), 3, 4), ".w1", sep = ''),
          paste("gdas1.jan",
                substr(year(start_time_GMT), 3, 4), ".w2", sep = ''),
          paste("gdas1.jan",
                substr(year(start_time_GMT), 3, 4), ".w3", sep = ''))
      
      if (case_over_month == TRUE &
            met_type == "gdas1") met <-
        c(paste("gdas1.",
                substr(tolower(format(end_time_GMT, "%B")), 1, 3),
                substr(year(end_time_GMT), 3, 4), ".w3", sep = ''),                                      
          paste("gdas1.",
                substr(tolower(format(end_time_GMT, "%B")), 1, 3),
                substr(year(end_time_GMT), 3, 4), ".w4", sep = ''),
          ifelse(month(end_time_GMT) == 2 &
                   leap_year == TRUE, 0,
                 paste("gdas1.",
                       substr(tolower(format(end_time_GMT, "%B")), 1, 3),
                       substr(year(end_time_GMT), 3, 4), ".w5", sep = '')),
          paste("gdas1.",
                substr(tolower(format(start_time_GMT, "%B")), 1, 3),
                substr(year(start_time_GMT), 3, 4), ".w1", sep = ''),
          paste("gdas1.",
                substr(tolower(format(start_time_GMT, "%B")), 1, 3),
                substr(year(start_time_GMT), 3, 4), ".w2", sep = ''),
          paste("gdas1.",
                substr(tolower(format(start_time_GMT, "%B")), 1, 3),
                substr(year(start_time_GMT), 3, 4), ".w3", sep = ''))
      
      # Get vector lists of met files applicable to run from the NCEP/NCAR reanalysis dataset
      if (met_type == "reanalysis") met <- 
        c(paste("RP",
                ifelse(start_month_GMT == "01",
                       year(start_time_GMT) - 1,
                       year(start_time_GMT)),
                ifelse(start_month_GMT == "01", "12",
                       formatC(month(start_time_GMT)-1, width = 2, format = "d", flag = "0")),
                ".gbl",
                sep = ''),
          paste("RP",
                year(start_time_GMT),
                start_month_GMT, ".gbl",
                sep = ''),
          paste("RP",
                ifelse(start_month_GMT == "12",
                       year(start_time_GMT) + 1,
                       year(start_time_GMT)),
                ifelse(start_month_GMT == "12", "01",
                       formatC(month(start_time_GMT)+1, width = 2, format = "d", flag = "0")),
                ".gbl",
                sep = ''))
      
      # Remove list values containing '0' (representing missing .w5
      # data files for Feb in leap years)
      if(exists("met")) met <- met[!met %in% c(0)]
      
      #       # Are the met files available on the selected path?
      #       met.file.df <- setNames(data.frame(mat.or.vec(nr = length(met), nc = 2)),
      #                               nm = c("file","available?"))
      #       for (k in 1:length(met)) {
      #         met.file.df[k, 1] <- met[k]
      #         met.file.df[k, 2] <- as.character(file.exists(paste(path_met_files, met[k], sep = '')))}
      
      # Construct the output filename string for this model run
      output_filename <- paste("--disp",
                               ifelse(backward_running == TRUE, '(back)', '(forward)'), "-",
                               start_year_GMT, "-",
                               start_month_GMT, "-",
                               start_day_GMT, "-",
                               start_hour_GMT, "-",
                               "lat_", start_lat_deg, "_",
                               "long_",start_long_deg, "-",
                               "height_",start_height_m_AGL, "-",
                               simulation_duration_h, "h", sep = '')
      
      # Write start year, month, day, hour to 'CONTROL'
      cat(start_year_GMT, " ", 
          start_month_GMT, " ",
          start_day_GMT, " ",
          start_hour_GMT, "\n",
          file = paste(path_wd, "CONTROL", sep = ''),
          sep = '', append = FALSE)
      
      #Write number of starting locations to 'CONTROL'
      cat(no_starting_locations, "\n",
          file = paste(path_wd, "CONTROL", sep = ''),
          sep = '', append = TRUE)
      
      # Write starting latitude, longitude, height AGL to 'CONTROL'
      cat(start_lat_deg, " ", 
          start_long_deg, " ", 
          start_height_m_AGL, "\n",
          file = paste(path_wd, "CONTROL", sep = ''),
          sep = '', append = TRUE)
      
      # Write direction and number of simulation hours to 'CONTROL'
      cat(ifelse(backward_running == TRUE, "-", ""),
          simulation_duration_h, "\n",
          file = paste(path_wd, "CONTROL", sep = ''),
          sep = '', append = TRUE)
      
      # Write vertical motion option to 'CONTROL'
      cat(vertical_motion_option, "\n",
          file = paste(path_wd, "CONTROL", sep = ''),
          sep = '', append = TRUE)
      
      # Write top of model domain in meters to 'CONTROL'
      cat(top_of_model_domain_m, "\n",
          file = paste(path_wd, "CONTROL", sep = ''),
          sep = '', append = TRUE)
      
      # Write number of met files used to 'CONTROL'
      cat(length(met), "\n",
          file = paste(path_wd, "CONTROL", sep = ''),
          sep = '', append = TRUE)
      
      # Write met file paths to 'CONTROL'
      for (i in 1:length(met)){
        cat(path_met_files, "\n", met[i], "\n",
            file = paste(path_wd, "CONTROL", sep = ''),
            sep = '', append = TRUE)}
      
      # Write emissions blocks to 'CONTROL'
      for (i in 1:length(dispersion.preset.get("emissions", emissions))){
        cat(dispersion.preset.get("emissions", emissions)[i], "\n",
            file = paste(path_wd, "CONTROL", sep = ''),
            sep = '', append = TRUE)}
      
      # Write grid blocks to 'CONTROL'
      
      # Get vector text elements through reading selected elements from 'grids' file
      grids_text <- dispersion.preset.get("grids", grids)
      
      # Get vector text indices that contain the short name(s) of the grid(s)
      gridnames_indices <- seq(from = 1 + 5, to = length(grids_text) - 5, by = 10)
      
      # Combine short grid name string with longer 'output_filename' string
      for (i in 1:((length(grids_text) - 1)/10)){
        grids_text[gridnames_indices[i]] <- paste(grids_text[gridnames_indices[i]],
                                                  output_filename, sep = '')
      }
      
      # Write grid blocks to 'CONTROL'
      for (i in 1:length(grids_text)){
        cat(grids_text[i], "\n",
            file = paste(path_wd, "CONTROL", sep = ''),
            sep = '', append = TRUE)}
      
      # Write species blocks to 'CONTROL'
      for (i in 1:length(dispersion.preset.get("species", species))){
        cat(dispersion.preset.get("species", species)[i], "\n",
            file = paste(path_wd, "CONTROL", sep = ''),
            sep = '', append = TRUE)}
      
      # CONTROL file is now complete and in the working directory
      # Execute the model run
      system(paste("(cd ", path_wd, " && ", path_executable, "hycs_std)",
                   sep = ''))
      
      # Extract the particle positions at every hour
      system(paste("(cd ", path_wd, " && ", path_executable, "parhplot -iPARDUMP -a1)",
                   sep = ''))
      
      # Remove the .att files from the working directory
      system(paste("(cd ", path_wd, " && rm GIS_part*.att)",
                   sep = ''))
      
      # Remove the postscript plot from the working directory
      system(paste("(cd ", path_wd, " && rm parhplot.ps)",
                   sep = ''))
      
      # Rename the .txt files as .csv files
      system(paste("(cd ", path_wd, " && for files in GIS*.txt;",
                   " do mv \"$files\" \"${files%.txt}.csv\"; done)",
                   sep = ''))
      
      # Remove the 'END' string near the end of each .csv file
      system(paste("(cd ", path_wd, " && sed -i .bk 's/END//g'",
                   " GIS_part_*.csv; rm *.bk)",
                   sep = ''))
      
      # Create list of particle positions for each hour of the model run
      for (i in 1:simulation_duration_h){
        if (i == 1) particle_positions <- vector(mode = "list",
                                                 length = simulation_duration_h)
        particle_positions[[i]] <-
          read.csv(paste("~/Documents/SplitR/Working/GIS_part_",
                         formatC(i, width = 3, format = "d", flag = "0"),
                         "_ps.csv", sep = ''),
                   col.names = c("p_no", "longitude", "latitude", "height_mAGL"))
      }
      
      # Get limits of latitude and longitude
      for (i in 1:simulation_duration_h){
        if (i == 1){
          min_longitude <- vector(mode = "numeric", length = simulation_duration_h)
          max_longitude <- vector(mode = "numeric", length = simulation_duration_h)
          min_latitude <- vector(mode = "numeric", length = simulation_duration_h)
          max_latitude <- vector(mode = "numeric", length = simulation_duration_h)    
        }
        
        min_longitude[i] <- min(particle_positions[[i]]$longitude)
        max_longitude[i] <- max(particle_positions[[i]]$longitude)
        min_latitude[i] <- min(particle_positions[[i]]$latitude)
        max_latitude[i] <- max(particle_positions[[i]]$latitude)
        
        if (i == simulation_duration_h){
          min_longitude_i <- min(min_longitude)
          max_longitude_i <- max(max_longitude)
          min_latitude_i <- min(min_latitude)
          max_latitude_i <- max(max_latitude)
          
          rm(min_longitude, max_longitude, min_latitude, max_latitude, i)
        }
      }
      
      # Move the .csv files from the working directory to the output folder
      system(paste("(cd ", path_wd, " && mv GIS_part*.csv ", path_output_files, ")",
                   sep = ''))
      
      # Create a particle graphic for each hour and place in the output folder
      for (i in 1:simulation_duration_h){
        jpeg(filename = paste(path_output_files, "map-", output_filename, "-",
                             formatC(i, width = 3, format = "d", flag = "0"),
                             ".jpg", sep = ''))
        map(database = "worldHires", xlim = c(min_longitude_i, max_longitude_i),
            ylim = c(min_latitude_i, max_latitude_i), col = "gray90", fill = TRUE)
        
        if (top_of_model_domain_m >= 500){
        h__1000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL > 0 &
                            particle_positions[[i]]$height_mAGL < 1000)
        
        points(x = h__1000$longitude, y = h__1000$latitude,
               pch = 1, cex = 0.2, col = 113)
        }
        
        if (top_of_model_domain_m >= 1000){
        h__2000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 1000 &
                            particle_positions[[i]]$height_mAGL < 2000)
        
        points(x = h__2000$longitude, y = h__2000$latitude,
               pch = 1, cex = 0.2, col = 51)
        }
        
        if (top_of_model_domain_m >= 2000){
        h__3000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 2000 &
                            particle_positions[[i]]$height_mAGL < 3000)
        
        points(x = h__3000$longitude, y = h__3000$latitude,
               pch = 1, cex = 0.2, col = 50)
        }
        
        if (top_of_model_domain_m >= 3000){
        h__4000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 3000 &
                            particle_positions[[i]]$height_mAGL < 4000)
        
        points(x = h__4000$longitude, y = h__4000$latitude,
               pch = 1, cex = 0.2, col = 48)
        }
        
        if (top_of_model_domain_m >= 4000){
        h__5000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 4000 &
                            particle_positions[[i]]$height_mAGL < 5000)
        
        points(x = h__5000$longitude, y = h__5000$latitude,
               pch = 1, cex = 0.2, col = 254)
        }
        
        if (top_of_model_domain_m >= 5000){
        h__6000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 5000 &
                            particle_positions[[i]]$height_mAGL < 6000)
        
        points(x = h__6000$longitude, y = h__6000$latitude,
               pch = 1, cex = 0.2, col = 96)
        }
        
        if (top_of_model_domain_m >= 6000){
        h__7000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 6000 &
                            particle_positions[[i]]$height_mAGL < 7000)
        
        points(x = h__7000$longitude, y = h__7000$latitude,
               pch = 1, cex = 0.2, col = 97)
        }
        
        if (top_of_model_domain_m >= 7000){
        h__8000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 7000 &
                            particle_positions[[i]]$height_mAGL < 8000)
        
        points(x = h__8000$longitude, y = h__8000$latitude,
               pch = 1, cex = 0.2, col = 98)
        }
        
        if (top_of_model_domain_m >= 8000){
        h__9000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 8000 &
                            particle_positions[[i]]$height_mAGL < 9000)
        
        points(x = h__9000$longitude, y = h__9000$latitude,
               pch = 1, cex = 0.2, col = 95)
        }
        
        if (top_of_model_domain_m >= 9000){
        h__10000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 9000 &
                            particle_positions[[i]]$height_mAGL < 10000)
        
        points(x = h__10000$longitude, y = h__10000$latitude,
               pch = 1, cex = 0.2, col = 99)
        }
        
        if (top_of_model_domain_m >= 10000){
        h__11000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 10000 &
                            particle_positions[[i]]$height_mAGL < 11000)
        
        points(x = h__11000$longitude, y = h__11000$latitude,
               pch = 1, cex = 0.2, col = 305)
        }
        
        if (top_of_model_domain_m >= 11000){
        h__12000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 11000 &
                            particle_positions[[i]]$height_mAGL < 12000)
        
        points(x = h__12000$longitude, y = h__12000$latitude,
               pch = 1, cex = 0.2, col = 302)
        }
        
        if (top_of_model_domain_m >= 12000){
        h__13000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 12000 &
                            particle_positions[[i]]$height_mAGL < 13000)
        
        points(x = h__13000$longitude, y = h__13000$latitude,
               pch = 1, cex = 0.2, col = 299)
        }
        
        if (top_of_model_domain_m >= 13000){
        h__14000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 13000 &
                            particle_positions[[i]]$height_mAGL < 14000)
        
        points(x = h__14000$longitude, y = h__14000$latitude,
               pch = 1, cex = 0.2, col = 296)
        }
        
        if (top_of_model_domain_m >= 14000){
        h__15000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 14000 &
                            particle_positions[[i]]$height_mAGL < 15000)
        
        points(x = h__15000$longitude, y = h__15000$latitude,
               pch = 1, cex = 0.2, col = 293)
        }
        
        if (top_of_model_domain_m >= 15000){
        h__16000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 15000 &
                            particle_positions[[i]]$height_mAGL < 16000)
        
        points(x = h__16000$longitude, y = h__16000$latitude,
               pch = 1, cex = 0.2, col = 290)
        }
        
        if (top_of_model_domain_m >= 16000){
        h__17000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 16000 &
                            particle_positions[[i]]$height_mAGL < 17000)
        
        points(x = h__17000$longitude, y = h__17000$latitude,
               pch = 1, cex = 0.2, col = 287)
        }
        
        if (top_of_model_domain_m >= 17000){
        h__18000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 17000 &
                            particle_positions[[i]]$height_mAGL < 18000)
        
        points(x = h__18000$longitude, y = h__18000$latitude,
               pch = 1, cex = 0.2, col = 284)
        }
        
        if (top_of_model_domain_m >= 18000){
        h__19000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 18000 &
                            particle_positions[[i]]$height_mAGL < 19000)
        
        points(x = h__19000$longitude, y = h__19000$latitude,
               pch = 1, cex = 0.2, col = 281)
        }
        
        if (top_of_model_domain_m >= 19000){
        h__20000 <- subset(particle_positions[[i]],
                          particle_positions[[i]]$height_mAGL >= 19000 &
                            particle_positions[[i]]$height_mAGL < 20000)
        
        points(x = h__20000$longitude, y = h__20000$latitude,
               pch = 1, cex = 0.2, col = 278)
        }
        
        if (top_of_model_domain_m >= 20000){
        h_20000_ <- subset(particle_positions[[i]],
                           particle_positions[[i]]$height_mAGL >= 20000)
        
        points(x = h_20000_$longitude, y = h_20000_$latitude,
               pch = 1, cex = 0.2, col = 275)
        }
        
        dev.off()
      }
      
      # Close the hour loop 
    }
    
    # Close the day loop 
  }
  
}

