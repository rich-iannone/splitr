hysplit.forecast.trajectory <- function(start_lat_deg,
                                        start_long_deg,
                                        start_height_m_AGL,
                                        simulation_duration_h = 24,
                                        backtrajectory = FALSE,
                                        met_type = "nam",
                                        forecast_cycle = "latest",
                                        vertical_motion_option = 0,
                                        top_of_model_domain_m = 20000,
                                        run_start = NULL,
                                        daily_hours_to_start,
                                        path_met_files = "~/Documents/SplitR/Met/",
                                        path_output_files = paste(getwd(), "/", sep = ''),
                                        path_wd = "~/Documents/SplitR/Working/",
                                        path_executable = "~/Documents/SplitR/Exec/") {
  
  # Define package requirements
  require(lubridate)
  
  # Reset path for output files to 'Output' folder if not in a project
  if (grepl("Documents/SplitR/Projects/.*_[0-9][0-9][0-9][0-9]-.*",
            path_output_files) == FALSE){
    path_output_files <- "~/Documents/SplitR/Output/"
  }
  
  # Set number of starting locations to 1 for this function
  no_starting_locations <- 1

  # If the most recent NAM forecast met file isn't already available, get the file
  today <- gsub("-", "", Sys.Date())
  
  if (paste(path.expand(path_met_files), paste(today, ".t00z.namf", sep = ''),
            sep = '') %in% list.files(path = gsub("/$", "", path_met_files),
                                      full.names = TRUE) == FALSE) {
    get.met.forecast.nam(path_met_files)
  }
  
  # Construct the output filename string for this model run
  output_filename <- paste("traj-forecast",
                           ifelse(backtrajectory == TRUE, '(back)', '(forward)'), "-",
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
  
  # Write number of starting locations to 'CONTROL'
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
  cat(ifelse(backtrajectory == TRUE, "-", ""),
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
  cat("1", "\n",
      file = paste(path_wd, "CONTROL", sep = ''),
      sep = '', append = TRUE)
  
  # Write met file path to 'CONTROL'
  cat(path_met_files, "\n", paste(path_met_files, paste(today, ".t00z.namf", sep = ''),
                                  sep = ''),
      file = paste(path_wd, "CONTROL", sep = ''),
      sep = '', append = TRUE)
  
  # Write path for trajectory output files to 'CONTROL'
  cat(path_wd, "\n",
      file = paste(path_wd, "CONTROL", sep = ''),
      sep = '', append = TRUE)
  
  # Write name of output filename to 'CONTROL'
  cat(output_filename, "\n",
      file = paste(path_wd, "CONTROL", sep = ''),
      sep = '', append = TRUE)
  
  # CONTROL file is now complete and in the working directory
  # Execute the model run
  system(paste("(cd ", path_wd, " && ", path_executable, "hyts_std)", sep = ''))
  
  # Copy files from 'Working' to current project folder
  system(paste("(cd ", path_wd, " && mv traj* '", path_output_files, "')", sep = ''))
  
  # Close the function
}
