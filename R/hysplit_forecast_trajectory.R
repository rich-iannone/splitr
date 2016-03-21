#' Conduct HYSPLIT forecast trajectory runs
#' @description The function executes a HYSPLIT
#' forecast trajectory run using NAM forecast
#' meteorological data (CONUS, 12 km, 3 hrly, pressure
#' levels, 48 h).
#' @param start_lat_deg the starting latitude (in
#' decimal degrees) for the model run(s)
#' @param start_long_deg the starting longitude (in
#' decimal degrees) for the model run(s)
#' @param start_height_m_AGL the starting height (in
#' meters above ground level) for the model run(s)
#' @param simulation_duration_h the duration of each
#' model run (either forward or backward) in hours
#' @param backtrajectory an option to select whether
#' to conduct forward trajectory model runs or to
#' generate backtrajectories
#' @param met_type an option to select meteorological
#' data files. The options are "gdas1" (Global Data
#' Assimilation System 1-degree resolution data) and
#' 'reanalysis' (NCAR/NCEP global reanalysis data). 
#' @param forecast_cycle the forecast cycle to use;
#' defaults to \code{latest}.
#' @param vertical_motion_option a numbered option to
#' select the method used to simulation vertical
#' motion. The methods are: (0) input model data, (1)
#' isobaric, (2) isentropic, (3) constant density, (4)
#' isosigma, (5) from divergence, (6) remap MSL to AGL,
#' (7) average data, and (8) damped magnitude.
#' @param top_of_model_domain_m 
#' @param run_start 
#' @param path_met_files a full path should be
#' provided for the location of the meteorological data
#' files relevant to the model options chosen. By
#' default, the location '~/Documents/SplitR/Met' is
#' chosen.
#' @param path_output_files a full path should be
#' provided for a location that the trajectory output
#' files will be written. By default, the location
#' '~/Documents/SplitR/Output' is chosen.
#' @param path_wd a full path should be provided for
#' the HYSPLIT working directory; the CONTROL file for
#' each model run will be written to and read from this
#' location. By default, the location
#' '~/Documents/SplitR/Working' is chosen.
#' @param path_executable the full path and name of the
#' HYSPLIT executable file for trajectory runs must be
#' provided. By default, the location
#' '~/Documents/SplitR/Exec' is chosen.
#' @import lubridate
#' @export hysplit_forecast_trajectory

hysplit_forecast_trajectory <- function(start_lat_deg,
                                        start_long_deg,
                                        start_height_m_AGL,
                                        simulation_duration_h = 12,
                                        backtrajectory = FALSE,
                                        met_type = "nam",
                                        forecast_cycle = "latest",
                                        vertical_motion_option = 0,
                                        top_of_model_domain_m = 20000,
                                        run_start,
                                        path_met_files,
                                        path_output_files,
                                        path_wd,
                                        path_executable){
  
  # Set number of starting locations to 1 for this function
  no_starting_locations <- 1
  
  # Obtain the appropriate forecast met files, depending on user selection
  if (met_type == "nam"){
    
    # If the most recent NAM forecast met file isn't already available, get the file
    today <- gsub("-", "", Sys.Date())
    
    if (paste0(path.expand(path_met_files), paste0(today, ".t00z.namf")) %in%
        list.files(path = gsub("/$", "", path_met_files),
                   full.names = TRUE) == FALSE){
      get_met_forecast_nam(path_met_files = path_met_files)
    }
    
    start_year_GMT <- year(Sys.Date())
    
    start_month_GMT <- formatC(month(Sys.Date()),
                               width = 2, format = "d", flag = "0")
    
    start_day_GMT <- formatC(day(Sys.Date()),
                             width = 2, format = "d", flag = "0")
    
    start_hour_GMT <- run_start_hour
    
    # Construct the output filename string for this model run
    output_filename <- 
      paste0("traj-forecast",
             ifelse(backtrajectory == TRUE,
                    '(back)', '(forward)'), "-",
             start_year_GMT, "-",
             start_month_GMT, "-",
             start_day_GMT, "-",
             start_hour_GMT, "-",
             "lat_", start_lat_deg, "_",
             "long_",start_long_deg, "-",
             "height_",start_height_m_AGL, "-",
             simulation_duration_h, "h")
    
    # Write start year, month, day, hour to 'CONTROL'
    cat(start_year_GMT, " ", 
        start_month_GMT, " ",
        start_day_GMT, " ",
        start_hour_GMT, "\n",
        file = paste0(path_wd, "CONTROL"),
        sep = '', append = FALSE)
    
    # Write number of starting locations to 'CONTROL'
    cat(no_starting_locations, "\n",
        file = paste0(path_wd, "CONTROL"),
        sep = '', append = TRUE)
    
    # Write starting latitude, longitude, height AGL to 'CONTROL'
    cat(start_lat_deg, " ", 
        start_long_deg, " ", 
        start_height_m_AGL, "\n",
        file = paste0(path_wd, "CONTROL"),
        sep = '', append = TRUE)
    
    # Write direction and number of simulation hours to 'CONTROL'
    cat(ifelse(backtrajectory == TRUE, "-", ""),
        simulation_duration_h, "\n",
        file = paste0(path_wd, "CONTROL"),
        sep = '', append = TRUE)
    
    # Write vertical motion option to 'CONTROL'
    cat(vertical_motion_option, "\n",
        file = paste0(path_wd, "CONTROL"),
        sep = '', append = TRUE)
    
    # Write top of model domain in meters to 'CONTROL'
    cat(top_of_model_domain_m, "\n",
        file = paste0(path_wd, "CONTROL"),
        sep = '', append = TRUE)
    
    # Write number of met files used to 'CONTROL'
    cat("1", "\n",
        file = paste0(path_wd, "CONTROL"),
        sep = '', append = TRUE)
    
    # Write met file path to 'CONTROL'
    cat(path_met_files, "\n",
        file = paste0(path_wd, "CONTROL"),
        sep = '', append = TRUE)
    
    # Write file name for met file
    cat(paste0(today, ".t00z.namf"), "\n",
        file = paste0(path_wd, "CONTROL"),
        sep = '', append = TRUE)
    
    # Write path for trajectory output files to 'CONTROL'
    cat(path_wd, "\n",
        file = paste0(path_wd, "CONTROL"),
        sep = '', append = TRUE)
    
    # Write name of output filename to 'CONTROL'
    cat(output_filename, "\n",
        file = paste0(path_wd, "CONTROL"),
        sep = '', append = TRUE)
    
    # CONTROL file is now complete and in the working
    # directory Execute the model run; the system call
    # will be different whether the platform in win or
    # unix
    if (.Platform$OS.type == "unix"){
      system(paste0("(cd ", path_wd, " && ", path_executable, "hyts_std)"))
      
      # Copy files from 'Working' to current project folder
      system(paste0("(cd ", path_wd, " && mv traj* '", path_output_files, "')"))
    }
    
    if (.Platform$OS.type == "windows"){
      shell(paste0("(cd ", path_wd, " && ", path_executable, "hyts_std)"))
      
      # Copy files from 'Working' to the output folder
      #      shell(paste0("(cd ", path_wd, " && move traj* '", path_output_files, "')"))
    } 
  }  
}
