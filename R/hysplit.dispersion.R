#' Conduct HYSPLIT dispersion runs
#' @description The function executes single/multiple forward or backward HYSPLIT dispersion runs using specified meteorological datasets.
#' @param disp_name an optional, descriptive name for the output file collection
#' @param return_disp_df an option to return a data frame with dispersion data
#' @param write_disp_CSV an option to write disperison data to a CSV file
#' @param plot_maps an option to plot maps of the dispersion particles
#' @param start_lat_deg the starting latitude (in decimal degrees) for the model run(s)
#' @param start_long_deg the starting longitude (in decimal degrees) for the model run(s)
#' @param start_height_m_AGL the starting height (in meters above ground level) for the model run(s)
#' @param simulation_duration_h the duration of each model run (either forward or backward) in hours
#' @param backward_running an option to select whether the dispersion runs should be running forward in time (the default) or in a backward running state.
#' @param met_type an option to select meteorological data files. The options are "gdas1" (Global Data Assimilation System 1-degree resolution data) and 'reanalysis' (NCAR/NCEP global reanalysis data).
#' @param vertical_motion_option a numbered option to select the method used to simulation vertical motion. The methods are: (0) input model data, (1) isobaric, (2) isentropic, (3) constant density, (4) isosigma, (5) from divergence, (6) remap MSL to AGL, (7) average data, and (8) damped magnitude.
#' @param top_of_model_domain_m the upper limit of the model domain in meters
#' @param number_of_particles the number of particles released by source during each release cycle
#' @param max_particles the number of particles released by a source during a model run
#' @param run_type used to select whether models should be run for a single day ("day"), for one or more years ("years"), or within a specified date range ("range").
#' @param run_day used when 'run_type' of 'day' is selected. The date format should be provided here as "YYYY-MM-DD".
#' @param run_range used when 'run_type' of 'range' is selected. The date format should be provided here as "c("YYYY-MM-DD", "YYYY-MM-DD")".
#' @param run_years used when 'run_type' of 'years' is selected. The format should either be a single year ("YYYY") or a range of years ("YYYY-YYYY").
#' @param daily_hours_to_start should consist of a single daily hour in the format "HH", or, several daily hours in the format "c("HH", "HH", "HH", ...)".
#' @param emissions the numbers corresponding to the stored emissions presets. These presets are specified using the function 'hysplit.dispersion.define("emissions")'.
#' @param species the numbers corresponding to the stored species presets. These presets are specified using the function 'hysplit.dispersion.define("species")'.
#' @param grids the numbers corresponding to the stored grid presets. These presets are specified using the function 'hysplit.dispersion.define("grids")'.
#' @param path_met_files a full path should be provided for the location of the meteorological data files relevant to the model options chosen.
#' @param path_output_files a full path should be provided for a location that the trajectory output files will be written.
#' @param path_wd a full path should be provided for the HYSPLIT working directory; the CONTROL file for each model run will be written to and read from this location.
#' @param path_executable the full path and name of the HYSPLIT executable file for trajectory runs must be provided.
#' @export hysplit.dispersion 
#' @examples
#' \dontrun{
#' # Perform a dispersion run lasting 12 hours over two consecutive days, both starting at midnight
#' # Grid presets 1 and 2 will be used as sampling grids
#' # Presets for species and emissions are set to the first (this is also the default)
#' hysplit.dispersion(disp_name = "example",
#'                    return_disp_df = FALSE,
#'                    write_disp_CSV = TRUE,
#'                    start_lat_deg = 49.289328, start_long_deg = -123.117665,
#'                    start_height_m_AGL = 15,
#'                    simulation_duration_h = 12, backward_running = FALSE, met_type = "reanalysis",
#'                    vertical_motion_option = 0, top_of_model_domain_m = 2000,
#'                    emissions = 1,
#'                    species = 1,
#'                    grids = c(1,2),
#'                    run_type = "range", run_range = c("2012-02-01", "2012-02-03"),
#'                    daily_hours_to_start = "00",
#'                    path_met_files = "~/Documents/SplitR/Met/",
#'                    path_output_files = "~/Documents/SplitR/Output/",
#'                    path_wd = "~/Documents/SplitR/Working/",
#'                    path_executable = "~/Documents/SplitR/Exec/hycs_std")
#'}

hysplit.dispersion <- function(disp_name = NULL,
                               return_disp_df = TRUE,
                               write_disp_CSV = TRUE,
                               plot_maps = TRUE,
                               start_lat_deg,
                               start_long_deg,
                               start_height_m_AGL,
                               simulation_duration_h = 24,
                               backward_running = FALSE,
                               met_type = "reanalysis",
                               vertical_motion_option = 0,
                               top_of_model_domain_m = 20000,
                               number_of_particles = 2500,
                               max_particles = 10000,
                               run_type = "day",
                               run_day = "2013-02-01",
                               run_range = c("2013-02-01", "2013-02-10"),
                               run_years = "2013",
                               daily_hours_to_start = "00",
                               emissions = c(1),
                               species = c(1),
                               grids = c(1),
                               path_met_files,
                               path_output_files,
                               path_wd,
                               path_executable){ 
  
  # Add require statements
  require(lubridate)
  require(maps)
  require(mapdata)
  require(ggmap)
  
  # Set parameters
  run_type <- run_type
  run_day <- run_day
  
  # Set number of starting locations to 1 for this function
  no_starting_locations <- 1
  
  # Set number of particles to 1 in the SETUP.CFG file
  setup.cfg <- readLines(paste(path_wd, "SETUP.CFG", sep = ''))
  
  setup.cfg <- gsub(" numpar = ([0-9]*),",
                    paste(" numpar = ", number_of_particles, ",", sep = ''),
                    setup.cfg)
  
  setup.cfg <- gsub(" maxpar = ([0-9]*),",
                    paste(" maxpar = ", max_particles, ",", sep = ''),
                    setup.cfg)
  
  writeLines(setup.cfg, paste(path_wd, "SETUP.CFG", sep = ''))
  
  rm(setup.cfg)
  
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
      
      # Are the met files available on the selected path?
      met.file.df <- setNames(data.frame(mat.or.vec(nr = length(met), nc = 2)),
                              nm = c("file","available"))
      
      if (.Platform$OS.type == "unix"){
        
        for (k in 1:length(met)) {
          met.file.df[k, 1] <- met[k]
          met.file.df[k, 2] <- as.character(file.exists(paste(path_met_files, met[k], sep = '')))}
        
        # Write the met file availability to file
        write.table(met.file.df, file = paste(path_wd, "met_file_list", sep = ''),
                    sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,
                    append = FALSE)
        
        # Download the missing met files
        if (FALSE %in% met.file.df[,2]){
          
          files_to_get <- subset(met.file.df, available == FALSE)[,1]
          
          if (met_type == "reanalysis"){
            get.met.reanalysis(files = files_to_get, path_met_files = path_met_files)
          }
          
          if (met_type == "gdas1"){
            get.met.gdas1(files = files_to_get, path_met_files = path_met_files)
          }
          
        }
        
      }
      
      if (.Platform$OS.type == "windows"){
        
        for (k in 1:length(met)) {
          met.file.df[k, 1] <- met[k]
          met.file.df[k, 2] <- as.character(file.exists(paste(path_met_files,
                                                              met[k], sep = '')))}
        
        # Write the met file availability to file
        write.table(met.file.df, file = paste(path_wd, "met_file_list", sep = ''),
                    sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,
                    append = FALSE)
        
        # Download the missing met files
        if (FALSE %in% met.file.df[,2]){
          
          files_to_get <- subset(met.file.df, available == FALSE)[,1]
          
          if (met_type == "reanalysis"){
            get.met.reanalysis(files = files_to_get, path_met_files = path_met_files)
          }
          
          if (met_type == "gdas1"){
            get.met.gdas1(files = files_to_get, path_met_files = path_met_files)
          }
          
        }
        
      }
      
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
      for (i in 1:length(dispersion.preset.get("emissions", emissions, path_wd = path_wd))){
        cat(dispersion.preset.get("emissions", emissions, path_wd = path_wd)[i], "\n",
            file = paste(path_wd, "CONTROL", sep = ''),
            sep = '', append = TRUE)}
      
      # Write grid blocks to 'CONTROL'
      
      # Get vector text elements through reading selected elements from 'grids' file
      grids_text <- dispersion.preset.get("grids", grids, path_wd = path_wd)
      
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
      for (i in 1:length(dispersion.preset.get("species", species, path_wd = path_wd))){
        cat(dispersion.preset.get("species", species, path_wd = path_wd)[i], "\n",
            file = paste(path_wd, "CONTROL", sep = ''),
            sep = '', append = TRUE)}
      
      # CONTROL file is now complete and in the working directory
      # Execute the model run
      if (.Platform$OS.type == "unix"){
        system(paste("(cd ", path_wd, " && ", path_executable, "hycs_std)",
                     sep = ''))
      }
      
      if (.Platform$OS.type == "windows"){
        shell(paste("(cd ", path_wd, " && ", path_executable, "hycs_std)",
                    sep = ''))
      }
      
      # Extract the particle positions at every hour
      if (.Platform$OS.type == "unix"){
        system(paste("(cd ", path_wd, " && ", path_executable, "parhplot -iPARDUMP -a1)",
                     sep = ''))
      }
      
      if (.Platform$OS.type == "windows"){
        shell(paste("(cd ", path_wd, " && ", path_executable, "parhplot -iPARDUMP -a1)",
                    sep = ''))
      }
      
      # Remove the .att files from the working directory
      if (.Platform$OS.type == "unix"){
        system(paste("(cd ", path_wd, " && rm GIS_part*.att)",
                     sep = ''))
      }
      
      if (.Platform$OS.type == "windows"){
        shell(paste("(cd ", path_wd, " && del GIS_part*.att)",
                    sep = ''))
      }
      
      # Remove the postscript plot from the working directory
      if (.Platform$OS.type == "unix"){
        system(paste("(cd ", path_wd, " && rm parhplot.ps)",
                     sep = ''))
      }
      
      if (.Platform$OS.type == "windows"){
        shell(paste("(cd ", path_wd, " && del parhplot.ps)",
                    sep = ''))
      }
      
      # Rename the .txt files as .csv files
      if (.Platform$OS.type == "unix"){
        system(paste("(cd ", path_wd, " && for files in GIS*.txt;",
                     " do mv \"$files\" \"${files%.txt}.csv\"; done)",
                     sep = ''))
      }
      
      # Remove the 'END' string near the end of each .csv file
      if (.Platform$OS.type == "unix"){
        system(paste("(cd ", path_wd, " && sed -i .bk 's/END//g'",
                     " GIS_part_*.csv; rm *.bk)",
                     sep = ''))
      }
      
      if (.Platform$OS.type == "windows"){        
        temp_file_list <- list.files(path = path_wd, pattern = "*._ps.txt",
                                     full.names = TRUE)
        
        for (i in 1:length(temp_file_list)){
          temp_lines <- readLines(temp_file_list[i])
          temp_lines <- temp_lines[-(length(temp_lines))]
          write.table(temp_lines, file = gsub("txt", "csv", temp_file_list[i]),
                      col.names = FALSE, row.names = FALSE, quote = FALSE)
        }
        
      }
      
      # Move the .csv files from the working directory to the output folder
      if (.Platform$OS.type == "unix"){
        #         system(paste("(cd ", path_wd, " && mv GIS_part*.csv '", path_output_files, "')",
        #                      sep = ''))
        #         
        if (is.null(disp_name)){
          folder_name <- paste("disp--", format(Sys.time(), "%Y-%m-%d--%H-%M-%S"), sep = '')  
        } else if (!is.null(disp_name)){
          folder_name <- paste(disp_name, "--", format(Sys.time(), "%Y-%m-%d--%H-%M-%S"), sep = '')  
        }
        
        # Perform the movement of all dispersion files into a folder residing in the
        # output directory
        dir.create(path = paste(path_output_files, folder_name, sep = ''))
        
        system(paste("(cd ", path_wd, " && mv GIS_part*.csv '", path_output_files, folder_name,
                     "')", sep = ''))
        
      }
      
      if (.Platform$OS.type == "windows"){
        system(paste("(cd ", path_wd, " && mv GIS_part*.csv '", path_output_files, "')",
                     sep = ''))
      }
      
      # Close the hour loop 
    }
    
    # Close the day loop 
  }
  
  
  # Write the dispersion data frame to a CSV if it is requested
  if (write_disp_CSV == TRUE){
    
    disp.df <- dispersion.read(archive_folder =
                                 paste(path_output_files, folder_name, sep = ''))
    
    if (.Platform$OS.type == "unix"){
      write.table(disp.df, file = paste(path_output_files, folder_name, "/dispersion.csv", sep = ''),
                  sep = ",", row.names = FALSE)
    }
    
    if (.Platform$OS.type == "windows"){
      write.table(disp.df, file = paste(path_output_files, folder_name, "\\dispersion.csv", sep = ''),
                  sep = ",", row.names = FALSE)
      
    }
    
  }
  
  # Return a dispersion data frame if it is requested
  if (return_disp_df == TRUE){
    
    disp.df <- dispersion.read(archive_folder =
                                 paste(path_output_files, folder_name, sep = ''))
    
    return(disp.df)
    
  }
  
  # Plot a map of the dispersion data if it is requested
  if (plot_maps == TRUE){
    
    disp.df <- dispersion.read(archive_folder =
                                 paste(path_output_files, folder_name, sep = ''))
    
    bbox <- make_bbox(lon = disp.df$lon, lat = disp.df$lat)
    
    map <- get_map(location = bbox, maptype = "terrain",
                   source = "osm")
    
    for (h in 1:simulation_duration_h){
      
      disp.hour <- subset(disp.df, hour == h)
      
      gg <- ggmap(ggmap = map) +
        geom_point(aes(x = disp.hour$lon,
                       y = disp.hour$lat,
                       size = 0.5 * disp.hour$height/10000,
                       alpha = 0.1)) +
        geom_smooth(aes(x = disp.hour$lon,
                        y = disp.hour$lat), method = lm) +
        geom_point(x = start_lat_deg, y = start_long_deg, shape = 1, alpha = 1) +
        theme(legend.position = "none")
      
      ggsave(filename = paste("dispersion-map-h", h, ".png"),
             path = path_output_files)
      
    }
    
  }
  
}