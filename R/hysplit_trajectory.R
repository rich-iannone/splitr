#' Conduct HYSPLIT trajectory runs
#'
#' The function executes single/multiple forward or backward HYSPLIT trajectory
#' runs using specified meteorological datasets.
#'
#' @param lat,lon The starting latitude and longitude (in decimal degrees) for
#'   each model run.
#' @param height The starting height (in meters above ground level) for each
#'   model run.
#' @param duration The duration of each model run (either forward or backward)
#'   in hours.
#' @param days A vector of days that the model will run. This is combined with
#'   the `daily_hours` to produce a series of date-times.
#' @param daily_hours Should consist of a single daily hour as an integer hour
#'   (from `0` to `23`), or, a vector of several daily hours represented as
#'   integers.
#' @param direction An option to select whether to conduct the model in the
#'   `"forward"` (default) or `"backward"` directions.
#' @param met_type An option to select meteorological data files. The options
#'   are `"reanalysis"` (NCAR/NCEP global reanalysis data, the default),
#'   `"gdas1"` (Global Data Assimilation System 1-degree resolution data), and
#'   `"narr"` (North American Regional Reanalysis).
#' @param vert_motion A numbered option to select the method used to simulation
#'   vertical motion. The methods are: `0` (input model data), `1` (isobaric),
#'   `2` (isentropic), `3` (constant density), `4` (isosigma), `5` (from
#'   divergence), `6` (remap MSL to AGL), `7` (average data), and `8` (damped
#'   magnitude).
#' @param model_height The upper limit of the model domain in meters.
#' @param extended_met An option to report additional meteorological data along
#'   each output trajectory.
#' @param return_traj_df An option to return a data frame with trajectory data.
#' @param traj_name An optional, descriptive name for the output file
#'   collection.
#' @param exec_dir An optional file path for the working directory of the model
#'   input and output files.
#' @param met_dir An optional file path for storage and access of meteorological
#'   data files.
#' @param binary_path An optional path to a HYSPLIT trajectory model binary.
#'   When not specified, the model binary will be chosen from several available
#'   in the package (based on the user's platform).
#' @examples
#' \dontrun{
#' library(lubridate)
#' 
#' # Run a trajectory model 4 times a day throughout
#' # 2004 using NCEP/NCAR reanalysis data
#' trajectory <-
#'   hysplit_trajectory(
#'     lat = 50.108,
#'     lon = -122.942,
#'     height = 100,
#'     duration = 48,
#'     days = seq(ymd("2012-02-22"), ymd("2012-02-27"), by = "1 day"),
#'     daily_hours = c(0, 6, 12, 18)
#'   )
#' }
#' @export
hysplit_trajectory <- function(lat = 49.263,
                               lon = -123.250,
                               height = 50,
                               duration = 24,
                               days = NULL,
                               daily_hours = 0,
                               direction = "forward",
                               met_type = "reanalysis",
                               vert_motion = 0,
                               model_height = 20000,
                               extended_met = FALSE,
                               return_traj_df = TRUE,
                               traj_name = NULL,
                               exec_dir = NULL,
                               met_dir = NULL,
                               binary_path = NULL) {
  
  if (is.null(exec_dir)) exec_dir <- getwd()
  
  if (is.null(met_dir)) met_dir <- getwd()
  
  binary_path <- set_binary_path(binary_path = binary_path)
  
  # Generate name of output folder
  if (is.null(traj_name)) {
    folder_name <- paste0("traj-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
  } else if (!is.null(traj_name)) {
    folder_name <- traj_name
  }
  
  # Write default versions of the SETUP.CFG and
  # ASCDATA.CFG files in the working directory
  hysplit_config_init(dir = exec_dir)
  
  # Modify the default `SETUP.CFG` file when the
  # option for extended meteorology is `TRUE`
  hysplit_config_extended_met(
    extended_met = extended_met,
    dir = exec_dir
  )
  
  # Stop function if there are vectors of different
  # length for `lat` and `lon`
  if (length(lat) != length(lon)) {
    stop("The coordinate vectors are not the same length.", call. = FALSE)
  }
  
  if (met_type == "gdas1") {
    
    # Get filenames for GDAS1 files
    met_files <-
      get_gdas1_filenames(
        days = days,
        duration = duration,
        direction = direction
      )
    
    # Get the GDAS1 meteorology files
    get_met_gdas1(
      files = met_files,
      path_met_files = met_dir
    )
  }
  
  if (met_type == "reanalysis") {
    
    # Get filenames for reanalysis files
    met_files <-
      get_reanalysis_filenames(
        days = days,
        duration = duration,
        direction = direction
      )
    
    get_met_reanalysis(
      files = met_files,
      path_met_files = met_dir
    )
  }
  
  if (met_type == "narr") {
    
    # Get filenames for NARR files
    met_files <-
      get_narr_filenames(
        days = days,
        duration = duration,
        direction = direction
      )
    
    get_met_narr(
      files = met_files,
      path_met_files = met_dir
    )
  }
  
  # Create a coordinates list
  coords <- list(lat = lat, lon = lon)
  
  # Create a dataframe for the ensemble
  ensemble_df <- data.frame()
  
  # For every set of coordinates, perform a set
  # of model runs
  for (z in 1:length(coords$lat)) {
    
    lat <- coords$lat[z]
    lon <- coords$lon[z]
    
    list_run_days <- days %>% as.character()
    
    # Initialize a vector for all filenames to be used
    all_trajectory_files <- vector(mode = "character", length = 0)
    
    # Make loop with all run days
    for (i in seq(list_run_days)) {
      
      # Define starting time parameters
      start_year_GMT <- to_short_year(list_run_days[i])
      start_month_GMT <- to_short_month(list_run_days[i])
      start_day_GMT <- to_short_day(list_run_days[i])
      
      # Sort daily starting hours if given as
      # numeric values
      if (inherits(daily_hours, "numeric")) {
        daily_hours <- formatC(sort(daily_hours), width = 2, flag = 0)
      }
      
      # Make nested loop with daily beginning hours
      for (j in daily_hours) {
        
        start_hour_GMT <- j
        
        if (start_year_GMT > 40) {
          full_year_GMT <- paste0("19", start_year_GMT)
        } else {
          full_year_GMT <- paste0("20", start_year_GMT)
        }
        
        # Construct the output filename string for this
        # model run
        output_filename <-
          get_traj_output_filename(
            traj_name = traj_name,
            site = z,
            direction = direction,
            year = start_year_GMT,
            month = start_month_GMT,
            day = start_day_GMT,
            hour = start_hour_GMT,
            lat = lat,
            lon = lon,
            height = height,
            duration = duration
          )
        
        all_trajectory_files <- c(all_trajectory_files, output_filename)
        
        if (any(c("mac", "unix") %in% get_os())) {
          
          # Write start year, month, day, hour to 'CONTROL'
          cat(start_year_GMT, " ", 
              start_month_GMT, " ",
              start_day_GMT, " ",
              start_hour_GMT, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = FALSE
          )
          
          # Write number of starting locations to 'CONTROL'
          cat("1\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
          
          # Write starting latitude, longitude, height AGL to 'CONTROL'
          cat(coords$lat[z], " ", 
              coords$lon[z], " ", 
              height, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
          
          # Write direction and number of simulation hours to 'CONTROL'
          cat(ifelse(direction == "backward", "-", ""),
              duration, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
          
          # Write vertical motion option to 'CONTROL'
          cat(vert_motion, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
          
          # Write top of model domain in meters to
          # 'CONTROL'
          cat(model_height, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
          
          # Write number of met files used to 'CONTROL'
          cat(length(met_files), "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
          
          # Write met file paths to 'CONTROL'
          for (i in 1:length(met_files)) {
            cat(met_dir, "/\n", met_files[i], "\n",
                file = paste0(exec_dir, "/CONTROL"),
                sep = "", append = TRUE
            )
          }
          
          # Write path for trajectory output files to 'CONTROL'
          cat(exec_dir, "/\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
          
          # Write name of output filename to 'CONTROL'
          cat(output_filename, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
        }
        
        if (get_os() == "win") {
          
          # Write start year, month, day, hour to 'CONTROL'
          cat(start_year_GMT, " ", 
              start_month_GMT, " ",
              start_day_GMT, " ",
              start_hour_GMT, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = FALSE
          )
          
          # Write number of starting locations to 'CONTROL'
          cat("1\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
          
          # Write starting latitude, longitude, height AGL to 'CONTROL'
          cat(coords$lat[z], " ", 
              coords$lon[z], " ", 
              height, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
          
          # Write direction and number of simulation hours to 'CONTROL'
          cat(ifelse(direction == "backward", "-", ""),
              duration, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
          
          # Write vertical motion option to 'CONTROL'
          cat(vert_motion, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
          
          # Write top of model domain in meters to
          # 'CONTROL'
          cat(model_height, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
          
          # Write number of met files used to 'CONTROL'
          cat(length(met_files), "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
          
          # Write met file paths to 'CONTROL'
          for (i in 1:length(met_files)) {
            cat(met_dir, "/\n", met_files[i], "\n",
                file = paste0(exec_dir, "/CONTROL"),
                sep = "", append = TRUE
            )
          }
          
          # Write path for trajectory output files to
          # 'CONTROL'
          cat(exec_dir, "/\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
          
          # Write name of output filename to 'CONTROL'
          cat(output_filename, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = "", append = TRUE
          )
        }
        
        # The CONTROL file is now complete and in the
        # working directory, so, execute the model run
        if (any(c("mac", "unix") %in% get_os())) {
          
          system(
            paste0(
              "(cd ", exec_dir, " && ",
              binary_path,
              " >> /dev/null 2>&1)"
            )
          )
        }
        
        if (get_os() == "win") {
          shell(
            paste0(
              "(cd \"", exec_dir, "\" && \"",
              binary_path,
              "\")"
            )
          )
        }
      }
    }
    
    # Create the output folder if it doesn't exist
    if (!dir.exists(paste0(exec_dir, "/", folder_name))) {
      
      dir.create(path = paste0(exec_dir, "/", folder_name))
    }
    
    if (any(c("mac", "unix") %in% get_os())) {
      
      # Perform the movement of all trajectory files
      # into a folder residing to the output directory
      for (i in 1:length(all_trajectory_files)) {
        
        system(
          paste0(
            "(cd ", exec_dir, " && mv '",
            all_trajectory_files[i], "' ",
            paste0(exec_dir, "/", folder_name),
            ")"
          )
        )
      }
    }
    
    if (get_os() == "win") {
      
      # Perform the movement of all trajectory files
      # into a folder residing to the output directory
      for (i in 1:length(all_trajectory_files)) {
        
        shell(
          paste0(
            "(cd \"", exec_dir, "\" && move \"",
            all_trajectory_files[i], "\" \"",
            paste0(exec_dir, "/", folder_name),
            "\")"
          )
        )
      }
    }
    
    # Obtain a trajectory data frame
    if (return_traj_df) {
      traj_df <-
        trajectory_read(
          output_folder = paste0(exec_dir, "/", folder_name)
        )
    }
    
    if (z == 1) {
      
      col_names <- colnames(traj_df)
      
      ensemble_df <-
        data.frame(
          mat.or.vec(
            nr = 0,
            nc = length(col_names)
          )
        )
      
      colnames(ensemble_df) <- col_names
    }
    
    traj_df[,1] <- z
    
    ensemble_df <- rbind(ensemble_df, traj_df)
  }
  
  ensemble_df
}
