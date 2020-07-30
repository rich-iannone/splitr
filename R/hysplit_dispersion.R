#' Conduct HYSPLIT dispersion runs
#' 
#' The function executes single/multiple forward or backward HYSPLIT dispersion
#' runs using specified meteorological datasets.
#' 
#' @inheritParams hysplit_trajectory
#' @param binary_name An optional file name for the hysplit binary code. Defaults to "hycs_std", but could be "hycm_std", for example.
#' @param start_day the day that the model will initialize and run. This should
#'   take the form of a single-length vector for a day (`"YYYY-MM-DD"`).
#' @param start_hour a single daily hour as an integer hour (from `0` to `23`).
#' @param particle_num The number of particles released by a source during each
#'   release cycle.
#' @param particle_max The maximum number of particles released by a source
#'   during a model run.
#' @param species A list of parameters corresponding to an emissions source..
#' @param disp_name An optional, descriptive name for the output file
#'   collection.
#'   
#' @export
hysplit_dispersion <- function(lat = 49.263,
                               lon = -123.250,
                               height = 50,
                               start_day = "2015-07-01",
                               start_hour = 0,
                               duration = 24,
                               direction = "forward",
                               met_type = "reanalysis",
                               vert_motion = 0,
                               model_height = 20000,
                               particle_num = 2500,
                               particle_max = 10000,
                               species,
                               disp_name = NULL,
                               binary_path = NULL, 
                               binary_name = NULL,
                               exec_dir = NULL,
                               met_dir = NULL,
                               softrun = NULL,
                               clean_up = TRUE) {
  
  # If the execution dir isn't specified, use the working directory
  if (is.null(exec_dir)) exec_dir <- getwd()
  
  # If the meteorology dir isn't specified, use the working directory
  if (is.null(met_dir)) met_dir <- getwd()
  
  # Set the path for the binary file. Defaults to "hycs_std"
  if (is.null(binary_name)) binary_name <- "hycs_std"
  
  hycs_std_binary_path <- 
    set_binary_path(
      binary_path = binary_path,
      binary_name = binary_name
    )
  
  parhplot_binary_path <-
    set_binary_path(
      binary_path = binary_path,
      binary_name = "parhplot"
    )
  
  # Get the system type
  system_type <- get_os()
  
  # Ensure that there aren't issues with the `start_day` value
  check_start_day(start_day)
  
  # Stop function if there are vectors of different
  # length for `lat` and `lon`
  if (length(lat) != length(lon)) {
    stop("The coordinate vectors are not the same length.", call. = FALSE)
  }
  
  # Convert `start_day` to a `Date` object
  start_day <- lubridate::as_date(start_day)
  
  # Download any necessary meteorological data files
  # and return a vector of the all files required
  met_files <- 
    download_met_files(
      met_type = met_type,
      days = start_day,
      duration = duration,
      direction = direction,
      met_dir = met_dir
    )
  
  recep_file_path_stack <- c()
  
  # Write default versions of the SETUP.CFG and
  # ASCDATA.CFG files in the working directory
  hysplit_config_init(dir = exec_dir)
  
  # Modify numbers of particles in the SETUP.CFG file
  readLines(con = file.path(exec_dir, "SETUP.CFG")) %>%
    tidy_gsub(
      pattern = " numpar = ([0-9]*),",
      replacement = paste0(" numpar = ", particle_num, ",")
    ) %>%
    tidy_gsub(
      pattern = " maxpar = ([0-9]*),",
      replacement = paste0(" maxpar = ", particle_max, ",")
    ) %>%
    writeLines(con = file.path(exec_dir, "SETUP.CFG"))
  
  # Define starting time parameters
  start_year_GMT <- to_short_year(start_day)
  start_month_GMT <- to_short_month(start_day)
  start_day_GMT <- to_short_day(start_day)
  
  # Format `start_hour` if given as a numeric value
  if (inherits(start_hour, "numeric")) {
    start_hour <- formatC(sort(start_hour), width = 2, flag = 0)
  }
  
  if (start_year_GMT > 40) {
    full_year_GMT <- paste0("19", start_year_GMT)
  } else {
    full_year_GMT <- paste0("20", start_year_GMT)
  }
  
  # Construct the output filename string for this
  # model run
  output_filename <-
    get_disp_output_filename(
      disp_name = disp_name,
      direction = direction,
      year = start_year_GMT,
      month = start_month_GMT,
      day = start_day_GMT,
      hour = start_hour,
      lat = lat,
      lon = lon,
      height = height,
      duration = duration
    )
  
  write_disp_control_file(
    start_day = start_day,
    start_year_GMT = start_year_GMT,
    start_month_GMT = start_month_GMT,
    start_day_GMT = start_day_GMT,
    start_hour = start_hour,
    lat = lat,
    lon = lon,
    height = height,
    direction = direction,
    duration = duration,
    vert_motion = vert_motion,
    model_height = model_height,
    met_files = met_files,
    species = species,
    output_filename = output_filename,
    system_type = system_type,
    met_dir = met_dir,
    exec_dir = exec_dir
  )
  
  
  # The CONTROL file is now complete and in the
  # working directory, so, execute the model run
  sys_cmd <- 
    paste0(
      "(cd \"",
      exec_dir,
      "\" && \"",
      hycs_std_binary_path,
      "\" ",
      to_null_dev(system_type = system_type),
      ")"
    )
  
  if (isFALSE(softrun)) {
    execute_on_system(sys_cmd, system_type = system_type)
  }
  
  # Extract the particle positions at every hour
  sys_cmd <- 
    paste0(
      "(cd \"",
      exec_dir,
      "\" && \"",
      parhplot_binary_path,
      "\" -iPARDUMP -a1 ",
      to_null_dev(system_type = system_type),
      ")"
    )
  
  if (isFALSE(softrun)) {
    execute_on_system(sys_cmd, system_type = system_type)
  }
  
  if (isFALSE(softrun)) {
  dispersion_file_list <-
    list.files(
      path = exec_dir,
      pattern = "^GIS_part_[0-9][0-9][0-9]_ps.txt",
      full.names = TRUE
    )
  
  dispersion_tbl <-
    dplyr::tibble(
      particle_i = character(0),
      hour = integer(0),
      lat = numeric(0),
      lon = numeric(0),
      height = numeric(0)
    )
  
  for (file in dispersion_file_list) {
    
    hour_index <- 
      file %>%
      tidy_gsub(".*GIS_part_([0-9][0-9][0-9])_ps.*", "\\1") %>%
      as.integer()
    
    disp_tbl <-
      readr::read_csv(
        file,
        col_names = c("particle_i", "lon", "lat", "height"),
        col_types = "cddd",
        comment = "END"
      ) %>%
      dplyr::mutate(hour = hour_index) %>%
      dplyr::select(particle_i, hour, lat, lon, height)
    
    dispersion_tbl <-
      dplyr::bind_rows(dispersion_tbl, disp_tbl)
    }
  }
  
  if (clean_up) {
    unlink(file.path(exec_dir, list.files(path = exec_dir, pattern = "^.*$")), force = TRUE)
  }
  
  dispersion_tbl
}
