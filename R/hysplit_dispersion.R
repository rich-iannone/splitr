#' Conduct HYSPLIT dispersion runs
#' 
#' The function executes single/multiple forward or backward HYSPLIT dispersion
#' runs using specified meteorological datasets.
#' 
#' @inheritParams hysplit_trajectory
#' @param start_day the day that the model will initialize and run. This should
#'   take the form of a single-length vector for a day (`"YYYY-MM-DD"`).
#' @param start_hour a single daily hour as an integer hour (from `0` to `23`).
#' @param particle_num The number of particles released by a source during each
#'   release cycle.
#' @param particle_max The maximum number of particles released by a source
#'   during a model run.
#' @param emissions,species,grids An ID corresponding to the stored emissions,
#'   species, and grids presets.
#' @param disp_name An optional, descriptive name for the output file
#'   collection.
#'   
#' @examples
#' \dontrun{
#' # Perform a dispersion run lasting 12 hours over
#' # two consecutive days, both starting at midnight;
#' # grid presets 1 and 2 will be used as sampling
#' # grids (presets for species and emissions are set
#' # to the first, which is the default)
#' hysplit_dispersion(
#'   lat = 49.263,
#'   lon = -123.250,
#'   height = 15,
#'   duration = 12,
#'   start_day = "2012-02-01",
#'   start_hour = 0,
#'   emissions = 1,
#'   species = 1,
#'   grids = c(1,2),
#'   disp_name = "example")
#' }
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
                               emissions,
                               species,
                               disp_name = NULL,
                               binary_path = NULL, 
                               exec_dir = NULL,
                               met_dir = NULL,
                               clean_up = TRUE) {
  
  # If the execution dir isn't specified, use the working directory
  if (is.null(exec_dir)) exec_dir <- getwd()
  
  # If the meteorology dir isn't specified, use the working directory
  if (is.null(met_dir)) met_dir <- getwd()
  
  # Set the path for the `hycs_std` binary file
  hycs_std_binary_path <- 
    set_binary_path(
      binary_path = binary_path,
      binary_name = "hycs_std"
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
  
  # CONTROL file is now complete and in the
  # working directory; execute the model run
  if (any(c("mac", "unix") %in% system_type)) {
    
    sys_cmd <- 
      paste0("(cd ", exec_dir, " && ", hycs_std_binary_path, " >> /dev/null 2>&1)")
    
    system(sys_cmd)
  }
  
  if (system_type == "win") {
    
    sys_cmd <-
      paste0("(cd \"", exec_dir, "\" && \"", hycs_std_binary_path, "\")")
    
    shell(sys_cmd)
  }
  
  # Extract the particle positions at every hour
  sys_cmd <- 
    paste0("(cd ", exec_dir, " && ", parhplot_binary_path, " -iPARDUMP -a1)")
  
  system(sys_cmd)

  # Remove the .att files from the working directory
  if (any(c("mac", "unix") %in% system_type)) {
    system(paste0("(cd ", exec_dir,
                  " && rm GIS_part*.att)"))
  }
  
  if (system_type == "win") {
    shell(paste0("(cd \"", exec_dir,
                 "\" && del GIS_part*.att)"))
  }
  
  # Remove the postscript plot from the working directory
  if (any(c("mac", "unix") %in% system_type)) {
    system(paste0("(cd ", exec_dir,
                  " && rm parhplot.ps)"))
  }
  
  if (system_type == "win") {
    shell(paste0("(cd \"", exec_dir,
                 "\" && del parhplot.ps)"))
  }
  
  # Rename the TXT files as CSV files
  if (any(c("mac", "unix") %in% system_type)) {
    system(
      paste0("(cd ", exec_dir,
             " && for files in GIS*.txt;",
             " do mv \"$files\" \"${files%.txt}.csv\"; done)"))
  }
  
  # Remove the 'END' string near the end of
  # each CSV file
  if (any(c("mac", "unix") %in% system_type)) {
    system(paste0("(cd ", exec_dir,
                  " && sed -i .bk 's/END//g'",
                  " GIS_part_*.csv; rm *.bk)"))
  }
  
  if (system_type == "win") {        
    temp_file_list <- 
      list.files(path = exec_dir,
                 pattern = "*._ps.txt",
                 full.names = TRUE)
    
    for (i in 1:length(temp_file_list)) {
      temp_lines <- readLines(temp_file_list[i])
      temp_lines <- temp_lines[-(length(temp_lines))]
      utils::write.table(temp_lines,
                         file = gsub("txt", "csv",
                                     temp_file_list[i]),
                         col.names = FALSE,
                         row.names = FALSE,
                         quote = FALSE)
    }
  }
  
  # Move the .csv files from the working directory
  # to the output folder
  if (any(c("mac", "unix") %in% system_type)) {
    
    if (is.null(disp_name)) {
      folder_name <- 
        paste0("disp--", 
               format(Sys.time(),
                      "%Y-%m-%d--%H-%M-%S"))  
    } else if (!is.null(disp_name)) {
      folder_name <- 
        paste0(disp_name, "--",
               format(Sys.time(),
                      "%Y-%m-%d--%H-%M-%S"))  
    }
    
    # Perform the movement of all dispersion files
    # into a folder residing in the output dir
    dir.create(path = file.path(exec_dir, folder_name))
    
    system(paste0("(cd ", exec_dir,
                  " && mv GIS_part*.csv '",
                  file.path(exec_dir, folder_name),
                  "')"))
  }
  
  if (system_type == "win") {
    
    if (is.null(disp_name)) {
      folder_name <- 
        paste0("disp--", 
               format(Sys.time(),
                      "%Y-%m-%d--%H-%M-%S"))  
    } else if (!is.null(disp_name)) {
      folder_name <- 
        paste0(disp_name, "--",
               format(Sys.time(),
                      "%Y-%m-%d--%H-%M-%S"))  
    }
    
    # Perform the movement of all dispersion files
    # into a folder residing in the output dir
    dir.create(path = file.path(exec_dir, folder_name))
    
    shell(paste0("(cd \"", getwd(),
                 "\" && move GIS_part*.csv \"",
                 file.path(exec_dir, folder_name),
                 "\")"))
  }
  
  disp_tbl <- 
    create_dispersion_tbl(dispersion_dir = file.path(exec_dir, folder_name))
  
  if (clean_up) {
    unlink(file.path(exec_dir, disp_output_files()), force = TRUE)
    unlink(file.path(exec_dir, folder_name), recursive = TRUE, force = TRUE)
  }
  
  disp_tbl
}
