#' Conduct HYSPLIT dispersion runs
#' @description The function executes single/multiple
#' forward or backward HYSPLIT dispersion runs using
#' specified meteorological datasets.
#' @param lat the starting latitude (in decimal
#' degrees) for the model run(s).
#' @param lon the starting longitude (in decimal
#' degrees) for the model run(s).
#' @param height the starting height (in meters above
#' ground level) for the model run(s).
#' @param duration the duration of each
#' model run (either forward or backward) in hours.
#' @param start_day the day that the model will 
#' initialize and run. This should take the form of a
#' single-length vector for a day (\code{"YYYY-MM-DD"}).
#' @param start_hour a single daily hour as an
#' integer hour (from \code{0} to \code{23}).
#' @param direction an option to select whether to
#' conduct the model in the \code{forward} or 
#' \code{backward} directions.
#' @param met_type an option to select meteorological
#' data files. The options are \code{gdas1} (Global Data
#' Assimilation System 1-degree resolution data) and
#' \code{reanalysis} (NCAR/NCEP global reanalysis data).
#' @param met_dir an optional file path for storage and
#' access of meteorological data files.
#' @param vert_motion a numbered option to
#' select the method used to simulation vertical
#' motion. The methods are: (0) input model data,
#' (1) isobaric, (2) isentropic, (3) constant density,
#' (4) isosigma, (5) from divergence, (6) remap MSL to
#' AGL, (7) average data, and (8) damped magnitude.
#' @param model_height the upper limit of the
#' model domain in meters.
#' @param particle_num the number of particles
#' released by source during each release cycle.
#' @param particle_max the number of particles
#' released by a source during a model run.
#' @param emissions the numbers corresponding to the
#' stored emissions presets. These presets are
#' specified using the function
#' \code{hysplit_dispersion_define("emissions")}.
#' @param species the numbers corresponding to the
#' stored species presets. These presets are specified
#' using the function
#' \code{hysplit_dispersion_define("species")}.
#' @param grids the numbers corresponding to the
#' stored grid presets. These presets are specified
#' using the function
#' \code{hysplit_dispersion_define("grids")}.
#' @param return_disp_df an option to return a data
#' frame with dispersion data.
#' @param write_disp_CSV an option to write disperison
#' data to a CSV file.
#' @param disp_name an optional, descriptive name for
#' the output file collection.
#' @import lubridate
#' @import ggmap
#' @export hysplit_dispersion 
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
#'   run_type = "range",
#'   start_day = "2012-02-01",
#'   start_hour = 0,
#'   emissions = 1,
#'   species = 1,
#'   grids = c(1,2),
#'   return_disp_df = FALSE,
#'   disp_name = "example")
#'}

hysplit_dispersion <- function(lat = 49.263,
                               lon = -123.250,
                               height = 50,
                               duration = 24,
                               start_day = "2015-07-01",
                               start_hour = 0,
                               direction = "forward",
                               met_type = "reanalysis",
                               met_dir = NULL,
                               vert_motion = 0,
                               model_height = 20000,
                               particle_num = 2500,
                               particle_max = 10000,
                               emissions,
                               species,
                               grids,
                               return_disp_df = TRUE,
                               write_disp_CSV = TRUE,
                               disp_name = NULL) { 
  
  if (is.null(met_dir)) met_dir <- getwd()
  
  if (length(start_day) == 1 &
      class(start_day) == "character" &
      all(grepl("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]",
                start_day))) {
    
    run_type <- "day"
    run_day <- start_day
  }
  
  # If SETUP.CFG or ASCDATA.CFG do not exist in the 
  # working directory, write default versions of those
  # config files
  if (!("SETUP.CFG" %in% list.files()) |
      !("ASCDATA.CFG" %in% list.files())) {
    hysplit_config_init(getwd())
  }
  
  # Set number of particles to 1 in the SETUP.CFG file
  setup.cfg <- readLines(paste0(getwd(), "/SETUP.CFG"))
  
  setup.cfg <- gsub(" numpar = ([0-9]*),",
                    paste0(" numpar = ",
                           particle_num,
                           ","),
                    setup.cfg)
  
  setup.cfg <- gsub(" maxpar = ([0-9]*),",
                    paste0(" maxpar = ",
                           particle_max,
                           ","),
                    setup.cfg)
  
  writeLines(setup.cfg, paste0(getwd(), "/SETUP.CFG"))
  
  rm(setup.cfg)
  
  # Make a vector list of run days in POSIXct format
  run_day <- 
    as.POSIXct(run_day,
               origin = "1970-01-01",
               tz = "UTC")
  
  # Define starting time parameters
  start_year_GMT <- 
    substr(
      as.character(year(run_day)), 3, 4)
  
  start_month_GMT <- 
    formatC(as.numeric(month(run_day)),
            width = 2, format = "d", flag = "0")
  
  start_day_GMT <- 
    formatC(as.numeric(day(run_day)),
            width = 2, format = "d", flag = "0")
  
  # Format `start_hour` if given as a numeric value
  if (class(start_hour) == "numeric") {
    start_hour <-
      formatC(sort(start_hour),
              width = 2,
              flag = 0)
  }
  
  # Determine the start time of the model run
  start_time_GMT <- 
    ymd_hms(paste0(ifelse(start_year_GMT > 50,
                          paste0("19",
                                 start_year_GMT),
                          start_year_GMT), "-",
                   start_month_GMT, "-",
                   start_day_GMT, " ",
                   start_hour, ":00:00"))
  
  # Determine the end time of the model run
  end_time_GMT <- 
    as.POSIXct(
      ifelse(direction == "backward", 
             start_time_GMT -
               (duration * 3600),
             start_time_GMT +
               (duration * 3600)),
      origin = "1970-01-01",
      tz = "UTC")
  
  # Determine whether the start year is a leap year
  leap_year <- 
    leap_year(ymd(paste0(start_year_GMT, "-",
                         start_month_GMT, "-",
                         start_day_GMT)))
  
  # Determine whether the beginning and end of the
  # current run crosses over a calendar year
  number_of_calendar_years <- 
    ifelse(year(start_time_GMT) ==
             year(end_time_GMT), 1, 2)
  
  # Determine whether the beginning and end of
  # the current run crosses over a calendar month
  number_of_calendar_months <- 
    ifelse(month(start_time_GMT) ==
             month(end_time_GMT), 1, 2)
  
  #--- Divide different requirements for met files
  #    into different cases
  
  # Set the different cases to FALSE by default
  case_within_month <- FALSE
  case_over_year <- FALSE
  case_over_month <- FALSE
  
  # Determine which of the three cases is true
  if (number_of_calendar_years == 1 &
      number_of_calendar_months == 1) {
    case_within_month <- TRUE
  } else if (number_of_calendar_years > 1) {
    case_over_year <- TRUE
  } else if (number_of_calendar_months > 1) {
    case_over_month <- TRUE
  } else { NULL }
  
  #--- Get vector lists of met files applicable to
  #    run from GDAS 1-degree dataset
  
  # Trap leap-year condition of missing .w5 met
  # file for February in a '0' list value
  if (case_within_month &
      met_type == "gdas1") met <- 
    c(paste0(
      "gdas1.",
      substr(tolower(format(start_time_GMT,
                            "%B")), 1, 3),
      substr(year(start_time_GMT), 3, 4), ".w1"),                                      
      paste0(
        "gdas1.",
        substr(tolower(format(start_time_GMT,
                              "%B")), 1, 3),
        substr(year(start_time_GMT), 3, 4), ".w2"),
      paste0(
        "gdas1.",
        substr(tolower(format(start_time_GMT,
                              "%B")), 1, 3),
        substr(year(start_time_GMT), 3, 4), ".w3"),
      paste0(
        "gdas1.",
        substr(tolower(format(start_time_GMT,
                              "%B")), 1, 3),
        substr(year(start_time_GMT), 3, 4), ".w4"),
      ifelse(
        month(start_time_GMT) == 2 &
          leap_year, 0,
        paste0("gdas1.",
               substr(tolower(format(start_time_GMT,
                                     "%B")), 1, 3),
               substr(year(start_time_GMT), 3, 4),
               ".w5")))
  
  if (case_over_year &
      met_type == "gdas1") met <- 
    c(paste0(
      "gdas1.dec",
      substr(year(end_time_GMT), 3, 4), ".w3"),                                      
      paste0(
        "gdas1.dec",
        substr(year(end_time_GMT), 3, 4), ".w4"),
      paste0(
        "gdas1.dec",
        substr(year(end_time_GMT), 3, 4), ".w5"),
      paste0(
        "gdas1.jan",
        substr(year(start_time_GMT), 3, 4), ".w1"),
      paste0(
        "gdas1.jan",
        substr(year(start_time_GMT), 3, 4), ".w2"),
      paste0(
        "gdas1.jan",
        substr(year(start_time_GMT), 3, 4), ".w3"))
  
  if (case_over_month == TRUE &
      met_type == "gdas1") met <-
    c(paste0(
      "gdas1.",
      substr(tolower(format(end_time_GMT, 
                            "%B")), 1, 3),
      substr(year(end_time_GMT), 3, 4), ".w3"),                                      
      paste0(
        "gdas1.",
        substr(tolower(format(end_time_GMT, 
                              "%B")), 1, 3),
        substr(year(end_time_GMT), 3, 4), ".w4"),
      ifelse(
        month(end_time_GMT) == 2 &
          leap_year == TRUE, 0,
        paste0(
          "gdas1.",
          substr(tolower(format(end_time_GMT, 
                                "%B")), 1, 3),
          substr(year(end_time_GMT), 3, 4), ".w5")),
      paste0(
        "gdas1.",
        substr(tolower(format(start_time_GMT, 
                              "%B")), 1, 3),
        substr(year(start_time_GMT), 3, 4), ".w1"),
      paste0(
        "gdas1.",
        substr(tolower(format(start_time_GMT, 
                              "%B")), 1, 3),
        substr(year(start_time_GMT), 3, 4), ".w2"),
      paste0(
        "gdas1.",
        substr(tolower(format(start_time_GMT, 
                              "%B")), 1, 3),
        substr(year(start_time_GMT), 3, 4), ".w3"))
  
  # Get vector lists of met files applicable to run
  # from the NCEP/NCAR reanalysis dataset
  if (met_type == "reanalysis") met <- 
    c(paste0(
      "RP",
      ifelse(start_month_GMT == "01",
             year(start_time_GMT) - 1,
             year(start_time_GMT)),
      ifelse(start_month_GMT == "01", "12",
             formatC(month(start_time_GMT) - 1,
                     width = 2, 
                     format = "d", 
                     flag = "0")),
      ".gbl"),
      paste0(
        "RP",
        year(start_time_GMT),
        start_month_GMT, ".gbl"),
      paste0(
        "RP",
        ifelse(start_month_GMT == "12",
               year(start_time_GMT) + 1,
               year(start_time_GMT)),
        ifelse(start_month_GMT == "12", "01",
               formatC(month(start_time_GMT) + 1,
                       width = 2, 
                       format = "d", 
                       flag = "0")),
        ".gbl"))
  
  # Remove list values containing '0' (representing
  # missing .w5 data files for Feb in leap years)
  if(exists("met")) met <- met[!met %in% c(0)]
  
  # Are the met files available in the
  # selected path?
  met_file_df <- 
    setNames(data.frame(mat.or.vec(nr = length(met),
                                   nc = 2)),
             nm = c("file", "available"))
  
  if (any(c("mac", "unix") %in% get_os())) {
    
    for (k in 1:length(met)) {
      met_file_df[k, 1] <- met[k]
      met_file_df[k, 2] <-
        as.character(
          file.exists(paste0(met_dir, "/",
                             met[k])))
    }
    
    # Write the met file availability to file
    write.table(
      met_file_df,
      file = paste0(met_dir, "/", "met_file_list"),
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      append = FALSE)
    
    # Download the missing met files
    if (FALSE %in% met_file_df[,2]) {
      
      files_to_get <-
        subset(met_file_df,
               available == FALSE)[,1]
      
      if (met_type == "reanalysis") {
        get_met_reanalysis(
          files = files_to_get,
          path_met_files = paste0(met_dir, "/"))
      }
      
      if (met_type == "gdas1") {
        get_met_gdas1(
          files = files_to_get,
          path_met_files = paste0(met_dir, "/"))
      } 
    }
  }
  
  if (get_os() == "win") {
    
    for (k in 1:length(met)) {
      met_file_df[k, 1] <- met[k]
      met_file_df[k, 2] <-
        as.character(
          file.exists(paste0(met_dir, "\\",
                             met[k])))}
    
    # Write the met file availability to file
    write.table(met_file_df,
                file = paste0(met_dir, "\\",
                              "met_file_list"),
                sep = ",",
                row.names = FALSE,
                col.names = FALSE,
                quote = FALSE,
                append = FALSE)
    
    # Download the missing met files
    if (FALSE %in% met_file_df[,2]) {
      
      files_to_get <-
        subset(met_file_df, available == FALSE)[,1]
      
      if (met_type == "reanalysis") {
        get_met_reanalysis(
          files = files_to_get,
          path_met_files = met_dir)
      }
      
      if (met_type == "gdas1") {
        get_met_gdas1(
          files = files_to_get,
          path_met_files = met_dir)
      } 
    }
  }
  
  # Construct the output filename string
  # for this model run
  output_filename <- 
    paste0("--disp",
           ifelse(direction == "backward",
                  "(back)", "(forward)"), "-",
           start_year_GMT, "-",
           start_month_GMT, "-",
           start_day_GMT, "-",
           start_hour, "-",
           "lat_", lat, "_",
           "long_",lon, "-",
           "height_",height, "-",
           duration, "h")
  
  # Write start year, month, day, hour to 'CONTROL'
  cat(start_year_GMT, " ", 
      start_month_GMT, " ",
      start_day_GMT, " ",
      start_hour, "\n",
      file = paste0(getwd(), "/", "CONTROL"),
      sep = '', append = FALSE)
  
  #Write number of starting locations to 'CONTROL'
  cat("1\n",
      file = paste0(getwd(), "/", "CONTROL"),
      sep = '', append = TRUE)
  
  # Write starting latitude, longitude, and height
  # AGL to 'CONTROL'
  cat(lat, " ", 
      lon, " ", 
      height, "\n",
      file = paste0(getwd(), "/", "CONTROL"),
      sep = '', append = TRUE)
  
  # Write direction and number of simulation hours
  # to 'CONTROL'
  cat(ifelse(direction == "backward", "-", ""),
      duration, "\n",
      file = paste0(getwd(), "/", "CONTROL"),
      sep = '', append = TRUE)
  
  # Write vertical motion option to 'CONTROL'
  cat(vert_motion, "\n",
      file = paste0(getwd(), "/", "CONTROL"),
      sep = '', append = TRUE)
  
  # Write top of model domain in meters to 'CONTROL'
  cat(model_height, "\n",
      file = paste0(getwd(), "/", "CONTROL"),
      sep = '', append = TRUE)
  
  # Write number of met files used to 'CONTROL'
  cat(length(met), "\n",
      file = paste0(getwd(), "/", "CONTROL"),
      sep = '', append = TRUE)
  
  # Write met file paths to 'CONTROL'
  for (i in 1:length(met)) {
    cat(met_dir, "/\n", met[i], "\n",
        file = paste0(getwd(), "/", "CONTROL"),
        sep = '', append = TRUE)
  }
  
  # Write emissions blocks to 'CONTROL'
  for (i in 1:nrow(emissions)) {
    cat(c(nrow(emissions), "\n",
          substr(emissions[i, 1], 1, 4), "\n",
          emissions[i, 2], "\n",
          emissions[i, 3], "\n",
          paste0(paste(unlist(strsplit(substr(emissions[i, 4], 3, 10), "-")),
                       collapse = " "),
                 " ",
                 formatC(emissions[i, 5],
                         width = 2, 
                         format = "d", 
                         flag = "0"),
                 " 00")), "\n",
        file = paste0(getwd(), "/", "CONTROL"),
        sep = "", append = TRUE)
  }
  
  # Get vector text elements through reading
  # selected elements from the 'grids' data frame
  if (any(is.na(grids$lat),
          is.na(grids$lon))) {
    
    grids$lat <- lat
    grids$lon <- lon
  }
  
  if (any(is.na(grids$duration),
          is.na(grids$start_day),
          is.na(grids$start_hour),
          is.na(grids$end_day),
          is.na(grids$end_hour))) {
    
    grids$duration <- duration
    grids$start_day <- start_day
    grids$start_hour <- start_hour
    
    grids$end_day <-
      format(ymd_h(paste(grids$start_day, grids$start_hour)) + 
               (duration * 3600),
             "%Y-%m-%d")
    
    grids$end_hour <-
      as.numeric(
        format(ymd_h(paste(grids$start_day, grids$start_hour)) + 
                 (duration * 3600),
               "%H")
      )
  }
  
  if (grids[1, 14] == "avg") {
    sampling_type <- "0"
  } else if (grids[1, 14] == "snapshot") {
    sampling_type <- "1"
  } else if (grids[1, 14] == "max") {
    sampling_type <- "2"
  } else {
    sampling_type <- "0"
  }
  
  grids_text <-
    c("1",
      paste(grids[1, 2],
            grids[1, 3]),
      paste(grids[1, 6],
            grids[1, 7]),
      paste(grids[1, 4],
            grids[1, 5]),
      paste0(getwd(), "/"),
      grids[1,1],
      "1", "50",
      paste0(paste(unlist(strsplit(substr(grids[i, 9], 3, 10), "-")),
                   collapse = " "),
             " ",
             formatC(grids[1, 10],
                     width = 2, 
                     format = "d", 
                     flag = "0"),
             " 00"),
      paste0(paste(unlist(strsplit(substr(grids[i, 11], 3, 10), "-")),
                   collapse = " "),
             " ",
             formatC(grids[1, 12],
                     width = 2, 
                     format = "d", 
                     flag = "0"),
             " 00"),
      paste0(sampling_type, " ",
             formatC(grids[1, 15],
                     width = 2, 
                     format = "d", 
                     flag = "0"),
             " 00"))
  
  # Get vector text indices that contain the short
  # name(s) of the grid(s)
  gridnames_indices <-
    seq(from = 1 + 5,
        to = length(grids_text) - 5,
        by = 10)
  
  # Combine short grid name string with longer
  # 'output_filename' string
  for (i in 1:((length(grids_text) - 1)/10)) {
    grids_text[gridnames_indices[i]] <-
      paste0(grids_text[gridnames_indices[i]],
             output_filename)
  }
  
  # Write grid blocks to 'CONTROL'
  for (i in 1:length(grids_text)) {
    cat(grids_text[i], "\n",
        file = paste0(getwd(), "/", "CONTROL"),
        sep = '', append = TRUE)
  }
  
  # Write species blocks to 'CONTROL'
  for (i in 1:nrow(species)) {
    cat(c(nrow(species), "\n",
          paste(species[1, 2],
                species[1, 3],
                species[1, 4]), "\n",
          paste(species[1, 5],
                species[1, 6],
                species[1, 7],
                species[1, 8],
                species[1, 9]), "\n",
          paste(species[1, 10],
                species[1, 11],
                species[1, 12]), "\n",
          species[1, 13], "\n",
          species[1, 14]), "\n",
        file = paste0(getwd(), "/", "CONTROL"),
        sep = "", append = TRUE)
  }
  
  # CONTROL file is now complete and in the
  # working directory; execute the model run
  if (get_os() == "mac") {
    system(paste0("(cd ", getwd(), " && ",
                  system.file("osx/hycs_std",
                              package = "SplitR"),
                  " >> /dev/null 2>&1)"))
  }
  
  if (get_os() == "unix") {
    system(paste0("(cd ", getwd(), " && ",
                  system.file("linux-amd64/hycs_std",
                              package = "SplitR"),
                  " >> /dev/null 2>&1)"))
  }
  
  if (get_os() == "win") {
    shell(paste0("(cd \"", getwd(), "\" && \"",
                 system.file("win/hycs_std.exe",
                             package = "SplitR"),
                 "\")"))
  }
  
  # Extract the particle positions at every hour
  if (get_os() == "mac") {
    system(paste0("(cd ", getwd(), "/", " && ",
                  system.file("osx/parhplot",
                              package = "SplitR"),
                  " -iPARDUMP -a1)"))
  }
  
  if (get_os() == "unix") {
    system(paste0("(cd ", getwd(), "/", " && ",
                  system.file("linux-amd64/parhplot",
                              package = "SplitR"),
                  " -iPARDUMP -a1)"))
  }
  
  if (get_os() == "win") {
    shell(paste0("(cd \"", getwd(), "\" && \"",
                 system.file("win/parhplot.exe",
                             package = "SplitR"),
                 "\" -iPARDUMP -a1)"))
  }
  
  # Remove the .att files from the working directory
  if (any(c("mac", "unix") %in% get_os())) {
    system(paste0("(cd ", getwd(),
                  " && rm GIS_part*.att)"))
  }
  
  if (get_os() == "win") {
    shell(paste0("(cd \"", getwd(),
                 "\" && del GIS_part*.att)"))
  }
  
  # Remove the postscript plot from the working directory
  if (any(c("mac", "unix") %in% get_os())) {
    system(paste0("(cd ", getwd(),
                  " && rm parhplot.ps)"))
  }
  
  if (get_os() == "win") {
    shell(paste0("(cd \"", getwd(),
                 "\" && del parhplot.ps)"))
  }
  
  # Rename the TXT files as CSV files
  if (any(c("mac", "unix") %in% get_os())) {
    system(
      paste0("(cd ", getwd(),
             " && for files in GIS*.txt;",
             " do mv \"$files\" \"${files%.txt}.csv\"; done)"))
  }
  
  # Remove the 'END' string near the end of
  # each CSV file
  if (any(c("mac", "unix") %in% get_os())) {
    system(paste0("(cd ", getwd(),
                  " && sed -i .bk 's/END//g'",
                  " GIS_part_*.csv; rm *.bk)"))
  }
  
  if (get_os() == "win") {        
    temp_file_list <- 
      list.files(path = getwd(),
                 pattern = "*._ps.txt",
                 full.names = TRUE)
    
    for (i in 1:length(temp_file_list)) {
      temp_lines <- readLines(temp_file_list[i])
      temp_lines <- temp_lines[-(length(temp_lines))]
      write.table(temp_lines,
                  file = gsub("txt", "csv",
                              temp_file_list[i]),
                  col.names = FALSE,
                  row.names = FALSE,
                  quote = FALSE)
    }
  }
  
  # Move the .csv files from the working directory
  # to the output folder
  if (any(c("mac", "unix") %in% get_os())) {
    
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
    dir.create(path = paste0(getwd(), "/",
                             folder_name))
    
    system(paste0("(cd ", getwd(),
                  " && mv GIS_part*.csv '",
                  getwd(), "/",
                  folder_name,
                  "')"))
  }
  
  if (get_os() == "win") {
    
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
    dir.create(path = paste0(getwd(), "/",
                             folder_name))
    
    shell(paste0("(cd \"", getwd(),
                 "\" && move GIS_part*.csv \"",
                 getwd(), "/",
                 folder_name,
                 "\")"))
  }
  
  # Write the dispersion data frame to a CSV if
  # it is requested
  if (write_disp_CSV) {
    disp_df <- 
      dispersion_read(archive_folder =
                        paste0(getwd(), "/",
                               folder_name))
    
    if (any(c("mac", "unix") %in% get_os())) {
      write.table(
        disp_df,
        file = paste0(getwd(), "/",
                      folder_name,
                      "/dispersion.csv"),
        sep = ",",
        row.names = FALSE)
    }
    
    if (get_os() == "win") {
      write.table(
        disp_df,
        file = paste0("\"", getwd(), "/",
                      folder_name,
                      "/dispersion.csv\""),
        sep = ",",
        row.names = FALSE) 
    }
  }
  
  # Return a dispersion data frame if it is requested
  if (return_disp_df) {
    
    disp_df <- 
      dispersion_read(archive_folder = folder_name)
    
    invisible(disp_df)
  }
}
