
#' Download files using consistent options
#' @param urls One or more URLs from which the files reside.
#' @param local_path The path to which the files should be written.
#'
#' @noRd
met_download <- function(urls,
                         local_path) {

  for (url in urls) {

    download(
      url = url,
      destfile = local_path,
      method = "auto",
      quiet = TRUE,
      mode = "wb",
      cacheOK = FALSE
    )
  }
}

#' Read a `listing` file
#' @param file_path The path to the `listing` file.
#'
#' @noRd
read_listing_file <- function(file_path) {

  as.vector(
    utils::read.table(
      file = file_path,
      sep = "\n"
    )[, 1]
  )
}

#' Create default `SETUP.CFG` and `ASCDATA.CFG` files
#' @param dir The directory to which the files should be written.
#'
#' @noRd
hysplit_config_init <- function(dir) {

  # Default `SETUP.CFG` configuration file
  cat(
    "&SETUP",
    "tratio = 0.75,",
    "initd = 0,",
    "kpuff = 0,",
    "khmax = 9999,",
    "kmixd = 0,",
    "kmix0 = 250,",
    "kzmix = 0,",
    "kdef = 0,",
    "kbls = 1,",
    "kblt = 2,",
    "conage = 48,",
    "numpar = 2500,",
    "qcycle = 0.0,",
    "efile = '',",
    "tkerd = 0.18,",
    "tkern = 0.18,",
    "ninit = 1,",
    "ndump = 1,",
    "ncycl = 1,",
    "pinpf = 'PARINIT',",
    "poutf = 'PARDUMP',",
    "mgmin = 10,",
    "kmsl = 0,",
    "maxpar = 10000,",
    "cpack = 1,",
    "cmass = 0,",
    "dxf = 1.0,",
    "dyf = 1.0,",
    "dzf = 0.01,",
    "ichem = 0,",
    "maxdim = 1,",
    "kspl = 1,",
    "krnd = 6,",
    "frhs = 1.0,",
    "frvs = 0.01,",
    "frts = 0.10,",
    "frhmax = 3.0,",
    "splitf = 1.0,",
    "tm_pres = 0,",
    "tm_tpot = 0,",
    "tm_tamb = 0,",
    "tm_rain = 0,",
    "tm_mixd = 0,",
    "tm_relh = 0,",
    "tm_sphu = 0,",
    "tm_mixr = 0,",
    "tm_dswf = 0,",
    "tm_terr = 0,",
    "/",
    sep = "\n",
    file = paste0(dir, "/", "SETUP.CFG")
  )

  # Default `ASCDATA.CFG` file
  cat(
    "-90.0  -180.0  lat/lon of lower left corner (last record in file)",
    "1.0  1.0	lat/lon spacing in degrees between data points",
    "180  360	lat/lon number of data points",
    "2  	default land use category",
    "0.2  	default roughness length (meters)",
    "'.'  directory location of data files",
    sep = "\n",
    file = paste0(dir, "/", "ASCDATA.CFG")
  )
}

# #' Modify default `SETUP.CFG` for extended meteorology
# #' @param extended_met An option to report additional meteorological data along
# #'   each output trajectory.
# #' @param dir The directory to which the files should be written.
# #'
# #' @noRd
# hysplit_config_extended_met <- function(extended_met,
#                                         exec_dir) {
#
#   if (extended_met) {
#     setup_cfg <- readLines(con = file.path(exec_dir, "SETUP.CFG"))
#     setup_cfg <- gsub("(tm_.* )(0),", "\\11,", setup_cfg)
#     cat(setup_cfg, file = file.path(exec_dir, "SETUP.CFG"), sep = "\n")
#   }
# }

#' Determine which operating system is in use
#'
#' @noRd
get_os <- function() {
  if (.Platform$OS.type == "windows") {
    return("win")
  } else if (Sys.info()["sysname"] == "Darwin") {
    return("mac")
  } else if (.Platform$OS.type == "unix") {
    return("unix")
  } else {
    stop("Unknown OS", call. = FALSE)
  }
}

#' Determine whether 64-bit architecture is present
#'
#' @noRd
is_64bit_system <- function() {
  ifelse(.Machine$sizeof.pointer == 8, TRUE, FALSE)
}

#' Obtain the redirect to null device command-line text based on system
#'
#' @noRd
to_null_dev <- function(system_type) {

  if (system_type %in% c("mac", "unix")) {
    null_dev <- ">> /dev/null 2>&1"
  } else if (system_type == "win") {
    null_dev <- "> NUL 2>&1"
  }

  null_dev
}

#' Execute a system command appropriate on the system type
#'
#' @noRd
execute_on_system <- function(sys_cmd, system_type) {

  if (system_type %in% c("mac", "unix")) {
    system(sys_cmd)
  } else if (system_type == "win") {
    shell(sys_cmd)
  }
}

#' Upgrade the `binary_path` variable
#'
#' @noRd
set_binary_path <- function(binary_path,
                            binary_name) {

  # By default, binary names should be either:
  #  - hyts_std (trajectory models)
  #  - hycs_std (dispersion models)

  # If a user uses another binary name, the path to it should also be specified
  
  if (is.null(binary_path)) {

    system_os <- get_os()

    if (system_os == "mac") {
      binary_path <-
        system.file(
          file.path("osx", binary_name),
          package = "splitr"
          )
    }

    if (system_os == "unix") {
      binary_path <-
        system.file(
          file.path("linux-amd64", binary_name),
          package = "splitr"
        )
    }

    if (system_os == "win") {
      binary_path <-
        system.file(
          file.path("win", paste0(binary_name, ".exe")),
          package = "splitr"
        )
    }
  } else {
    binary_path <- paste0(binary_path, binary_name)
  }

  binary_path
}

#' Create a file list for output files
#' @param output_folder The directory where the file list is to be written.
#'
#' @noRd
create_file_list <- function(output_folder,
                             create_file = TRUE,
                             file_name = "file_list.txt") {

  # List files from the specified archive folder
  file_list <-
    list.files(
      path = output_folder,
      pattern = "traj.*"
    )

  # Create file list in the output folder
  cat(
    file_list,
    file = paste0(output_folder, "/", file_name),
    sep = '\n',
    append = FALSE
  )

  file_list
}

to_short_year <- function(date) {

  date %>%
    lubridate::year() %>%
    as.character() %>%
    substr(3, 4)
}

to_short_month <- function(date) {

  formatC(
    date %>% lubridate::month(),
    width = 2, flag = "0"
  )
}

to_short_day <- function(date) {

  formatC(
    date %>% lubridate::day(),
    width = 2, flag = "0"
  )
}


get_monthly_filenames <- function(days,
                                  duration,
                                  direction,
                                  prefix = NULL,
                                  extension = NULL) {

  # Determine the minimum month (as a `Date`) for the model run
  if (direction == "backward") {
    min_month <-
      (lubridate::as_date(days) - (duration / 24) - lubridate::days(1)) %>%
      min() %>%
      lubridate::floor_date(unit = "month")
  } else if (direction == "forward") {
    min_month <-
      (lubridate::as_date(days)) %>%
      min() %>%
      lubridate::floor_date(unit = "month")
  }

  # Determine the maximum month (as a `Date`) for the model run
  if (direction == "backward") {
    max_month <-
      (lubridate::as_date(days)) %>%
      max() %>%
      lubridate::floor_date(unit = "month")
  } else if (direction == "forward") {
    max_month <-
      (lubridate::as_date(days) + (duration / 24) + lubridate::days(1)) %>%
      max() %>%
      lubridate::floor_date(unit = "month")
  }

  met_months <- seq(min_month, max_month, by = "1 month")

  months_short <- met_months %>% to_short_month()

  years_long <- lubridate::year(met_months)

  paste0(prefix, years_long, months_short, extension)
}


get_daily_filenames <- function(days,
                                duration,
                                direction,
                                prefix = NULL,
                                suffix = NULL) {

  # Determine the minimum month (as a `Date`) for the model run
  if (direction == "backward") {

    min_day <-
      (lubridate::as_date(days) - (duration / 24) - lubridate::days(1)) %>%
      min()

  } else if (direction == "forward") {

    min_day <-
      (lubridate::as_date(days)) %>%
      min()
  }

  # Determine the maximum month (as a `Date`) for the model run
  if (direction == "backward") {

    max_day <-
      (lubridate::as_date(days)) %>%
      max()

  } else if (direction == "forward") {

    max_day <-
      (lubridate::as_date(days) + (duration / 24) + lubridate::days(1)) %>%
      max()
  }

  met_days <-
    seq(min_day, max_day, by = "1 day") %>%
    as.character() %>%
    tidy_gsub("-", "")

  paste0(prefix, met_days, suffix)
}

get_met_files <- function(files, path_met_files, ftp_dir) {

  # Determine which met files are already locally available
  files_in_path <- list.files(path_met_files)

  # Download list of GFS0.25 met files by name
  if (!is.null(files)) {

    for (file in files) {

      if (!(file %in% files_in_path)) {

        downloader::download(
          url = file.path(ftp_dir, file),
          destfile = path.expand(file.path(path_met_files, file)),
          method = "auto",
          quiet = FALSE,
          mode = "wb",
          cacheOK = FALSE
        )
      }
    }
  }

  files
}

get_traj_output_filename <- function(traj_name,
                                     site,
                                     direction,
                                     year,
                                     month,
                                     day,
                                     hour,
                                     lat,
                                     lon,
                                     height,
                                     duration) {

  paste0(
    "traj-",
    ifelse(is.null(traj_name), "", traj_name),
    "-",
    ifelse(direction == "backward", "bwd", "fwd"), "-",
    year, "-",
    month, "-",
    day, "-",
    hour, "-",
    site,
    "lat_", gsub("\\.", "p", as.character(lat)), "_",
    "lon_", gsub("\\.", "p", as.character(lon)), "-",
    "hgt_", height, "-",
    duration, "h"
  )
}

get_disp_output_filename <- function(disp_name,
                                     direction,
                                     year,
                                     month,
                                     day,
                                     hour,
                                     lat,
                                     lon,
                                     height,
                                     duration) {

  paste0(
    "disp-",
    ifelse(is.null(disp_name), "", disp_name),
    "-",
    ifelse(direction == "backward", "bwd", "fwd"), "-",
    year, "-",
    month, "-",
    day, "-",
    hour, "-",
    "lat_", gsub("\\.", "p", as.character(lat)), "_",
    "lon_", gsub("\\.", "p", as.character(lon)), "-",
    "hgt_", height, "-",
    duration, "h"
  )
}

get_receptor_values <- function(receptors_tbl,
                                receptor_i) {

  receptors_tbl[receptor_i, ] %>% as.list()
}

#' Wrapper for `gsub()` where `x` is the first argument
#'
#' This function is wrapper for `gsub()` that uses default argument values and
#' rearranges first three arguments for better pipelining
#' @param x,pattern,replacement,fixed Select arguments from the `gsub()`
#'   function.
#' @noRd
tidy_gsub <- function(x, pattern, replacement, fixed = FALSE) {

  gsub(pattern, replacement, x, fixed = fixed)
}

tidy_sub <- function(x, pattern, replacement, fixed = FALSE) {

  sub(pattern, replacement, x, fixed = fixed)
}

tidy_grepl <- function(x, pattern) {

  vapply(
    pattern,
    FUN = function(pattern) {
      grepl(pattern = pattern, x = x)
    },
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
}

traj_output_files <- function() {

  c(
    "ASCDATA.CFG",
    "CONTROL",
    "MESSAGE",
    "SETUP.CFG",
    "TRAJ.CFG",
    "WARNING"
  )
}

disp_output_files <- function() {

  c(
    "ASCDATA.CFG",
    "CONC.CFG",
    "CONTROL",
    "MESSAGE",
    "output.bin",
    "PARDUMP",
    "SETUP.CFG",
    "VMSDIST",
    "WARNING"
  )
}

check_start_day <- function(start_day) {

  # Stop if `start_day` isn't a length 1 vector
  if (length(start_day) != 1) {
    stop("The value provided to `start_day` must be a single value.",
         call. = FALSE)
  }

  if (!(inherits(start_day, "character") |
        inherits(start_day, "Date") |
        inherits(start_day, "POSIXct"))) {
    stop("The value provided to `start_day` must be a valid date.",
         call. = FALSE)
  }
}
