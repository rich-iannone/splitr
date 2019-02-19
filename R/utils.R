
#' Download files using consistent options
#' @param url The URL from which the files reside.
#' @param local_path The path to which the files should be written.
met_download <- function(url, local_path) {
  
  download(
    url = url,
    destfile = local_path,
    method = "auto",
    quiet = TRUE,
    mode = "wb",
    cacheOK = FALSE
  )
}

#' Create default `SETUP.CFG` and `ASCDATA.CFG` files
#' @param dir The directory to which the files should be written.
#' @noRd
hysplit_config_init <- function(dir) {
  
  # Default `SETUP.CFG` configuration file
  cat(
    " &SETUP", " tratio = 0.75,", " initd = 0,", " kpuff = 0,", " khmax = 9999,",
    " kmixd = 0,", " kmix0 = 250,", " kzmix = 0,", " kdef = 0,", " kbls = 1,",
    " kblt = 2,", " conage = 48,", " numpar = 2500,", " qcycle = 0.0,", " efile = '',",
    " tkerd = 0.18,", " tkern = 0.18,", " ninit = 1,", " ndump = 1,", " ncycl = 1,",
    " pinpf = 'PARINIT',", " poutf = 'PARDUMP',", " mgmin = 10,", " kmsl = 0,",
    " maxpar = 10000,", " cpack = 1,", " cmass = 0,", " dxf = 1.0,", " dyf = 1.0,",
    " dzf = 0.01,", " ichem = 0,", " maxdim = 1,", " kspl = 1,", " krnd = 6,",
    " frhs = 1.0,", " frvs = 0.01,", " frts = 0.10,", " frhmax = 3.0,", " splitf = 1.0,",
    " tm_pres = 0,", " tm_tpot = 0,", " tm_tamb = 0,", " tm_rain = 0,", " tm_mixd = 0,", 
    " tm_relh = 0,", " tm_sphu = 0,", " tm_mixr = 0,", " tm_dswf = 0,", " tm_terr = 0,",  
    " /",
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

#' Determine which operating system is in use
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
#' @noRd 
is_64bit_system <- function() {
  ifelse(.Machine$sizeof.pointer == 8, TRUE, FALSE)
}

#' Create a file list for output files
#' @param output_folder The directory where the file list is to be written.
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
