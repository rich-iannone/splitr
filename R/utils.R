# Create default `SETUP.CFG` and `ASCDATA.CFG`
# files for trajectory and dispersion runs
hysplit_config_init <- function(dir) {
  
  # Default `SETUP.CFG` configuration file
  cat(" &SETUP", " tratio = 0.75,", " initd = 0,", " kpuff = 0,", " khmax = 9999,",
      " kmixd = 0,", " kmix0 = 250,", " kzmix = 0,", " kdef = 0,", " kbls = 1,",
      " kblt = 2,", " conage = 48,", " numpar = 2500,", " qcycle = 0.0,", " efile = '',",
      " tkerd = 0.18,", " tkern = 0.18,", " ninit = 1,", " ndump = 1,", " ncycl = 1,",
      " pinpf = 'PARINIT',", " poutf = 'PARDUMP',", " mgmin = 10,", " kmsl = 0,",
      " maxpar = 10000,", " cpack = 1,", " cmass = 0,", " dxf = 1.0,", " dyf = 1.0,",
      " dzf = 0.01,", " ichem = 0,", " maxdim = 1,", " kspl = 1,", " krnd = 6,",
      " frhs = 1.0,", " frvs = 0.01,", " frts = 0.10,", " frhmax = 3.0,", " splitf = 1.0,",
      " tm_pres = 0,", " tm_tpot = 0,", " tm_tamb = 0,", " tm_rain = 0,", " tm_mixd = 0,", 
      " tm_relh = 0,", " tm_sphu = 0,", " tm_mixr = 0,", " tm_dswf = 0,", " tm_terr = 0,",  
      " /", sep = "\n",
      file = paste0(dir, "/", "SETUP.CFG"))
  
  # Default `ASCDATA.CFG` file
  cat("-90.0  -180.0  lat/lon of lower left corner (last record in file)",
      "1.0  1.0	lat/lon spacing in degrees between data points",
      "180  360	lat/lon number of data points",
      "2  	default land use category",
      "0.2  	default roughness length (meters)",
      "'.'  directory location of data files",
      sep = "\n",
      file = paste0(dir, "/", "ASCDATA.CFG"))
}

# Determine the operating system in use
get_os <- function() {
  if (.Platform$OS.type == "windows") { 
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac" 
  } else if (.Platform$OS.type == "unix") { 
    "unix"
  } else {
    stop("Unknown OS")
  }
}

# Determine whether a 64-bit architecture is present 
is_64bit_system <- function() {
  ifelse(.Machine$sizeof.pointer == 8, TRUE, FALSE)
}

create_file_list <- function(output_folder,
                             create_file = TRUE,
                             file_name = "file_list.txt") {
  
  # List files from the specified archive folder
  file_list <- 
    list.files(output_folder,
               pattern = "traj.*")
  
  # Create file list in the output folder
  cat(file_list,
      file = paste0(output_folder, "/",
                    file_name),
      sep = '\n', append = FALSE)
  
  return(file_list)
}
