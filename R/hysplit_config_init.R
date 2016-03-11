#' Create default configuration files for trajectory
#' and dispersion modelling runs
#' @description This function creates default
#' \code{SETUP.CFG} and \code{ASCDATA.CFG}
#' configuration files and places them in the
#' working directories.
#' @export hysplit_config_init
#' @examples
#' \dontrun{
#' # Create a default `SETUP.CFG` and `ASCDATA.CFG`
#' # files for trajectory and dispersion runs
#' hysplit_config_init()
#' }

hysplit_config_init <- function(){
  
  # Create default `SETUP.CFG` configuration file for
  # dispersion runs and place it in the working
  # directory
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
      file = paste0(getwd(), "/", "SETUP.CFG"))
  
  # Create `ASCDATA.CFG` file and place in the 
  # working directory
  cat("-90.0  -180.0  lat/lon of lower left corner (last record in file)",
      "1.0  1.0	lat/lon spacing in degrees between data points",
      "180  360	lat/lon number of data points",
      "2  	default land use category",
      "0.2  	default roughness length (meters)",
      "'.'  directory location of data files",
      sep = "\n",
      file = paste0(getwd(), "/", "ASCDATA.CFG"))
}
