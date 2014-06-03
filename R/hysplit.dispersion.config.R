hysplit.dispersion.config <- function(path_wd = "~/Documents/SplitR/Working/"){
  
  # Create default configuration file for dispersion runs and place it in the
  # working directory
  if (create_default_config == TRUE){
    
    # Create SETUP.CFG file and place in working directory
    cat(" &SETUP", " tratio = 0.75,", " initd = 0,", " kpuff = 0,", " khmax = 9999,",
        " kmixd = 0,", " kmix0 = 250,", " kzmix = 0,", " kdef = 0,", " kbls = 1,",
        " kblt = 2,", " conage = 48,", " numpar = 2500,", " qcycle = 0.0,", " efile = '',",
        " tkerd = 0.18,", " tkern = 0.18,", " ninit = 1,", " ndump = 1,", " ncycl = 1,",
        " pinpf = 'PARINIT',", " poutf = 'PARDUMP',", " mgmin = 10,", " kmsl = 0,",
        " maxpar = 10000,", " cpack = 1,", " cmass = 0,", " dxf = 1.0,", " dyf = 1.0,",
        " dzf = 0.01,", " ichem = 0,", " maxdim = 1,", " kspl = 1,", " krnd = 6,",
        " frhs = 1.0,", " frvs = 0.01,", " frts = 0.10,", " frhmax = 3.0,", " splitf = 1.0,",
        " /", sep = "\n", file = "~/Documents/SplitR/Working/SETUP.CFG")
    
    # Create ASCDATA.CFG file and place in working directory
    cat("-90.0  -180.0  lat/lon of lower left corner (last record in file)",
        "1.0  1.0	lat/lon spacing in degrees between data points",
        "180  360	lat/lon number of data points",
        "2  	default land use category",
        "0.2  	default roughness length (meters)",
        "'.'  directory location of data files",
        sep = "\n", file = "~/Documents/SplitR/Working/ASCDATA.CFG")
    
    # Create ROUGHLEN.ASC, LANDUSE.ASC, and TERRAIN.ASC
    
  } 
  
}
                                      