hysplit.dispersion.define <- function(define){
  
  require(lubridate)
  
  # Define species (names, properties)
  if (define == 'species'){
    
    # Ask for the name of the species
    species_name <-
      readline("What is the name of the species? ")
    
    # Ask whether it's a gas-phase species or a particle
    species_type <-
      readline("Is the species a gas or particle? [gas/particle] ")
    
    # If gas-phase, ask if default parameters acceptable
    if (species_type == "gas") {
      
      species_type_gas_use_default <-
        readline("Use the default parameters for a gas-phase species? [y/n] ")
      
      if (species_type_gas_use_default == "y") {
        particle_pdiam <- 0.0
        particle_density <- 0.0
        particle_shape_factor <- 0.0
        ddep_velocity <- 0.0
        ddep_MW <- 0.0
        ddep_A_ratio <- 0.0
        ddep_D_ratio <- 0.0
        ddep_Henrys_Law_coeff <- 0.0
        wdep_Henrys_Law_coeff <- 0.0
        wdep_in_cloud_dep <- 0.0
        wdep_below_cloud_dep <- 0.0
        rad_decay <- 0.0
        pollutant_resuspension_factor <- 0.0
      }
      if (species_type_gas_use_default == "n") {
        
        # Automatically assign default value to 'particle_pdiam'
        particle_pdiam <- 0.0
        
        # Automatically assign default value to 'particle_density'
        particle_density <- 0.0
        
        # Automatically assign default value to 'particle_shape_factor'
        particle_shape_factor <- 0.0
        
        # Ask to assign value to 'ddep_velocity'
        ddep_velocity <-
          readline(paste(cat("Category: Dry Deposition", "\n",
                             "Set the dry deposition velocity.", "\n",
                             "Units: m/s. Default: 0 m/s.", "\n",
                             "Provide a positive real number (<ENTER> for default value): ",
                             sep = '')))
        if (ddep_velocity == "") ddep_velocity <- 0.0
        if (ddep_velocity >= 0) ddep_velocity <- as.numeric(ddep_velocity)
        
        # Ask to assign value to 'ddep_MW'
        ddep_MW <-
          readline(paste(cat("Category: Dry Deposition", "\n",
                             "Set the molecular weight.", "\n",
                             "Units: g/mol. Default: 0 g/mol.", "\n",
                             "Provide a positive real number (<ENTER> for default value): ",
                             sep = '')))
        if (ddep_MW == "") ddep_MW <- 0.0
        if (ddep_MW >= 0) ddep_MW <- as.numeric(ddep_MW)
        
        # Ask to assign value to 'ddep_A_ratio'
        ddep_A_ratio <-
          readline(paste(cat("Category: Dry Deposition", "\n",
                             "Set the surface reactivity ratio.", "\n",
                             "Units: none. Default: 0.", "\n",
                             "Provide a value between 0-1 (<ENTER> for default value): ",
                             sep = '')))
        if (ddep_A_ratio == "") ddep_A_ratio <- 0.0
        if (ddep_A_ratio >= 0) ddep_A_ratio <- as.numeric(ddep_A_ratio)
          
        # Ask to assign value to 'ddep_D_ratio'
        ddep_D_ratio <-
          readline(paste(cat("Category: Dry Deposition", "\n",
                             "Set the diffusivity ratio.", "\n",
                             "Units: none. Default: 0.", "\n",
                             "Provide a positive real number (<ENTER> for default value): ",
                             sep = '')))
        if (ddep_D_ratio == "") ddep_D_ratio <- 0.0
        if (ddep_D_ratio >= 0) ddep_D_ratio <- as.numeric(ddep_D_ratio)
          
        # Ask to assign value to 'ddep_Henrys_Law_coeff'
        ddep_Henrys_Law_coeff <-
          readline(paste(cat("Category: Dry Deposition", "\n",
                             "Set the effective Henry's Law Constant.", "\n",
                             "Units: M/atm. Default: 0.", "\n",
                             "Provide a postive real number (<ENTER> for default value): ",
                             sep = '')))
        if (ddep_Henrys_Law_coeff == "") ddep_Henrys_Law_coeff <- 0.0
        if (ddep_Henrys_Law_coeff >= 0) ddep_Henrys_Law_coeff <-
          as.numeric(ddep_Henrys_Law_coeff)
          
        # Ask to assign value to 'wdep_Henrys_Law_coeff' 
        wdep_Henrys_Law_coeff <-
          readline(paste(cat("Category: Wet Deposition", "\n",
                             "Set the actual Henry's Law Constant.", "\n",
                             "Units: M/atm. Default: 0.", "\n",
                             "Provide a postive real number (<ENTER> for default value): ",
                             sep = '')))
        if (wdep_Henrys_Law_coeff == "") wdep_Henrys_Law_coeff <- 0.0
        if (wdep_Henrys_Law_coeff >= 0) wdep_Henrys_Law_coeff <-
          as.numeric(wdep_Henrys_Law_coeff)
          
        # Ask to assign value to 'wdep_in_cloud_dep'  
        wdep_in_cloud_dep <-
          readline(paste(cat("Category: Wet Deposition", "\n",
                             "Set the in-cloud deposition rate.", "\n",
                             "Units: L/L. Default: 0.", "\n",
                             "Provide a postive real number (<ENTER> for default value): ",
                             sep = '')))
        if (wdep_in_cloud_dep == "") wdep_in_cloud_dep <- 0.0
        if (wdep_in_cloud_dep >= 0) wdep_in_cloud_dep <-
          as.numeric(wdep_in_cloud_dep)
          
        # Ask to assign value to 'wdep_below_cloud_dep'  
        wdep_below_cloud_dep <-
          readline(paste(cat("Category: Wet Deposition", "\n",
                             "Set the below-cloud deposition rate.", "\n",
                             "Units: 1/s. Default: 0.", "\n",
                             "Provide a postive real number (<ENTER> for default value): ",
                             sep = '')))
        if (wdep_below_cloud_dep == "") wdep_below_cloud_dep <- 0.0
        if (wdep_below_cloud_dep >= 0) wdep_below_cloud_dep <-
          as.numeric(wdep_below_cloud_dep)
          
        # Ask to assign value to 'rad_decay'
        rad_decay <-
          readline(paste(cat("Category: Radioactive Decay", "\n",
                             "Set the radioactive decay half-life.", "\n",
                             "Units: days. Default: 0.", "\n",
                             "Provide a postive real number (<ENTER> for default value): ",
                             sep = '')))
        if (rad_decay == "") rad_decay <- 0.0
        if (rad_decay >= 0) rad_decay <- as.numeric(rad_decay)
          
        # Ask to assign value to 'pollutant_resuspension_factor'  
        pollutant_resuspension_factor <-
          readline(paste(cat("Category: Pollutant Resuspension", "\n",
                             "Set the pollutant resuspension factor.", "\n",
                             "Units: 1/m. Default: 0.", "\n",
                             "Provide a postive real number (<ENTER> for default value): ",
                             sep = '')))
        if (pollutant_resuspension_factor == "") pollutant_resuspension_factor <- 0.0
        if (pollutant_resuspension_factor >= 0) pollutant_resuspension_factor <-
          as.numeric(pollutant_resuspension_factor)
      }
    }
    
    # If particle, ask if default parameters acceptable
    if (species_type == "particle") {
      
      species_type_particle_use_default <-
        readline("Use the default parameters for a particle species? [y/n] ")
      
      if (species_type_particle_use_default == "y") {
        particle_pdiam <- 5.0
        particle_density <- 6.0
        particle_shape_factor <- 1.0
        ddep_velocity <- 0.0
        ddep_MW <- 0.0
        ddep_A_ratio <- 0.0
        ddep_D_ratio <- 0.0
        ddep_Henrys_Law_coeff <- 0.0
        wdep_Henrys_Law_coeff <- 0.0
        wdep_in_cloud_dep <- 40000
        wdep_below_cloud_dep <- 0.000006
        rad_decay <- 0.0
        pollutant_resuspension_factor <- 0.0
      }
      if (species_type_particle_use_default == "n") {
        
        # Ask to assign default value to 'particle_pdiam'
        particle_pdiam <-
          readline(paste(cat("Category: Particle Properties", "\n",
                             "Set the particle diameter.", "\n",
                             "Units: µm. Default: 5 µm.", "\n",
                             "Provide a positive real number (<ENTER> for default value): ",
                             sep = '')))
        if (particle_pdiam == "") particle_pdiam <- 5.0
        if (particle_pdiam >= 0) particle_pdiam <- as.numeric(particle_pdiam)
        
        # Ask to assign default value to 'particle_density'
        particle_density <-
          readline(paste(cat("Category: Particle Properties", "\n",
                             "Set the particle density.", "\n",
                             "Units: g/cm3. Default: 6 g/cm3.", "\n",
                             "Provide a positive real number (<ENTER> for default value): ",
                             sep = '')))
        if (particle_density == "") particle_density <- 6.0
        if (particle_density >= 0) particle_density <- as.numeric(particle_density)
        
        # Ask to assign default value to 'particle_shape_factor'
        particle_shape_factor <-
          readline(paste(cat("Category: Particle Properties", "\n",
                             "Set the particle shape factor.", "\n",
                             "Units: none. Default: 1.", "\n",
                             "Provide a value between 0-1 (<ENTER> for default value): ",
                             sep = '')))
        if (particle_shape_factor == "") particle_shape_factor <- 1.0
        if (particle_shape_factor >= 0) particle_shape_factor <-
          as.numeric(particle_shape_factor)
        
        # Ask to assign value to 'ddep_velocity'
        ddep_velocity <-
          readline(paste(cat("Category: Dry Deposition", "\n",
                             "Set the dry deposition velocity.", "\n",
                             "Units: m/s. Default: 0 m/s.", "\n",
                             "Provide a positive real number (<ENTER> for default value): ",
                             sep = '')))
        if (ddep_velocity == "") ddep_velocity <- 0.0
        if (ddep_velocity >= 0) ddep_velocity <- as.numeric(ddep_velocity)
        
        # Ask to assign value to 'ddep_MW'
        ddep_MW <-
          readline(paste(cat("Category: Dry Deposition", "\n",
                             "Set the molecular weight.", "\n",
                             "Units: g/mol. Default: 0 g/mol.", "\n",
                             "Provide a positive real number (<ENTER> for default value): ",
                             sep = '')))
        if (ddep_MW == "") ddep_MW <- 0.0
        if (ddep_MW >= 0) ddep_MW <- as.numeric(ddep_MW)
        
        # Ask to assign value to 'ddep_A_ratio'
        ddep_A_ratio <-
          readline(paste(cat("Category: Dry Deposition", "\n",
                             "Set the A ratio.", "\n",
                             "Units: none. Default: 0.", "\n",
                             "Provide a value between 0-1 (<ENTER> for default value): ",
                             sep = '')))
        if (ddep_A_ratio == "") ddep_A_ratio <- 0.0
        if (ddep_A_ratio >= 0) ddep_A_ratio <- as.numeric(ddep_A_ratio)
        
        # Ask to assign value to 'ddep_D_ratio'
        ddep_D_ratio <-
          readline(paste(cat("Category: Dry Deposition", "\n",
                             "Set the D ratio.", "\n",
                             "Units: none. Default: 0.", "\n",
                             "Provide a value between 0-1 (<ENTER> for default value): ",
                             sep = '')))
        if (ddep_D_ratio == "") ddep_D_ratio <- 0.0
        if (ddep_D_ratio >= 0) ddep_D_ratio <- as.numeric(ddep_D_ratio)
        
        # Ask to assign value to 'ddep_Henrys_Law_coeff'
        ddep_Henrys_Law_coeff <-
          readline(paste(cat("Category: Dry Deposition", "\n",
                             "Set the Henry's Law coefficient.", "\n",
                             "Units: M/a. Default: 0.", "\n",
                             "Provide a postive real number (<ENTER> for default value): ",
                             sep = '')))
        if (ddep_Henrys_Law_coeff == "") ddep_Henrys_Law_coeff <- 0.0
        if (ddep_Henrys_Law_coeff >= 0) ddep_Henrys_Law_coeff <-
          as.numeric(ddep_Henrys_Law_coeff)
        
        # Ask to assign value to 'wdep_Henrys_Law_coeff' 
        wdep_Henrys_Law_coeff <-
          readline(paste(cat("Category: Wet Deposition", "\n",
                             "Set the Henry's Law coefficient.", "\n",
                             "Units: M/a. Default: 0.", "\n",
                             "Provide a postive real number (<ENTER> for default value): ",
                             sep = '')))
        if (wdep_Henrys_Law_coeff == "") wdep_Henrys_Law_coeff <- 0.0
        if (wdep_Henrys_Law_coeff >= 0) wdep_Henrys_Law_coeff <-
          as.numeric(wdep_Henrys_Law_coeff)
        
        # Ask to assign value to 'wdep_in_cloud_dep'  
        wdep_in_cloud_dep <-
          readline(paste(cat("Category: Wet Deposition", "\n",
                             "Set the in-cloud deposition rate.", "\n",
                             "Units: L/L. Default: 0.", "\n",
                             "Provide a postive real number (<ENTER> for default value): ",
                             sep = '')))
        if (wdep_in_cloud_dep == "") wdep_in_cloud_dep <- 0.0
        if (wdep_in_cloud_dep >= 0) wdep_in_cloud_dep <-
          as.numeric(wdep_in_cloud_dep)
        
        # Ask to assign value to 'wdep_below_cloud_dep'  
        wdep_below_cloud_dep <-
          readline(paste(cat("Category: Wet Deposition", "\n",
                             "Set the below-cloud deposition rate.", "\n",
                             "Units: 1/s. Default: 0.", "\n",
                             "Provide a postive real number (<ENTER> for default value): ",
                             sep = '')))
        if (wdep_below_cloud_dep == "") wdep_below_cloud_dep <- 0.0
        if (wdep_below_cloud_dep >= 0) wdep_below_cloud_dep <-
          as.numeric(wdep_below_cloud_dep)
        
        # Ask to assign value to 'rad_decay'
        rad_decay <-
          readline(paste(cat("Category: Radioactive Decay", "\n",
                             "Set the radioactive decay half-life.", "\n",
                             "Units: days. Default: 0.", "\n",
                             "Provide a postive real number (<ENTER> for default value): ",
                             sep = '')))
        if (rad_decay == "") rad_decay <- 0.0
        if (rad_decay >= 0) rad_decay <- as.numeric(rad_decay)
        
        # Ask to assign value to 'pollutant_resuspension_factor'  
        pollutant_resuspension_factor <-
          readline(paste(cat("Category: Pollutant Resuspension", "\n",
                             "Set the pollutant resuspension factor.", "\n",
                             "Units: 1/m. Default: 0.", "\n",
                             "Provide a postive real number (<ENTER> for default value): ",
                             sep = '')))
        if (pollutant_resuspension_factor == "") pollutant_resuspension_factor <- 0.0
        if (pollutant_resuspension_factor >= 0) pollutant_resuspension_factor <-
          as.numeric(pollutant_resuspension_factor)
      }
    }
    
    # Provide a summary of the chosen options and ask if the chosen options are acceptable
    
    acceptable_species <-
      readline(paste(cat("The plan. Adding species: ", species_name, "\n",
                         "-------------------------", "\n", 
                         "Particle Properties // ", 
                         "diameter: ", particle_pdiam, " µm",
                         " | density: ", particle_density, " g/cm3",
                         " | shape factor: ", particle_shape_factor,
                         ifelse(particle_pdiam == 0 &
                                  particle_density == 0 &
                                  particle_shape_factor == 0,
                                paste(" ——not a particle species"), paste("")),
                         "\n",
                         "Dry Deposition // ",
                         "deposition velocity: ", ddep_velocity, " m/s",
                         " | molecular weight: ", ddep_MW, " g/mol", "\n",
                         "                 ",
                         " A ratio: ", ddep_A_ratio,
                         " | D ratio: ", ddep_D_ratio,
                         " | Henry's Law: ", ddep_Henrys_Law_coeff, " M/a",
                         ifelse(ddep_velocity == 0 &
                                ddep_MW == 0 &
                                ddep_A_ratio == 0 &
                                ddep_D_ratio == 0 &
                                ddep_Henrys_Law_coeff == 0,
                                paste(" ——no dry deposition"), paste("")),
                         "\n",
                         "Wet Deposition // ",
                         "Henry's Law coeff.: ", wdep_Henrys_Law_coeff, " M/a",
                         " | in-cloud deposition: ", wdep_in_cloud_dep, " L/L", "\n",
                         "                ",
                         "  below-cloud deposition: ", wdep_below_cloud_dep, " 1/s",
                         ifelse(wdep_Henrys_Law_coeff == 0 &
                                  wdep_in_cloud_dep == 0 &
                                  wdep_below_cloud_dep == 0,
                                paste(" ——no wet deposition"), paste("")),
                         "\n",
                         "Radioactive Decay // ",
                         "half-life: ", rad_decay, " days",
                         "\n",
                         "Pollutant Resuspension // ",
                         "factor: ", pollutant_resuspension_factor, " 1/m", "\n",
                         "------------------------------", "\n",
                         "This is what will be set. Okay? [y/n]: ",
                         sep = '')))
    
    # If acceptable, add the species and the options to the list of species
    # in the project folder
    
    
    # Close species block
  }
  
  # Define emissions (fixed location, rate, hours, start time, must tie to an already
  # defined species or define a new one here)
  if (define == 'emissions'){
  
    # Ask for the name of the emissions source
    emissions_name <-
      readline("What is the name of the emissions source? ")
    
    # Ask to assign value to 'emissions_start_time'  
    emissions_start_time <-
      readline(paste(cat("Provide the starting date and time.", "\n",
                         "Use the format YYYY-MM-DD HH:MM", "\n",
                         sep = '')))
    
    # Validate the input of the date and time string
    if (grepl("[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-9][0-9]",
              emissions_start_time, perl = TRUE) == TRUE) {
      emissions_start_time_valid_1 <- TRUE
    } else if (grepl("[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-9][0-9]",
                     emissions_start_time, perl = TRUE) == FALSE) {
      emissions_start_time_valid_1 <- FALSE
    }
    
    # Determine whether the supplied date/time string yields a valid POSIXct date/time
    if (class(ymd_hms(paste(emissions_start_time, ":00", sep = '')))[1] == "POSIXct"){
      emissions_start_time_valid_2 <- TRUE
    } else if (is.na(ymd_hms(paste(emissions_start_time, ":00", sep = '')))){
      emissions_start_time_valid_2 <- FALSE
    }
    
    # If the entered date passes both validation tests, assign it to 'emissions_start_time'
    if (emissions_start_time_valid_1 == TRUE &
          emissions_start_time_valid_2 == TRUE ) {
      emissions_start_time_char <- as.character(gsub("-", " ", emissions_start_time))
      emissions_start_time_char <- as.character(gsub(":", " ", emissions_start_time_char))
    }
    
    # Ask to assign value to 'emissions_duration'    
    emissions_duration <-
      readline(paste(cat("Provide either a time duration in hours", "\n",
                         "or days, or, provide an ending date and time.", "\n",
                         "Use the formats # h, # d, or YYYY-MM-DD HH:MM", "\n",
                         sep = '')))
    
    # Determine the format of the supplied input and divide into types to resolve into hours
    # Validate the input of the date and time string
    if (grepl("[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-9][0-9]",
              emissions_duration, perl = TRUE) == TRUE){
      emissions_duration_type <- "ending_date_time"
    } else if (grepl("^[0-9]+[ ]?h$", emissions_duration, perl = TRUE) == TRUE){
      emissions_duration_type <- "duration_hours"
    } else if (grepl("^[0-9]+[ ]?d$", emissions_duration, perl = TRUE) == TRUE){
      emissions_duration_type <- "duration_days"
    }
    
    # Work with case where 'emissions_duration_type' is an end date and time
    if (exists("emissions_duration_type") &
          emissions_duration_type ==  "ending_date_time"){
      
      # Determine whether the supplied date/time string yields a valid POSIXct date/time
      if (class(ymd_hms(paste(emissions_duration, ":00", sep = '')))[1] == "POSIXct"){
        emissions_duration_valid <- TRUE
      } else if (is.na(ymd_hms(paste(emissions_duration, ":00", sep = '')))){
        emissions_duration_valid <- FALSE
      }
      
      if (emissions_duration_valid == TRUE){
        
        # Get ending date/time as a POSIXct date/time object
        emissions_end_POSIXct <- ymd_hms(paste(emissions_duration, ":00", sep = ''))
        
        # Get starting date/time as a POSIXct date/time object
        emissions_start_POSIXct <- ymd_hms(paste(emissions_start_time, ":00", sep = ''))
        
        # Get the difference in days between the starting and ending times
        # Creates a 'difftime' object
        emissions_duration_diff_days <- emissions_end_POSIXct - emissions_start_POSIXct
        
        # Get the difference in hours
        emissions_duration_diff_hours <- emissions_duration_diff[[1]] * 24
    
        emissions_duration <- emissions_duration_diff_hours
      }
    }
    
    # Work with case where 'emissions_duration_type' is duration in hours
    if (exists("emissions_duration_type") &
          emissions_duration_type ==  "duration_hours"){
      
      # Get the numeric part of the supplied string, which is the number of hours
      emissions_duration <- as.numeric(gsub("^([0-9]+)[ ]?h$", "\\1",
                                 emissions_duration, perl = TRUE))
    }
    
    # Work with case where 'emissions_duration_type' is a duration in days
    if (exists("emissions_duration_type") &
          emissions_duration_type ==  "duration_days"){
      
      # Get the numeric part of the supplied string, which is the number of hours
      emissions_duration <- as.numeric(gsub("^([0-9]+)[ ]?d$", "\\1",
                                            emissions_duration, perl = TRUE)) * 24
    }
    
    # Ask to assign value to 'emissions_rate'    
    emissions_rate <-
      readline(paste(cat("Provide the rate of emissions in mass units per hour.", "\n",
                         sep = '')))
    
    # Provide a summary of the chosen options and ask if the chosen options are acceptable
    acceptable_emissions <-
      readline(paste(cat("The plan. Adding emissions source: ", emissions_name, "\n",
                         "----------------------------------", "\n", 
                         "Start Date/Time: ", emissions_start_time, "\n",
                         "       Duration: ", emissions_duration, " h", "\n",
                         " Emissions Rate: ", emissions_rate, " 1/h", "\n",
                         "----------------------------------", "\n",
                         "This is what will be set. Okay? [y/n]: ",
                         sep = '')))
    
    # If acceptable, add the emissions source and the options to the list of emissions
    # sources in the project folder
    
    
    # Close emissions block
  }
  
  # Define grids (fixed location, size/spacing, rate of sampling, sampling times)
  if (define == 'grids'){
    
    # Ask for the name of the grid
    grid_name <-
      readline("What is the name of the grid? ")
    
    # Ask to assign value to 'grid_center'
    grid_center <-
      readline(paste(cat("Provide the center of the grid.", "\n",
                         "Units: degrees. Default: none.", "\n",
                         "Provide the latitude and then the longitude: ",
                         sep = '')))
    
    # Get input string into a vector object of length 2
    grid_center_coords <- unlist(strsplit(grid_center, "[, | |,]"))
    grid_center_coords <- grid_center_coords[grid_center_coords != ""]
    
    # Separate latitude and longitude into two separate numeric objects
    grid_center_lat <- as.numeric(grid_center_coords[1])
    grid_center_lon <- as.numeric(grid_center_coords[2])
    
    # Check latitude value and validate
    if (grid_center_lat >= -90 & grid_center_lat <= 90) grid_center_lat_valid <- TRUE
    
    # Check longitude value and validate
    if (grid_center_lon >= -180 & grid_center_lon <= 180) grid_center_lon_valid <- TRUE    
    
    if (exists("grid_center_lat_valid") & grid_center_lat_valid == TRUE &
          exists("grid_center_lon_valid") & grid_center_lon_valid == TRUE){
      grid_center <- paste(grid_center_lat, grid_center_lon, sep = ' ')
      grid_center_char <- paste(grid_center_lat, ", ", grid_center_lon, sep = '') 
    }
    
    # Ask to assign value to 'grid_spacing'
    grid_spacing <-
      readline(paste(cat("Provide the spacing of adjacent grid points in the x and y directions.",
                         "\n",
                         "Units: degrees. Default: none.", "\n",
                         "Provide the latitude interval and then the longitude interval: ",
                         sep = '')))
    
    # Get input string into a vector object of length 2
    grid_spacing_values <- unlist(strsplit(grid_spacing, "[, | |,]"))
    grid_spacing_values <- grid_spacing_values[grid_spacing_values != ""]
    
    # Separate latitude and longitude into two separate numeric objects
    grid_spacing_lat <- as.numeric(grid_spacing_values[1])
    grid_spacing_lon <- as.numeric(grid_spacing_values[2])
    
    # Check latitude grid spacing value and validate
    if (grid_spacing_lat > 0) grid_spacing_lat_valid <- TRUE
    
    # Check longitude grid spacing value and validate
    if (grid_spacing_lon > 0) grid_spacing_lon_valid <- TRUE   
    
    if (exists("grid_spacing_lat_valid") & grid_spacing_lat_valid == TRUE &
          exists("grid_spacing_lon_valid") & grid_spacing_lon_valid == TRUE){
      grid_spacing <- paste(grid_spacing_lat, grid_spacing_lon, sep = ' ')
      grid_spacing_char <- paste(grid_spacing_lat, ", ", grid_spacing_lon, sep = '')
    }
    
    # Ask to assign value to 'grid_span'
    grid_span <-
      readline(paste(cat("Provide the total span of the grid in the x and y directions.",
                         "\n",
                         "Units: degrees. Default: none.", "\n",
                         "Provide the latitude value and then the longitude value: ",
                         sep = '')))
    
    # Get input string into a vector object of length 2
    grid_span_values <- unlist(strsplit(grid_span, "[, | |,]"))
    grid_span_values <- grid_span_values[grid_span_values != ""]
    
    # Separate latitude and longitude into two separate numeric objects
    grid_span_lat <- as.numeric(grid_span_values[1])
    grid_span_lon <- as.numeric(grid_span_values[2])
    
    # Check latitude grid span value and validate
    if (grid_span_lat > 0) grid_span_lat_valid <- TRUE 
    
    # Check longitude grid span value and validate
    if (grid_span_lon > 0) grid_span_lon_valid <- TRUE 
    
    if (exists("grid_span_lat_valid") & grid_span_lat_valid == TRUE &
          exists("grid_span_lon_valid") & grid_span_lon_valid == TRUE){
      
      # Check to see that grid span in y direction is a multiple of its grid spacing value
      if (grid_span_lat %% grid_spacing_lat == 0) grid_span_lat_valid <- TRUE
      if (grid_span_lat %% grid_spacing_lat != 0) grid_span_lat_valid <- FALSE
      
      # Check to see that grid span in x direction is a multiple of its grid spacing value
      if (grid_span_lon %% grid_spacing_lon == 0) grid_span_lon_valid <- TRUE
      if (grid_span_lon %% grid_spacing_lon != 0) grid_span_lon_valid <- FALSE  
      
      # If the span values pass the validity tests, assign string to the 'grid_span' object
      if (grid_span_lat_valid == TRUE &
            grid_span_lon_valid == TRUE){
        grid_span <- paste(grid_span_lat, grid_span_lon, sep = ' ')
        grid_span_char <- paste(grid_span_lat, ", ", grid_span_lon, sep = '')
      } 
    }
    
    # Automatically assign the location of the 'grids' folder
    grid_folder <- "./"
    
    # Automatically assign the output grid file name
    grid_filename <- grid_name
    
    # Ask to assign number of vertical levels 'grid_number_vertical'
    grid_number_vertical <-
      readline(paste(cat("Provide the number of vertical levels in the concentration grid.",
                         "\n",
                         "This number includes the deposition layer (with height = 0 m AGL)",
                         "\n",
                         "Default: 1.", "\n",
                         "Provide a postive integer (<ENTER> for default value): ",
                         sep = '')))
    
    if (grid_number_vertical == "") grid_number_vertical <- 1
    if (grid_number_vertical >= 0) grid_number_vertical <-
      round(as.numeric(grid_number_vertical), digits = 0)
    
    # Assign as string to 'grid_number_vertical'
    grid_number_vertical_string <- as.character(grid_number_vertical)
   
    # Ask to assign the height of each vertical level
    grid_heights <-
      readline(paste(cat(if(grid_number_vertical == 1){
                           paste("For the single level specified, does that refer to the",
                                 "ground (deposition layer) or some height ")
                         },
                         if(grid_number_vertical == 1) paste("\n"),
                         if(grid_number_vertical == 1) paste("above the ground?"),
                         if(grid_number_vertical == 1) paste("\n"),
                         if(grid_number_vertical == 1){
                           paste("Press <ENTER> to assign level to the ground layer, or,",
                                 " provide a height in meters above ground level: ",
                                 sep = '')
                         },
                         if(grid_number_vertical > 1){
                           paste("For the ", grid_number_vertical, " levels specified, provide ",
                                 "a list of heights in meters above ground level: ", sep = '')
                         },
                         sep = '')))
    
    if (grid_number_vertical == 1 & grid_heights == '') grid_heights <- 0
    if (grid_number_vertical > 0 & grid_heights == ''){
      grid_heights <- seq(0, 200*grid_number_vertical, by = 200)
    }
    
    # Get input string into a vector object
    if (grid_heights != 0){
    grid_heights_values <- unlist(strsplit(grid_heights, "[, | |,]"))
    grid_heights_values <- as.numeric(grid_heights_values[grid_heights_values != ""])
    }
    
    # Verify that the values are in ascending order; sort ascending if not sorted
    if (is.unsorted(grid_heights_values) == TRUE){
      grid_heights_values <- sort(grid_heights_values)
    }
    
    # Determine whether duplicate values were provided
    if (anyDuplicated(grid_heights_values) != 0) grid_heights_values_duplicated <- TRUE
    
    # If the grid height values pass the validity test, assign string to the 'grid_heights_string'
    # object
    if (!exists("grid_heights_values_duplicated")){
      grid_heights_string <- as.character(grid_heights_values)
    } 
    
    # Create a formatted list of grid heights
    for (i in 1:length(grid_heights_string)){
      if (i == 1) {
        grid_heights_string_list_comma_sep <- NULL
        grid_heights_string_list_space_sep <- NULL
      }
      grid_heights_string_list_comma_sep <- paste(grid_heights_string_list_comma_sep,
                                                  ifelse(i == 1, "", ", "),
                                                  grid_heights_string[i],
                                                  sep = '')
      grid_heights_string_list_space_sep <- paste(grid_heights_string_list_comma_sep,
                                                  ifelse(i == 1, "", " "),
                                                  grid_heights_string[i],
                                                  sep = '')
    }
    
    # Ask to assign value to 'grid_start_time'
    grid_start_time <-
      readline(paste(cat("Provide a date and time for the start of grid sampling.",
                         "\n",
                         "Use the format YYYY-MM-DD HH:MM", "\n",
                         sep = '')))
    
    # Validate the input of the date and time string
    if (grepl("[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-9][0-9]",
              grid_start_time, perl = TRUE) == TRUE) {
      grid_start_time_valid_1 <- TRUE
    } else if (grepl("[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-9][0-9]",
                     grid_start_time, perl = TRUE) == FALSE) {
      grid_start_time_valid_1 <- FALSE
    }

    # Determine whether the supplied date/time string yields a valid POSIXct date/time
    if (class(ymd_hms(paste(grid_start_time, ":00", sep = '')))[1] == "POSIXct"){
      grid_start_time_valid_2 <- TRUE
    } else if (is.na(ymd_hms(paste(grid_start_time, ":00", sep = '')))){
      grid_start_time_valid_2 <- FALSE
    }
    
    # If the entered date passes both validation tests, assign it to 'grid_start_time_char'
    if (grid_start_time_valid_1 == TRUE &
          grid_start_time_valid_2 == TRUE ) {
      grid_start_time_char <- as.character(gsub("-", " ", grid_start_time))
      grid_start_time_char <- as.character(gsub(":", " ", grid_start_time_char))
      grid_start_time_char <- as.character(gsub("^[0-9][0-9]", "", grid_start_time_char))
    }
    
    # Ask to assign value to 'grid_end_time'
    grid_end_time <-
      readline(paste(cat("Provide a date and time for the end of grid sampling.",
                         "\n",
                         "Use the format YYYY-MM-DD HH:MM", "\n",
                         sep = '')))
    
    # Validate the input of the date and time string
    if (grepl("[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-9][0-9]",
              grid_end_time, perl = TRUE) == TRUE) {
      grid_end_time_valid_1 <- TRUE
    } else if (grepl("[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-9][0-9]",
                     grid_end_time, perl = TRUE) == FALSE) {
      grid_end_time_valid_1 <- FALSE
    }
    
    # Determine whether the supplied date/time string yields a valid POSIXct date/time
    if (class(ymd_hms(paste(grid_start_time, ":00", sep = '')))[1] == "POSIXct"){
      grid_end_time_valid_2 <- TRUE
    } else if (is.na(ymd_hms(paste(grid_start_time, ":00", sep = '')))){
      grid_end_time_valid_2 <- FALSE
    }
    
    # If the entered date passes both validation tests, assign it to 'grid_end_time_char'
    if (grid_end_time_valid_1 == TRUE &
          grid_end_time_valid_2 == TRUE ) {
      grid_end_time_char <- as.character(gsub("-", " ", grid_end_time))
      grid_end_time_char <- as.character(gsub(":", " ", grid_end_time_char))
      grid_end_time_char <- as.character(gsub("^[0-9][0-9]", "", grid_end_time_char))
    }
    
    # Ask to assign value to 'sampling_interval_type'
    sampling_interval_type <-
      readline(paste(cat("Provide the type of grid sampling to perform.",
                         "\n",
                         "Choices are: (1) averaging, (2) snapshot, or (3) maximum", "\n",
                         "Press <ENTER> to assign the 'averaging' method", "\n",
                         sep = '')))
    
    # Check user input and assign a numeric value correponding to:
    # 0 - averaging, 1 - snapshot, 2 - maximum
    if (sampling_interval_type == "") sampling_interval_type_no <- 0
    if (sampling_interval_type == "1") sampling_interval_type_no <- 0
    if (sampling_interval_type == "averaging") sampling_interval_type_no <- 0
    if (sampling_interval_type == "snapshot") sampling_interval_type_no <- 1
    if (sampling_interval_type == "2") sampling_interval_type_no <- 1
    if (sampling_interval_type == "maximum") sampling_interval_type_no <- 2
    if (sampling_interval_type == "3") sampling_interval_type_no <- 2
    
    # Ask to assign value to 'sampling_interval_rate'
    sampling_interval_rate <-
      readline(paste(cat("Provide the grid sampling measurement frequency.",
                         "\n",
                         "Use the format HH:MM", "\n",
                         "Press <ENTER> to assign a 1-hour measurement frequency", "\n",
                         sep = '')))
    
    # Validate the input of the time string
    if (grepl("[0-2][0-9]:[0-9][0-9]", sampling_interval_rate, perl = TRUE) == TRUE) {
      sampling_interval_rate_valid <- TRUE
    } else if (grepl("[0-2][0-9]:[0-9][0-9]", sampling_interval_rate, perl = TRUE) == FALSE) {
      sampling_interval_rate_valid <- FALSE
    }
    
    # If the entered date passes the validation test, assign it to 'sampling_interval_rate_char'
    if (sampling_interval_rate_valid == TRUE) {
      sampling_interval_rate_char <- as.character(gsub(":", " ", sampling_interval_rate))
    }
    
    # If the default value of 1 hour was chosen by pressing <ENTER>, assign a value
    # of "01 00" to 'sampling_interval_rate_char'
    if (sampling_interval_rate == "") {
      sampling_interval_rate_char <- "01 00"
    }
    
    # Provide a summary of the chosen options and ask if the chosen options are acceptable
    acceptable_grid <-
      readline(paste(cat("The plan. Adding grid: ", grid_name, "\n",
                         "----------------------------------", "\n", 
                         "            Grid Center: ", 
                         paste(unlist(strsplit(grid_center, " "))[1], "º, ",
                               unlist(strsplit(grid_center, " "))[2], "º",
                               sep = ''), "\n",
                         "           Grid Spacing: ",
                         paste(unlist(strsplit(grid_spacing, " "))[1], "º, ",
                               unlist(strsplit(grid_spacing, " "))[2], "º",
                               sep = ''), "\n",
                         "              Grid Span: ",
                         paste(unlist(strsplit(grid_span, " "))[1], "º, ",
                               unlist(strsplit(grid_span, " "))[2], "º",
                               sep = ''), "\n",
                         " No. of Vertical Levels: ", grid_number_vertical, "\n",
                         "           Grid Heights: ", grid_heights_string_list, " m", "\n",
                         "      Start of Sampling: ", grid_start_time, "\n",
                         "        End of Sampling: ", grid_end_time, "\n",
                         "        Sampling Method: ", sampling_interval_type_no, "\n",
                         "     Sampling Frequency: ",
                         as.numeric(unlist(strsplit(sampling_interval_rate_char, " ")))[1],
                         " h",
                         ifelse(as.numeric(unlist(strsplit(sampling_interval_rate_char, " ")))[2]
                                == 0, paste(""), paste(
                                as.numeric(unlist(strsplit(sampling_interval_rate_char, " ")))[2],
                                " m", sep = '')),
                         "\n",
                         "----------------------------------", "\n",
                         "This is what will be set. Okay? [y/n]: ",
                         sep = '')))
    
    # If acceptable, add the grid and the options to the list of grids in the
    # project folder
    if (acceptable_grid == "y"){
      cat(paste("--- Grid named: ", grid_name, ", generated on ", Sys.time(), sep = ''), "\n",
          grid_center, "\n",
          grid_spacing, "\n",
          grid_span, "\n",
          grid_folder, "\n",
          grid_filename, "\n",
          grid_number_vertical, "\n",
          grid_heights_string_list, "\n",                
          grid_start_time_char, "\n",
          grid_end_time_char, "\n",
          sampling_interval_type_no, " ", sampling_interval_rate_char, "\n",
          file = "~/Documents/SplitR/grids", append = TRUE,
          sep = '')
    }

    
    # Close grids block
  }
  
  
  
  
  # Close function
}

