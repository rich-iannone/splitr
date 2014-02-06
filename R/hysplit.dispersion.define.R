hysplit.dispersion.define <- function(define){
  
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
  
  }
  
  # Define grids (fixed location, size/spacing, rate of sampling, sampling times)
  if (define == 'grids'){
    
    
  }
  
  # Close function
}

