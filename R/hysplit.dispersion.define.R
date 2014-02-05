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
  
  # Define emissions (fixed location, rate, hours, start time, must tie to an already
  # defined species or define a new one here)
  
  
  
  # Define sampling grids (fixed location, size/spacing, rate of sampling, sampling times)
  
  
  
}