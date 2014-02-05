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
        
  
  # Define emissions (fixed location, rate, hours, start time, must tie to an already
  # defined species or define a new one here)
  
  
  
  # Define sampling grids (fixed location, size/spacing, rate of sampling, sampling times)
  
  
  
}