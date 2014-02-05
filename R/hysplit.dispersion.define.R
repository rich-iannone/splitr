hysplit.dispersion.define <- function(define){
  
  
  # Define species (names, properties)
  if (define == 'species'){
    
    # Ask for the name of the species
    species_name <-
      readline("What is the name of the species? ")
    
    # Ask whether it's a gas-phase species or a particle
    species_type <-
      readline("Is the species a gas or particle? [gas/particle] ")
  
  # Define emissions (fixed location, rate, hours, start time, must tie to an already
  # defined species or define a new one here)
  
  
  
  # Define sampling grids (fixed location, size/spacing, rate of sampling, sampling times)
  
  
  
}