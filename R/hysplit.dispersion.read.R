hysplit.dispersion.read <- function(read, numbers){
    
  # Validate supplied digits for duplicates
  no_duplicates <- ifelse(anyDuplicated(numbers) == 0,
                          TRUE, FALSE)
  
  # If duplicated values are found, stop the function with a message
  if (no_duplicates == FALSE) {
    stop("The provided emissions presets contain duplicate values.")
  }
  
  
  # Read in and extract values from the different preset files that are available
  # These preset files are: 'emissions', 'grids', and 'species'
  
  # Reading and extracting elements from 'emissions' file
  if (read == "emissions" & no_duplicates == TRUE) {
    
    if (length(numbers) == 1){
    
    from_file <- as.vector(read.table("~/Documents/SplitR/emissions", sep = "\n"))
    
    number_of_entries <- nrow(from_file) / 6
    
    # Validate supplied digits for being within range
    in_range <- ifelse(all(numbers %in% 1:number_of_entries) == TRUE,
                       TRUE, FALSE)
    
    if (in_range == FALSE) {
      stop("The provided emissions preset is not within the range of available presets.")
    }
    
    # Get the numbered block
    block <- seq(from = ((numbers - 1) * 6) + 2,
                 to = ((numbers - 1) * 6) + 5,
                 by = 1)
    
    # Get vector of data elements for CONTROL script
    emissions <- c(1, as.vector(from_file[min(block):max(block),]))
    
    return(emissions)
    
    }
    
    if (length(numbers) > 1){
      
      from_file <- as.vector(read.table("~/Documents/SplitR/emissions", sep = "\n"))
      
      number_of_entries <- nrow(from_file) / 6
      
      # Get the numbered emissions blocks
      number_of_requests <- length(numbers)
      
      # Initialize 'emissions.list' list with appropriate length
      list.from_file <- vector(mode = "list", length = number_of_requests)
      
      # Cycle through 
      for (i in 1:number_of_requests){
        block <- seq(from = ((numbers[i] - 1) * 6) + 2,
                               to = ((numbers[i] - 1) * 6) + 5,
                               by = 1)
        
        vector.from_file <- as.vector(from_file[min(block):max(block),])
        
        list.from_file[[i]] <- vector.from_file
      }
      
      # Get vector of emisisons data elements for CONTROL script
      emissions <- c(number_of_requests, unlist(list.from_file))
      
      return(emissions)
 
    }
    
    # End emissions extraction
  }
      
  # Reading and extracting elements from 'grids' file
  if (read == "grids" & no_duplicates == TRUE) {
    
    if (length(numbers) == 1){
      
      from_file <- as.vector(read.table("~/Documents/SplitR/grids", sep = "\n"))
      
      number_of_entries <- nrow(from_file) / 12
      
      # Validate supplied digits for being within range
      in_range <- ifelse(all(numbers %in% 1:number_of_entries) == TRUE,
                         TRUE, FALSE)
      
      if (in_range == FALSE) {
        stop("The provided grid preset is not within the range of available presets.")
      }
      
      # Get the numbered block
      block <- seq(from = ((numbers - 1) * 12) + 2,
                   to = ((numbers - 1) * 12) + 11,
                   by = 1)
      
      # Get vector of data elements for CONTROL script
      grids <- c(1, as.vector(from_file[min(block):max(block),]))
      
      return(grids)
      
    }
    
    if (length(numbers) > 1){
      
      from_file <- as.vector(read.table("~/Documents/SplitR/grids", sep = "\n"))
      
      number_of_entries <- nrow(from_file) / 12
      
      # Get the numbered emissions blocks
      number_of_requests <- length(numbers)
      
      # Initialize 'list.from_file' list with appropriate length
      list.from_file <- vector(mode = "list", length = number_of_requests)
      
      # Cycle through 
      for (i in 1:number_of_requests){
        block <- seq(from = ((numbers[i] - 1) * 12) + 2,
                               to = ((numbers[i] - 1) * 12) + 11,
                               by = 1)
        
        vector.from_file <- as.vector(from_file[min(block):max(block),])
        
        list.from_file[[i]] <- vector.from_file 
      }
      
      # Get vector of data elements for CONTROL script
      grids <- c(number_of_requests, unlist(list.from_file))
      
      return(grids)
      
    }
    
    # End grids extraction
  }
  
  # Reading and extracting elements from 'species' file
  if (read == "species" & no_duplicates == TRUE) {
    
    if (length(numbers) == 1){
      
      from_file <- as.vector(read.table("~/Documents/SplitR/species", sep = "\n"))
      
      number_of_entries <- nrow(from_file) / 7
      
      # Validate supplied digits for being within range
      in_range <- ifelse(all(numbers %in% 1:number_of_entries) == TRUE,
                         TRUE, FALSE)
      
      if (in_range == FALSE) {
        stop("The provided grid preset is not within the range of available presets.")
      }
      
      # Get the numbered block
      block <- seq(from = ((numbers - 1) * 7) + 2,
                   to = ((numbers - 1) * 7) + 6,
                   by = 1)
      
      # Get vector of data elements for CONTROL script
      species <- c(1, as.vector(from_file[min(block):max(block),]))
      
      return(species)
      
    }
    
    if (length(numbers) > 1){
      
      from_file <- as.vector(read.table("~/Documents/SplitR/species", sep = "\n"))
      
      number_of_entries <- nrow(from_file) / 7
      
      # Get the numbered emissions blocks
      number_of_requests <- length(numbers)
      
      # Initialize 'list.from_file' list with appropriate length
      list.from_file <- vector(mode = "list", length = number_of_requests)
      
      # Cycle through 
      for (i in 1:number_of_requests){
        block <- seq(from = ((numbers[i] - 1) * 7) + 2,
                     to = ((numbers[i] - 1) * 7) + 6,
                     by = 1)
        
        vector.from_file <- as.vector(from_file[min(block):max(block),])
        
        list.from_file[[i]] <- vector.from_file 
      }
      
      # Get vector of data elements for CONTROL script
      species <- c(number_of_requests, unlist(list.from_file))
      
      return(species)
      
    }
    
    # End grids extraction
  }
  
  # End of function
}
  
