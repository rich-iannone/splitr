dispersion.preset.delete <- function(read, numbers = NULL, interactive = TRUE){
  
  # Validate supplied digits for duplicates
  if (!is.null(numbers)){
    no_duplicates <- ifelse(anyDuplicated(numbers) == 0,
                            TRUE, FALSE)
    
    # If duplicated values are found, stop the function with a message
    if (no_duplicates == FALSE) {
      stop("The provided presets contain duplicate values.")
    }
    
  }
  
  # Begin interactive section
  if (interactive == TRUE){
    
    if (read == "emissions"){
      
      from_file <- as.vector(read.table("~/Documents/SplitR/emissions", sep = "\n"))
      
      number_of_entries <- nrow(from_file) / 6
      
      seq_of_entries <- 1:number_of_entries
      
      list.from_file <- vector(mode = "list", length = number_of_entries)
      
      # Cycle through 
      for (i in seq_of_entries){
        block <- seq(from = ((seq_of_entries[i] - 1) * 6) + 2,
                     to = ((seq_of_entries[i] - 1) * 6) + 5,
                     by = 1)
        
        vector.from_file <- as.vector(from_file[min(block):max(block),])
        
        list.from_file[[i]] <- vector.from_file
      }
    }
    
    if (read == "grids"){
      
      from_file <- as.vector(read.table("~/Documents/SplitR/grids", sep = "\n"))
      
      number_of_entries <- nrow(from_file) / 12
      
      seq_of_entries <- 1:number_of_entries
      
      list.from_file <- vector(mode = "list", length = number_of_entries)
      
      # Cycle through 
      for (i in seq_of_entries){
        block <- seq(from = ((seq_of_entries[i] - 1) * 12) + 2,
                     to = ((seq_of_entries[i] - 1) * 12) + 11,
                     by = 1)
        
        vector.from_file <- as.vector(from_file[min(block):max(block),])
        
        list.from_file[[i]] <- vector.from_file
      } 
    }
    
    if (read == "species"){
      
      from_file <- as.vector(read.table("~/Documents/SplitR/species", sep = "\n"))
      
      number_of_entries <- nrow(from_file) / 7
      
      seq_of_entries <- 1:number_of_entries
      
      list.from_file <- vector(mode = "list", length = number_of_entries)
      
      # Cycle through 
      for (i in seq_of_entries){
        block <- seq(from = ((seq_of_entries[i] - 1) * 7) + 2,
                     to = ((seq_of_entries[i] - 1) * 7) + 6,
                     by = 1)
        
        vector.from_file <- as.vector(from_file[min(block):max(block),])
        
        list.from_file[[i]] <- vector.from_file
      } 
      
      # End species deletion
    }
    
  # End of function
}


    


