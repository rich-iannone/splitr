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
    if (read == "grids"){
      
      from_file <- as.vector(read.table("~/Documents/SplitR/grids", sep = "\n"))
      
      number_of_entries <- nrow(from_file) / 12
      
      seq_of_entries <- 1:number_of_entries
      
      list.from_file <- vector(mode = "list", length = number_of_entries)
      
    if (read == "species"){
      
      from_file <- as.vector(read.table("~/Documents/SplitR/species", sep = "\n"))
      
      number_of_entries <- nrow(from_file) / 7
      
      seq_of_entries <- 1:number_of_entries
      
      list.from_file <- vector(mode = "list", length = number_of_entries)
      
  # End of function
}


    


