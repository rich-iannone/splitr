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
      
  # End of function
}


    


