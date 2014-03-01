dispersion.preset.delete <- function(read = NULL, numbers = NULL, interactive = TRUE){
  
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
    
    # If an argument for read was not provided, prompt the user to select a preset class
    if (is.null(read)){
      read <-
        readline(paste(cat("What type of preset would you like to delete?", "\n",
                           "Choices are: (1) emissions, (2) grids, (3) species","\n",
                           "Press <ENTER> for no deletion. Otherwise, enter a number or type",
                           sep = '')))
      if (read == ""){
        stop("Nothing selected, so, no deletion can be made")
      }
      
      if (read == "1"){ 
      read <- "emissions"
      } else if (read == "2"){
        read <- "grids"  
      } else if (read == "3"){
        read <- "species"
      }
      
#       if (read %in% !c("emissions", "grids", "species")){
#         stop("Please select from \"emissions\", \"grids\", or \"species\"")
#       }
      
    }
    
    if (read == "emissions"){
      
      # Read the 'emissions' file line by line and place into a vector object
      from_file <- as.vector(read.table("~/Documents/SplitR/emissions", sep = "\n"))
      
      # Get the total number of preset entries available in the file
      number_of_entries <- nrow(from_file) / 6
      
      # Get the sequence of entries
      seq_of_entries <- 1:number_of_entries
      
      # Initialize an empty list object
      list.from_file <- vector(mode = "list", length = number_of_entries)
      
      # Cycle through blocks of presets in preset file 
      for (i in seq_of_entries){
        block <- seq(from = ((seq_of_entries[i] - 1) * 6) + 1,
                     to = ((seq_of_entries[i] - 1) * 6) + 6,
                     by = 1)
        
        vector.from_file <- as.vector(from_file[min(block):max(block),])
        
        list.from_file[[i]] <- vector.from_file
      }
      
      # Create block of oneline summaries for each of the presets
      for (i in seq_of_entries){
        if (i == 1) {
          oneline <- vector(mode = "character", length = number_of_entries)
        }
        oneline[i] <- paste("(", i, ") ",
                            gsub("^--- Emissions source named: ([a-zA-Z0-9]*),.*",
                                 "\\1",
                                 list.from_file[[i]][1]),
                            " / Rate: ",
                            list.from_file[[i]][3], " (mass units)/h",
                            " / Duration: ",
                            list.from_file[[i]][4], " h",
                            " / Release: ",
                            list.from_file[[i]][5],
                            sep = '')
      }
      
      # Create function to output oneline index calls and newline markers
      oneline_output <- function(){
        for (i in 1:number_of_entries){
          if (i == 1) {
            oneline_paste <- vector(mode = "character", length = number_of_entries)
            oneline_to_paste <- vector(mode = "character", length = 0)
          }
          oneline_paste[i] <- paste(oneline[i], "\n", sep = '')
          oneline_to_paste <- c(oneline_to_paste, oneline_paste[i]) 
        }
        
        return(oneline_to_paste)
      }
      
      # Display list of presets to remove
      preset_to_remove <-
        readline(paste(cat("Here are the current presets for emissions", "\n",
                           "------------------------------------------", "\n",
                           paste(oneline_output()),
                           "------------------------------------------", "\n",
                           "Which preset number should be deleted?", "\n",
                           "Press <ENTER> for no deletion. Otherwise, enter a number. ",
                           sep = '')))
      
      # Stop function if only <ENTER> was pressed (no deletion case)
      if (preset_to_remove == ""){
        stop("No preset number was provided, so, no deletion was made")
      }
      
      # Verify that the input is a single numeric value
      preset_numeric <- as.numeric(preset_to_remove)
      
      # Verify that the input is a number within the range of valid numbers  
      in_range <- ifelse(preset_numeric %in% seq_of_entries,
                         TRUE, FALSE)
      
      # Remove the preset from the local copy of all presets
      list.from_file[[as.numeric(preset_to_remove)]] <- NULL
      
      # Rewrite the list of presets to the preset file from the modified local copy of presets
      # (containing all presets except the one that was removed)
      write.table(unlist(list.from_file), "~/Documents/SplitR/emissions", sep = "\n",
                  quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
    
    if (read == "grids"){
      
      # Read the 'grids' file line by line and place into a vector object
      from_file <- as.vector(read.table("~/Documents/SplitR/grids", sep = "\n"))
      
      # Get the total number of preset entries available in the file
      number_of_entries <- nrow(from_file) / 12
      
      # Get the sequence of entries
      seq_of_entries <- 1:number_of_entries
      
      # Initialize an empty list object
      list.from_file <- vector(mode = "list", length = number_of_entries)
      
      # Cycle through blocks of presets in preset file
      for (i in seq_of_entries){
        block <- seq(from = ((seq_of_entries[i] - 1) * 12) + 1,
                     to = ((seq_of_entries[i] - 1) * 12) + 12,
                     by = 1)
        
        vector.from_file <- as.vector(from_file[min(block):max(block),])
        
        list.from_file[[i]] <- vector.from_file
      }
    
    # Create block of oneline summaries for each of the presets
    for (i in seq_of_entries){
      if (i == 1) {
        oneline <- vector(mode = "character", length = number_of_entries)
      }
      oneline[i] <- paste("(", i, ") ",
                          gsub("^--- Grid named: ([a-zA-Z0-9]*),.*",
                               "\\1",
                               list.from_file[[i]][1]),
                          " / C: ",
                          gsub("$", "º", gsub(" ", "º, ", list.from_file[[i]][2])),
                          " / I: ",
                          gsub("$", "º", gsub(" ", "º, ", list.from_file[[i]][3])),
                          " / S: ",
                          gsub("$", "º", gsub(" ", "º, ", list.from_file[[i]][4])),
                          " / ", list.from_file[[i]][7], " lv",
                          " / s->e: ",
                          list.from_file[[i]][9], " - ", list.from_file[[i]][10],
                          " / avg: ",
                          list.from_file[[i]][11],
                          sep = '')
    }
    
    # Create function to output oneline index calls and newline markers
    oneline_output <- function(){
      for (i in 1:number_of_entries){
        if (i == 1) {
          oneline_paste <- vector(mode = "character", length = number_of_entries)
          oneline_to_paste <- vector(mode = "character", length = 0)
        }
        oneline_paste[i] <- paste(oneline[i], "\n", sep = '')
        oneline_to_paste <- c(oneline_to_paste, oneline_paste[i]) 
      }
      
      return(oneline_to_paste)
    }
    
    # Display list of presets to remove
    preset_to_remove <-
      readline(paste(cat("Here are the current presets for grids", "\n",
                         "--------------------------------------", "\n",
                         paste(oneline_output()),
                         "--------------------------------------", "\n",
                         "Which preset number should be deleted?", "\n",
                         "Press <ENTER> for no deletion. Otherwise, enter a number. ",
                         sep = '')))
    
    # Stop function if only <ENTER> was pressed (no deletion case)
    if (preset_to_remove == ""){
      stop("No preset number was provided, so, no deletion was made")
    }
    
    # Verify that the input is a single numeric value
    preset_numeric <- as.numeric(preset_to_remove)
    
    # Verify that the input is a number within the range of valid numbers  
    in_range <- ifelse(preset_numeric %in% seq_of_entries,
                       TRUE, FALSE)
    
    # Remove the preset from the local copy of all presets
    list.from_file[[as.numeric(preset_to_remove)]] <- NULL
    
    # Rewrite the list of presets to the preset file from the modified local copy of presets
    # (containing all presets except the one that was removed)
    write.table(unlist(list.from_file), "~/Documents/SplitR/grids", sep = "\n",
                quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    }
    
    if (read == "species"){
      
      # Read the 'species' file line by line and place into a vector object
      from_file <- as.vector(read.table("~/Documents/SplitR/species", sep = "\n"))
      
      # Get the total number of preset entries available in the file
      number_of_entries <- nrow(from_file) / 7
      
      # Get the sequence of entries
      seq_of_entries <- 1:number_of_entries
      
      # Initialize an empty list object
      list.from_file <- vector(mode = "list", length = number_of_entries)
      
      # Cycle through blocks of presets in preset file
      for (i in seq_of_entries){
        block <- seq(from = ((seq_of_entries[i] - 1) * 7) + 1,
                     to = ((seq_of_entries[i] - 1) * 7) + 7,
                     by = 1)
        
        vector.from_file <- as.vector(from_file[min(block):max(block),])
        
        list.from_file[[i]] <- vector.from_file
      } 
      
      # End species deletion
    }
    
    # End interactive section
  }
  
  # End of function
}


    


