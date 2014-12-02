#' Remove dispersion preset information
#' @description The function allows for the deletion of presets specific to dispersion runs. Presets are effectively blocks of parameters that are stored on the disk. The presets are divided into three groups: "emissions", "grids", and "species". Each preset has a plaintext file that resides on disk of the same name (located in the SplitR folder). This function by default runs in an interactive manner, prompting the user to select the preset to list from a condensed summary of presets of a given type. The function can also be called in a non-interactive manner, where specific presets of a type can be deleted without feedback. For adding presets to the associated files, the interactive function 'dispersion.preset.add()' can be used.
#' @param read the type of preset to delete. The three preset types are "emissions", "grids", and "species". Only a single type can be specified. If the argument for interactive remains TRUE (as in the default), the function will interactively prompt the user for the preset type if none is supplied. The preset must be supplied if the function is to be used in a non-interactive manner (futhermore, a single numeric value or numeric vector must be provided as the 'numbers' argument if 'interactive' is set to FALSE).
#' @param numbers the values corresponding to the presets of a specified type (provided as the 'read' character argument) to be deleted. The argument is to be provided as a single numeric value or numeric vector. Any values provided for this argument will be ignored if 'interactive' is set to TRUE (the default). The numbers correspond to the order in which the presets appear in each of the 'emissions', 'grids', or 'species' plaintext files.
#' @param interactive determines whether the function should run in an interactive mode, prompting the for input on which presets to delete. One-line summaries of presets for a specific type are presented to guide the user on which items are available for deletion. If the interactive mode is selected (it is the default mode), no values for the 'numbers' need to be supplied.
#' @param path_wd a full path should be provided for the HYSPLIT working directory since presets will normally reside in this folder.
#' @export dispersion.preset.delete
#' @examples
#' \dontrun{
#' # Delete an 'emissions' preset
#' dispersion.preset.delete("emissions")
#' 
#' # Delete a 'grids' preset
#' dispersion.preset.delete("grids")
#' 
#' # Delete a 'species' preset
#' dispersion.preset.delete("species")
#' }

dispersion.preset.delete <- function(read = NULL,
                                     numbers = NULL,
                                     interactive = TRUE,
                                     path_wd){
  
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
      
    }
    
    if (read == "emissions"){
      
      # Read the 'emissions' file line by line and place into a vector object
      from_file <- as.vector(read.table(paste(path_wd, "emissions", sep = ''), sep = "\n"))
      
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
      write.table(unlist(list.from_file), paste(path_wd, "emissions", sep = ''), sep = "\n",
                  quote = FALSE, row.names = FALSE, col.names = FALSE)
      
      # End emissions deletion
    }
    
    if (read == "grids"){
      
      # Read the 'grids' file line by line and place into a vector object
      from_file <- as.vector(read.table(paste(path_wd, "grids", sep = ''), sep = "\n"))
      
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
      write.table(unlist(list.from_file), paste(path_wd, "grids", sep = ''), sep = "\n",
                  quote = FALSE, row.names = FALSE, col.names = FALSE)
      
      # End grids deletion
    }
    
    if (read == "species"){
      
      # Read the 'species' file line by line and place into a vector object
      from_file <- as.vector(read.table(paste(path_wd, "species", sep = ''), sep = "\n"))
      
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
      
      # Create block of oneline summaries for each of the presets
      for (i in seq_of_entries){
        if (i == 1) {
          oneline <- vector(mode = "character", length = number_of_entries)
        }
        oneline[i] <- paste("(", i, ") ",
                            gsub("^--- Species named: ([a-zA-Z0-9]*),.*",
                                 "\\1",
                                 list.from_file[[i]][1]),
                            " / Particle: ",
                            list.from_file[[i]][2],
                            " / DDep: ",
                            list.from_file[[i]][3],
                            " / WDep: ",
                            list.from_file[[i]][4],
                            " / RD: ",
                            list.from_file[[i]][5],
                            " / RS: ",
                            list.from_file[[i]][6],
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
        readline(paste(cat("Here are the current species presets", "\n",
                           "------------------------------------", "\n",
                           paste(oneline_output()),
                           "------------------------------------", "\n",
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
      write.table(unlist(list.from_file), paste(path_wd, "species", sep = ''), sep = "\n",
                  quote = FALSE, row.names = FALSE, col.names = FALSE)
      
      # End species deletion
    }
    
    # End interactive section
  }
  
  # End of function
}
