dispersion.preset.list <- function(read = NULL, search = NULL){
  
  # If an argument for read was not provided, prompt the user to select a preset class
  if (is.null(read)){
    read <-
      readline(paste(cat("Which preset type would you like to list?", "\n",
                         "Choices are: (1) emissions, (2) grids, (3) species","\n",
                         "Press <ENTER> to exit",
                         sep = '')))
    if (read == ""){
      stop("Nothing selected so no preset listing is provided.")
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
    
    # Get a list of listing numbers from the search string
    if (!is.null(search)){
      
      for (i in 1:length(list.from_file)){
        if (i == 1) names <- vector(mode = "character", length = 0)
        the_name <- list.from_file[[i]][1]
        the_name <- gsub("^.*: (.*),.*", "\\1", the_name)
        names <- c(names, the_name)
      }
      
      # If a search term provided, search for that name in 'names'
      for (i in 1:length(names)){
        if (search == names[i]) list_number <- i
      }
      
      if (exists("list_number")) return(list_number)
      
      if (!exists("list_number")) stop("The search term was not found in the preset list.")
      
    }
    
    if (is.null(search)){
      
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
      
      return(oneline_to_paste)
    }
    
    # Display list of emissions presets
    paste(cat("Here are the current presets for emissions", "\n",
                         "------------------------------------------", "\n",
                         paste(oneline_output()),
                         "------------------------------------------", "\n",
                         "\n", sep = ''))

    # End emissions list
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
    
    # Extract the listing number from the search string
    print(list.from_file)
    
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
    
    # Display list of grids presets
      paste(cat("Here are the current presets for grids", "\n",
                         "--------------------------------------", "\n",
                         paste(oneline_output()),
                         "--------------------------------------", "\n",
                         "\n", sep = ''))

    # End grids list
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
    
    # Extract the listing number from the search string
    print(list.from_file)
    
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
    
    # Display list of species presets
      paste(cat("Here are the current species presets", "\n",
                         "------------------------------------", "\n",
                         paste(oneline_output()),
                         "------------------------------------", "\n",
                         "\n", sep = ''))

    # End species list
  }
  
  # End of function
}
