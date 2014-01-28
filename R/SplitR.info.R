SplitR.info.edit <- function(mode = "add",
                            heading,
                            string,
                            line_number){
  
  # Number each of the headings using a switch statement
  heading_order <- switch(heading,
                          paths = 1,
                          platform = 2,
                          executables = 3,
                          met = 4,
                          log = 5)
  
  # Get Documents folder path (in user's home folder)
  if(length(grep("/Documents$",
                 list.dirs(path = "~", full.names = TRUE,
                           recursive = FALSE))) > 0) {
    documents_folder_path <- paste("~", "/Documents", sep = '')
  }
  
  # Check for existence of SplitR folder
  SplitR_dir_exists <- 
    ifelse(length(list.dirs(path = paste(documents_folder_path,
                                         "/SplitR", sep = ''),
                            full.names = TRUE, recursive = FALSE)) > 0, TRUE, FALSE)
  
  # Get path of SplitR folder
  if (SplitR_dir_exists == TRUE) {
    SplitR_path <- file.path(paste(documents_folder_path, "/SplitR", sep = ''))
  }
  
  # Check for existence of .SplitR file
  SplitR_file_exists <- file.exists(paste(SplitR_path, "/.SplitR", sep = ''))
  
  # Get path of .SplitR file
  if (SplitR_file_exists == TRUE) {
    SplitR_file_path <- file.path(paste(SplitR_path, "/.SplitR", sep = ''))
  }
  
  # Access the .SplitR file
  SplitR_file <- file(paste("file://",
                            system("cd ~ ; pwd", intern = TRUE),
                            gsub("~", "", SplitR_path),
                            "/.SplitR", sep = ''))
  
  # Get .SplitR file as character vector
  SplitR_file_text <- readLines(SplitR_file)
  
  # Check that the 'SplitR_file_text' object was created
  if (exists("SplitR_file_text")) SplitR_file_text_created <- TRUE
  
  # Close and destroy the connection to file
  close(SplitR_file)
  
  # Find vector positions of headings in .SplitR file
  for (i in 1:length(SplitR_file_text)) {
    if (i == 1) positions <- mat.or.vec(nr = 5, nc = 1)
    if (grepl("paths", SplitR_file_text[i]) == TRUE) positions[1] <- i
    if (grepl("platform", SplitR_file_text[i]) == TRUE) positions[2] <- i
    if (grepl("executables", SplitR_file_text[i]) == TRUE) positions[3] <- i
    if (grepl("met files", SplitR_file_text[i]) == TRUE) positions[4] <- i
    if (grepl("log", SplitR_file_text[i]) == TRUE) positions[5] <- i
  }
  
  # Add text to log
  if (mode == "add") {
    # Function for inserting a vector element at an arbitrary position in a vector
    insert.at <- function(a, pos, ...){
      dots <- list(...)
      stopifnot(length(dots) == length(pos))
      result <- vector("list", 2 * length(pos) + 1)
      result[c(TRUE, FALSE)] <- split(a, cumsum(seq_along(a) %in% (pos + 1)))
      result[c(FALSE, TRUE)] <- dots
      unlist(result)
    }
    
    # Insert string just below heading
    SplitR_file_text <- insert.at(SplitR_file_text, positions[heading_order] + 1, string)
    
    # Overwrite the original .SplitR file with new one
    cat(SplitR_file_text, file = SplitR_file_path, sep = "\n")
  }
  
  # Remove text from log
  if (mode == "remove") {
    # Function for removing a vector element from an arbitrary position in a vector
    remove.from <- function(a, heading, line_number){
      a <- SplitR_file_text
      remove <- SplitR_file_text[positions[heading_order] + line_number + 1]
      a[!a %in% remove]
    }
    
    # Remove string a set number of lines below heading
    SplitR_file_text <- remove.from(SplitR_file_text,
                                    heading = heading,
                                    line_number)
    
    # Overwrite the original .SplitR file with new one
    cat(SplitR_file_text, file = SplitR_file_path, sep = "\n")
  }
  
  # Close function
}


