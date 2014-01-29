SplitR.init <- function(install_folders = TRUE,
                        install_in_documents = TRUE,
                        other_install_path){

  
  #---- Check for existence and path of SplitR folder
  #
  # 
  
  # Check for existence of Documents folder in user home folder
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
  
  # If the SplitR folder exists, return its path
  if (SplitR_dir_exists == TRUE) {
    SplitR_path <- file.path(paste(documents_folder_path, "/SplitR", sep = ''))
  }
  
  #
  #
  #---- Check for existence and path of SplitR folder
  
  #---- Create folder structure and initialize .SplitR file
  #
  # 
  
  # If it the SplitR folder doesn't exist, crete the folder tree in ~/Documents
  if(SplitR_dir_exists == FALSE & install_folders == TRUE) {
    dir.create(file.path(paste(documents_folder_path,
                               "/SplitR", sep = '')),
               showWarnings = FALSE)
    
    # Set path of SplitR directory
    SplitR_path <- file.path(paste(documents_folder_path, "/SplitR", sep = ''))
    
    # Subfolder creation (Exec, Met, Projects)
    dir.create(file.path(paste(documents_folder_path, "/SplitR/Exec", sep = '')),
               showWarnings = FALSE)
    dir.create(file.path(paste(documents_folder_path, "/SplitR/Met", sep = '')),
               showWarnings = FALSE)
    dir.create(file.path(paste(documents_folder_path, "/SplitR/Projects", sep = '')),
               showWarnings = FALSE)  
  
    # Get date and time of initialization
    create_time <- Sys.time()

    # Get executable files
    SplitR.get.exec()
    
    # Initialize the .SplitR plaintext file
    cat("paths", "-----",
        paste(documents_folder_path, "/SplitR/Exec", sep = ''),
        paste(documents_folder_path, "/SplitR/Met", sep = ''),
        paste(documents_folder_path, "/SplitR/Projects", sep = ''),
        "platform", "--------",
        paste(Sys.info()[['sysname']], ", release ", Sys.info()[['release']], 
              " (", Sys.info()[['machine']], ")", sep = ''),
        "executables", "-----------",
        paste("Available in '/SplitR/Exec' folder"),
        "met files", "---------",
        "log", "---",
        paste("Folders were created on: ", create_time, sep = ''),
        file = paste(SplitR_path, "/.SplitR", sep = ''),
        sep = "\n")
  
  }
  
  #
  #
  #---- Create folder structure and initialize .SplitR file
  
  # Access the newly created .SplitR file
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
                                
  # Close function
}
