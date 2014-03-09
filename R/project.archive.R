project.archive <- function( ){ 
  
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
  
  # Perform remainder of script if the SplitR folder exists
  if (SplitR_dir_exists == TRUE) {
    
    # Get path to SplitR folder
    SplitR_path <- file.path(paste(documents_folder_path, "/SplitR", sep = ''))
    
    # Determine if SplitR.projects file exists
    if ("SplitR.projects" %in% list.files(path = paste(SplitR_path, "/Projects/", sep = ''))){
      NULL
    } else {
      cat(file = paste(file.path(paste(SplitR_path, "/Projects/", sep = '')),
                       "SplitR.projects", sep = ''),
          append = FALSE)
    }
    
    # If there is data in the 'SplitR.projects' file, read it and display a list
    if (file.info(paste(SplitR_path, "/Projects/SplitR.projects", sep = ''))[1,1] > 0){
      
      # Read SplitR.projects file as a data frame
      project_list <- read.csv(paste(SplitR_path, "/Projects/SplitR.projects", sep = ''),
                               header = FALSE, stringsAsFactors = FALSE)
      
      # Include column names
      colnames(project_list) <- c("Project Name", "Date Created", "Location")
      
      # Either format the list of project with or without the project paths, depending
      # on 'display_paths' value
      project_list_nopaths <- project_list
      project_list_nopaths[,2] <- as.POSIXct(project_list_nopaths[,2], origin = "1970-01-01")
      project_list_nopaths[,3] <- NULL
      
      # Ask which project should be archived
      print(project_list_nopaths)
      project_number_to_archive <-
        readline(paste("Which project number would you like to archive? "))
      project_number_to_archive <- as.numeric(project_number_to_archive)
