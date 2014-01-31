project.list <- function(display_paths = FALSE){ 
  
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
    
    # Read .SplitR_projects file as a data frame
    project_list <- read.csv(paste(SplitR_path, "/Projects/.SplitR_projects", sep = ''),
                             header = FALSE, stringsAsFactors = FALSE)
    
    # Include column names
    colnames(project_list) <- c("Project Name", "Date Created", "Location")
    
    # Either format the list of project with or without the project paths, depending
    # on 'display_paths' value
    if (display_paths == FALSE) {
      project_list_nopaths <- project_list
      project_list_nopaths[,2] <- as.POSIXct(project_list_nopaths[,2], origin = "1970-01-01")
      project_list_nopaths[,3] <- NULL
      return(project_list_nopaths)
    }
    
    if (display_paths == TRUE) {
      project_list_withpaths <- project_list
      project_list_withpaths[,2] <- as.POSIXct(project_list_withpaths[,2], origin = "1970-01-01")
      return(project_list_withpaths)
    }
    
  }
  
}