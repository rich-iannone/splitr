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
      
