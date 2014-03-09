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
    
