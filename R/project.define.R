project.define <- function(project_name,
                           install_folders = FALSE,
                           install_in_documents = TRUE,
                           other_install_path){
  
  require(lubridate)
  
  # This function allows for the creation of a new project folder
  # It also allows for the setting of global options that apply to all projects
  
  # Test data
  project_name <- "test-project"
  install_folders = FALSE
  install_in_documents = TRUE
  #other_install_path = ""
  
  #---- Create folder structure
  #
  # 
  
  #Check existence of Documents folder in user home folder
  if(length(grep("/Documents$",
                 list.dirs(path = "~", full.names = TRUE,
                           recursive = FALSE))) > 0) {
    documents_folder_path <- paste("~", "/Documents", sep = '')
  }
  
  # Check existence of SplitR directory and create if doesn't exist
  SplitR_dir_exists <- ifelse(length(list.dirs(path = paste(documents_folder_path,
                                                            "/SplitR", sep = ''),
                                               full.names = TRUE, recursive = FALSE)) > 0, 
                              TRUE, FALSE)
  
  if(SplitR_dir_exists == FALSE) dir.create(file.path(paste(documents_folder_path,
                                                            "/SplitR", sep = '')),
                                            showWarnings = FALSE)
  
  # Set new SplitR directory as the working directory
  setwd(file.path(paste(documents_folder_path, "/SplitR", sep = '')))
  
  # Set path of SplitR directory
  SplitR_path <- file.path(paste(documents_folder_path, "/SplitR", sep = ''))
  
  # If the SplitR directory didn't exist, create the other subfolders (Exec, Met, Projects)
  if(SplitR_dir_exists == FALSE){
    dir.create(file.path(paste(documents_folder_path, "/SplitR/Exec", sep = '')),
               showWarnings = FALSE)
    dir.create(file.path(paste(documents_folder_path, "/SplitR/Met", sep = '')),
               showWarnings = FALSE)
    dir.create(file.path(paste(documents_folder_path, "/SplitR/Projects", sep = '')),
               showWarnings = FALSE)  
  }
  #
  #
  #---- Create folder structure
  
  #---- Create project folder
  #
  #
  
  #
  #
  #---- Create project folder
  
  
  
}