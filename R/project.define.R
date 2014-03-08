project.define <- function(project_name){
  
  # Add require statement
  require(lubridate)
  
  # This function allows for the creation of a new project folder
  # It also allows for the setting of global options that apply to all projects
  

  
  #---- Check for existance and path of SplitR folder
  #
  # 
  
  # Check existence of Documents folder in user home folder
  if(length(grep("/Documents$",
                 list.dirs(path = "~", full.names = TRUE,
                           recursive = FALSE))) > 0) {
    documents_folder_path <- paste("~", "/Documents", sep = '')
  }
  
  # Check existence of SplitR folder
  SplitR_dir_exists <- ifelse(length(list.dirs(path = paste(documents_folder_path,
                                                            "/SplitR", sep = ''),
                                               full.names = TRUE, recursive = FALSE)) > 0, 
                              TRUE, FALSE)
  
  # If the SplitR folder exists, return its path
  SplitR_path <- file.path(paste(documents_folder_path, "/SplitR", sep = ''))
  
  #
  #
  #---- Check for existance and path of SplitR folder
  
  #---- Create folder structure
  #
  # 
  
  # If it the SplitR folder doesn't exist, crete the folder tree in ~/Documents
  if(SplitR_dir_exists == FALSE) SplitR.init()

  #
  #
  #---- Create folder structure
  
  #---- Create project folder
  #
  #
  
  # Check to see if the provided project name is a duplicate of existing project names
  if (file.info(paste(SplitR_path, "/Projects/SplitR.projects", sep = ''))[1,1] > 0){
    if (tolower(project_name) %in% tolower(project.list()[,1])) {
      stop("That name is already in the project list. Choose another name")
    }
  }
  
  # Construct a long project name based on the input string and date/time of creation 
  project_create_date <- Sys.time()
    
  create_date_string <-  paste(year(project_create_date), "-",
                               month(project_create_date), "-",
                               day(project_create_date), " (",
                               hour(project_create_date), "_",
                               minute(project_create_date), "_",
                               round(second(project_create_date), digits = 0), ")",
                               sep = '')
  
  long_project_name <- paste(project_name, "_",
                             create_date_string,
                             sep = '')
  
  # Create the new project directory in the SplitR/Projects folder
  dir.create(file.path(paste(SplitR_path, "/Projects/", long_project_name, sep = '')),
             showWarnings = FALSE)
  
  #
  #
  #---- Create project folder
  
  #---- Add to SplitR.projects document
  #
  #
  cat(project_name, ",",
      project_create_date, ",\"",
      file.path(paste(SplitR_path, "/Projects/", long_project_name, sep = '')), "\"\n",
      sep = '',
      file = paste(file.path(paste(SplitR_path, "/Projects/", sep = '')),
                   ".SplitR_projects", sep = ''),
      append = TRUE)
  #
  #  
  #---- Add to .SplitR_projects document
  
}