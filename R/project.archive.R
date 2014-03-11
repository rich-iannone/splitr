project.archive <- function(project_name = NULL){ 
  
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
      
      # Read 'SplitR.projects' file as a data frame
      project_list <- read.csv(paste(SplitR_path, "/Projects/SplitR.projects", sep = ''),
                               header = FALSE, stringsAsFactors = FALSE)
      
      # Include column names
      colnames(project_list) <- c("Project Name", "Date Created", "Location")
      
      # Format the list of project without the absolute project paths
      project_list_nopaths <- project_list
      project_list_nopaths[,2] <- as.POSIXct(project_list_nopaths[,2], origin = "1970-01-01")
      project_list_nopaths[,3] <- NULL
      
      # Ask which project should be archived
      print(project_list_nopaths)
      project_number_to_archive <-
        readline(paste("Which project number would you like to archive? "))
      project_number_to_archive <- as.numeric(project_number_to_archive)
      
      # Create an 'Archive' folder if one doesn't already exist in the 'SplitR' folder
      if ("Archive" %in% list.dirs(path = SplitR_path, full.names = FALSE, recursive = FALSE)){
        NULL
      } else {
        system(paste("cd ", SplitR_path, " ; ", "mkdir Archive", sep = ''))
      }
      
      # Get absolute path of project folder to archive
      absolute_path_origin <- project_list[project_number_to_archive,3]
      
      # Get folder name of project to archive
      folder_name_project_to_archive <- gsub("^.*/(.*)$", "\\1", absolute_path_origin)
      
      # Determine that there are files to move before performing the move
      if (length(list.files(path = absolute_path_origin, pattern = NULL,
                            all.files = TRUE, full.names = FALSE,
                            recursive = FALSE, ignore.case = FALSE,
                            include.dirs = FALSE, no.. = TRUE)) != 0){
        
        # Move contents of archived project to the 'Archive' folder
        system(paste("cd ~; cd Documents/SplitR/Archive ; mkdir '", folder_name_project_to_archive,
                     "' ; cd ../Projects ; cp '", folder_name_project_to_archive, "/*' '../Archive/",
                     folder_name_project_to_archive, "'",
                     sep = ''))
        
        # Delete project entry in the 'SplitR.projects' file
        SplitR_projects <- read.csv(paste(SplitR_path, "/Projects/SplitR.projects", sep = ''),
                                    header = FALSE, stringsAsFactors = FALSE)
        SplitR_projects <- SplitR_projects[-project_number_to_archive,]
        
        write.table(SplitR_projects,
                    file = paste(file.path(paste(SplitR_path, "/Projects/", sep = '')),
                                 "SplitR.projects", sep = ''), sep = ",",
                    append = FALSE, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        # Add an object to indicate that a project was removed
        project_removed <- TRUE
        
      }
      
      if (length(list.files(path = absolute_path_origin, pattern = NULL,
                            all.files = TRUE, full.names = FALSE,
                            recursive = FALSE, ignore.case = FALSE,
                            include.dirs = FALSE, no.. = TRUE)) == 0){
        
        # Remove empty folder from the 'Projects' folder
        system(paste("cd ~; cd Documents/SplitR/Projects ; rmdir '",
                     folder_name_project_to_archive, "'",
                     sep = ''))
        
        # Delete project entry in the 'SplitR.projects' file
        SplitR_projects <- read.csv(paste(SplitR_path, "/Projects/SplitR.projects", sep = ''),
                                    header = FALSE, stringsAsFactors = FALSE)
        SplitR_projects <- SplitR_projects[-project_number_to_archive,]
        
        write.table(SplitR_projects,
                    file = paste(file.path(paste(SplitR_path, "/Projects/", sep = '')),
                                 "SplitR.projects", sep = ''), sep = ",",
                    append = FALSE, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        # Add an object to indicate that an empty project was removed
        empty_project_removed <- TRUE
        
      }
      
    }
    
    # If there is no data in the 'SplitR.projects' file, state that there are no projects

    if (!exists("empty_project_removed") & !exists("project_removed")){
      if (file.info(paste(SplitR_path, "/Projects/SplitR.projects", sep = ''))[1,1] == 0){
        print("There are no projects defined.")
      }
    }
    
  }
  
}

