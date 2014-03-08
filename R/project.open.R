project.open <- function(list_select = TRUE, project_name = NULL){ 
  
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
  
  # If the SplitR folder exists, get its path; if it doesn't exist, stop the function
  if (SplitR_dir_exists == TRUE) {
    # Get path to SplitR folder
    SplitR_path <- file.path(paste(documents_folder_path, "/SplitR", sep = ''))
  } else if (SplitR_dir_exists == FALSE) {
    # Stop function if the path doesn't exist
    stop("The SplitR folder doesn't exist. Please set up with 'SplitR.init' function.")
  }
  
  # Check current working directory
  current_wd <- getwd()
  
  # If the current working directory is outside of ~/SplitR, set it to that
  if (grepl("/Documents/SplitR", current_wd, perl = TRUE) == FALSE) {
    setwd("~/Documents/SplitR/Projects")
  }

  # Get available project numbers
  project_numbers <- seq(from = 1, to = nrow(project.list()), by = 1)
  
  # Determine the current project number
  #
  # Get the current wd, strip the path to the project subfolder level
  current_wd_project <- gsub("^(.*)(Projects*)", "\\2", getwd())
  
  # Get list of paths for projects, strip the paths to the project subfolder level
  project_paths <- gsub("^(.*)(Projects*)", "\\2", project.list(display_paths = TRUE)[,3])
  
  # Set up loop to determine if the current wd (i.e. the current project) is set to any of
  # the paths for the currently defined project
  for (i in 1:length(project_paths)){
    if (current_wd_project %in% project_paths[i]) in_project_number <- i
  }
  
  # Allow for the selection of the project to open through the display of the project list
  if (list_select == TRUE & is.null(project)) {
    
    # Display the project list; indicate if a project is currently open
    project_list <- project.list(display_paths = FALSE)
    
    if (exists("in_project_number")){
      x <- (in_project_number == project_numbers)
      x <- gsub("TRUE", " *OPEN* ", x) 
      x <- gsub("FALSE", " ", x)  
      project_list <- cbind(project_list,
                            matrix(x, nrow = nrow(project_list),
                                   ncol = 1, dimnames = list(1:nrow(project_list), c(" "))))
    }
    
    print(project_list)
    
    # Filter a list of selectable project numbers
    if (exists("in_project_number")){
      if (in_project_number %in% project_numbers){
        selectable_project_numbers <- project_numbers[!project_numbers == in_project_number]
      }
    }
    
    # Allow user to switch to the only other selectable project
    if (exists("in_project_number") & exists("selectable_project_numbers")){
      
      if (length(selectable_project_numbers) == 1){
        
        switch_to_other_project <-
          readline(paste("Switch to project ", selectable_project_numbers, "? [y/n] ",
                         sep = ''))
        
        # Validate input
        if(!(switch_to_other_project %in% c("y", "n", "yes", "no", "yeah", "nope",
                                            "yep", "sure", "nah", "false"))){
          return("I need a yes or no answer.")
        }
        
        if(switch_to_other_project == "yes" | switch_to_other_project == "yeah" |
             switch_to_other_project == "yep" | switch_to_other_project == "sure"){
          switch_to_other_project <- "y"
        }
        
        if(switch_to_other_project == "no" | switch_to_other_project == "nope" |
             switch_to_other_project == "nah" | switch_to_other_project == "false"){
          switch_to_other_project <- "n"
        }
        
        if(switch_to_other_project == "y"){    
          project_number_to_open <- selectable_project_numbers
        } else if (switch_to_other_project == "n"){
          return("Okay, let's stay in the current project")
        }      
      }
    }
        
    # Allow user to open another selectable project by number
    
    if (exists("in_project_number") & exists("selectable_project_numbers")){
      
      if (length(selectable_project_numbers) > 1){
        
        project_number_to_open <-
          readline(paste("Which project number would you like to open? [",
                         paste(selectable_project_numbers, sep = ", "), "] ", sep = ''))
        project_number_to_open <- as.numeric(project_number_to_open)
        
        # Validate input
        if(!(project_number_to_open %in% selectable_project_numbers)){
          return("That is not a valid project number.")
        }
      }
    }
      
    # If currently in a project, ask to close that project before switching to the next
    if(grepl("*/Documents/SplitR/Projects/*", getwd()) == TRUE){
      close_current_project <- readline("Close the current project? [y/n] ")
      
      # Validate input
      if(!(close_current_project %in% c("y", "n", "yes", "no", "yeah", "nope",
                                "yep", "sure", "nah", "false"))){
        return("I need a yes or no answer.")
      }
      
      if(close_current_project == "yes" | close_current_project == "yeah" |
           close_current_project == "yep" | close_current_project == "sure"){
        close_current_project <- "y"
      }
      
      if(close_current_project == "no" | close_current_project == "nope" |
           close_current_project == "nah" | close_current_project == "false"){
        close_current_project <- "n"
      }
      
      if(close_current_project == "y"){    
      NULL
      } else if (close_current_project == "n"){
        return("Okay, let's stay in the current project")
      }      
    }

    # Change the working directory to that of the project to open
    setwd(project.list(display_paths = TRUE)[project_number_to_open,3])
    
    # Clear out objects in the global workspace; except 'list_select' and 'project'
    rm(list = ls())
    list_select <- TRUE
    project <- NULL
    
#     # Load the .Rdata file in the new project, if available
#     if(file.exists(paste(getwd(), "/.Rdata", sep = '')) == TRUE){
#       load(".RData", .GlobalEnv)
#     }
  }
  
  if (list_select == FALSE & !is.null(project)) {
    
  }  
}