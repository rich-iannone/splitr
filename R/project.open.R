project.open <- function(list_select = TRUE, project = NULL){ 
  
  # Get available project numbers
  project_numbers <- seq(from = 1, to = nrow(project.list()), by = 1)
  
  # Allow for the selection of the project to open through the display of the project list
  if (list_select == TRUE & is.null(project)) {
    
    # Display the project list
    print(project.list())
    
    # Allow for user to enter a number corresponding to the project to open
    project_number_to_open <-
      readline(paste("Which project number would you like to open? [1-",
                     paste(nrow(project.list()), sep = ''), "] ", sep = ''))
    project_number_to_open <- as.numeric(project_number_to_open)
    
    # Validate input
    if(!(project_number_to_open %in% project_numbers)){
      return("That is not a valid project number.")
    }
    
    # If currently in a project, ask to close that project before switching to the next
    
    
    # Ask to save the current workspace to an .Rdata file in its project directory
      save_to_Rdata <- readline("Save the workspace? [y/n] ")
    
    # Validate input
    if(!(save_to_Rdata %in% c("y", "n", "yes", "no", "yeah", "nope",
                              "yep", "sure", "nah", "false"))){
      save_to_Rdata <- "y"
    }
    
    if(save_to_Rdata == "yes" | save_to_Rdata == "yeah" |
         save_to_Rdata == "yep" | save_to_Rdata == "sure"){
      save_to_Rdata <- "y"
    }
    
    if(save_to_Rdata == "no" | save_to_Rdata == "nope" |
         save_to_Rdata == "nah" | save_to_Rdata == "false"){
      save_to_Rdata <- "n"
    }
    
    if(save_to_Rdata == "y"){    
      save(list = ls(all = TRUE), file = ".RData")
    }
    
    # Clear out objects in the global workspace; except 'list_select' and 'project'
    rm(list = ls())
    list_select <- TRUE
    project <- NULL
    
    # Change the working directory to that of the project to open
    setwd(project.list(display_paths = TRUE)[project_number_to_open,3])
    
    # Load the .Rdata file in the new project, if available
    
  }
  
  if (list_select == FALSE & !is.null(project)) {
    
    
  }
  
  
}