project.open <- function(list_select = TRUE, project = NULL){ 
  
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
    project_list <- (project.list(display_paths = FALSE))
    
    if (exists("in_project_number")){
      x <- (in_project_number == project_numbers)
      x <- gsub("TRUE", " *OPEN* ", x) 
      x <- gsub("FALSE", " ", x)  
      project_list <- cbind(project_list,
                            matrix(x, nrow = nrow(project_list),
                                   ncol = 1, dimnames = list(1:length(project_list), c(" "))))
    }
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
    
    # Change the working directory to that of the project to open
    setwd(project.list(display_paths = TRUE)[project_number_to_open,3])
    
    # Clear out objects in the global workspace; except 'list_select' and 'project'
    rm(list = ls())
    list_select <- TRUE
    project <- NULL
    
    # Load the .Rdata file in the new project, if available
    if(file.exists(paste(getwd(), "/.Rdata", sep = '')) == TRUE){
      load(".RData", .GlobalEnv)
    }
  }
  
  if (list_select == FALSE & !is.null(project)) {
    
    
  }
  
  
}