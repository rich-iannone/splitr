project.open <- function(list_select = TRUE, project = NULL){ 

  # Allow for the selection of the project to open through the display of the project list
  if (list_select = TRUE) {
  project.list()
  
  # Allow for user to enter a number corresponding to the project to open
  
  
  # If currently in a project, close that project before switching to the next
  
  
    # Ask to save the current workspace to an .Rdata file in its project directory
  
  
    # Clear out objects in the global workspace
  
  
  # Change the working directory to that of the project to open
  
  
  # Load the .Rdata file in the new project, if available
  
  }
  
  if (list_select = FALSE & !is.null(project)) {
  
  
  }
  
  
}