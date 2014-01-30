project.open <- function(list_select = TRUE, project = NULL){ 
  
  # Get available project numbers
  project_numbers <- seq(from = 1, to = nrow(project.list()), by = 1)
  
  # Allow for the selection of the project to open through the display of the project list
  if (list_select == TRUE) {
    
    # Display the project list
    print(project.list())
    # Clear out objects in the global workspace
  
  
  # Change the working directory to that of the project to open
  
  
  # Load the .Rdata file in the new project, if available
  
  }
  
  if (list_select == FALSE & !is.null(project)) {
  
  
  }
  
  
}