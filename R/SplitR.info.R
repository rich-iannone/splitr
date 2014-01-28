SplitR.info <- function(mode = "add",
                        heading,
                        string
                        ){
  
  # Want to add a line after 'executables' and the row of dashes
  # the line is 'test'
#   mode <- "add"
#   heading <- "paths"
#   string <- "test"
  
  heading_order <- switch(heading,
                          paths = 1,
                          platform = 2,
                          executables = 3,
                          met = 4,
                          log = 5)
  
  # Get Documents folder path (in user home folder)
  if(length(grep("/Documents$",
                 list.dirs(path = "~", full.names = TRUE,
                           recursive = FALSE))) > 0) {
    documents_folder_path <- paste("~", "/Documents", sep = '')
  }
}