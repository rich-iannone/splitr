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
  
}