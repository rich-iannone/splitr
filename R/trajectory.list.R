#' List HYSPLIT trajectory output archive files or folders
#' @description The function lists the HYSPLIT trajectory output files that reside in a specified directory.
#' @param output_folder the absolute path for the trajectory archive files (UNIX) or folders (Windows) is to be provided.
#' @return data frame with information on trajectory model output data archives
#' @export trajectory.list


trajectory.list <- function(output_folder){
  
  output_folder <- "~/Documents/SplitR/Output"
  
  if (.Platform$OS.type == "unix"){
    
    file_list <- list.files(path = output_folder, pattern = ".zip")
    
  }
  
  if (.Platform$OS.type == "windows"){
    
    dir_list <- list.dirs(path = output_folder, recursive = FALSE)
    
  }
  
  trajectory_output_df <- as.data.frame(mat.or.vec(nr = length(file_list), nc = 6))
  colnames(trajectory_output_df) <- c("No.", "Name", "Creation Date and Time",
                                      "Data Count", "Location", "Height, m")
  
  for (i in 1:length(file_list)){
    
}

