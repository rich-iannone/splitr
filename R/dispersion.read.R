#' Read HYSPLIT dispersion output files
#' @description The function takes HYSPLIT dispersion output files in a specified output directory and processes all files into a data frame object.
#' @param path_output_files the absolute path of the dispersion output files is to be provided.
#' @return data frame with hourly particle positions
#' @export dispersion.read
#' @examples
#' \dontrun{
#' # Process all dispersion output files in the specified output directory
#' dispersion.df <- dispersion.read(path_output_files =
#'                                  "~/hysplit/dispersion/")
#'}

dispersion.read <- function(path_output_files){  
  
  # Generate the file list
  dispersion_file_list <- list.files(path = path_output_files,
                                     pattern = "^GIS_part_[0-9][0-9][0-9]_ps.csv",
                                     full.names = TRUE)
  
  # Get each CSV file into a single data frame
  for (i in 1:length(dispersion_file_list)){
    if (i == 1){
      dispersion <- as.data.frame(mat.or.vec(nr = 0, nc = 5))
      colnames(dispersion) <- c("particle_no", "lon", "lat", "height", "hour")
    }
    disp <- read.csv(dispersion_file_list[i], header = FALSE)
    colnames(disp) <- c("particle_no", "lon", "lat", "height")
    disp$hour <- i
    dispersion <- rbind(dispersion, disp)
  }
  
  # Return the data frame
  return(dispersion)
    
}
