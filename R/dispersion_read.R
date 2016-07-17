#' Read HYSPLIT dispersion output files
#' @description The function takes HYSPLIT dispersion
#' output files in a specified output directory and
#' processes all files into a data frame object.
#' @param archive_folder the absolute path of the
#' directory containing dispersion output files is 
#' provided here.
#' @return data frame with hourly particle positions
#' @examples
#' \dontrun{
#' # Process all dispersion output files in the 
#' # specified output directory
#' dispersion.df <- 
#'   dispersion_read(
#'     path_output_files =  "~/hysplit/dispersion/")
#'}
#' @export dispersion_read

dispersion_read <- function(archive_folder) {  
  
  dispersion_file_list <- 
    list.files(
      path = archive_folder,
      pattern = "^GIS_part_[0-9][0-9][0-9]_ps.csv",
      full.names = TRUE)
  
  # Get each CSV file into a single data frame
  for (i in 1:length(dispersion_file_list)) {
    if (i == 1) {
      dispersion <- 
        as.data.frame(mat.or.vec(nr = 0, nc = 5))
      colnames(dispersion) <- 
        c("particle_no", "lon", "lat",
          "height", "hour")
    }
    disp <- 
      read.csv(dispersion_file_list[i], header = FALSE)
    colnames(disp) <- 
      c("particle_no", "lon", "lat", "height")
    disp$hour <- i
    dispersion <- rbind(dispersion, disp)
  }
  
  # Return the data frame
  return(dispersion)    
}
