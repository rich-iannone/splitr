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
