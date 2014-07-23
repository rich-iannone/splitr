hysplit.dispersion.animation <- function(dispersion_plots = NULL){
hysplit.dispersion.animation <- function(path_plot_files,
                                         frame_rate = 5,
                                         movie_output_name = NULL,
                                         path_output_files = NULL){
  
  # Convert PDF files to 400 ppi JPEG files using ImageMagick
  

  # Crop the resulting JPEG files
  
  
  # Construct a string with glob to pass into the ffmpeg call
  
  
  # Render and write the MP4 movie using FFMPEG
  if (.Platform$OS.type == "unix"){
    
    system(paste("ffmpeg -r ", frame_rate, " -pattern_type glob -i '",
                 dispersion_plot_glob, "' -c:v libx264 ", movie_output_name, ".mp4"),
           sep = '')
  
  }
  
  
  
}