hysplit.dispersion.animation <- function(dispersion_df = NULL,
                                         folder_name = NULL,
                                         frame_rate = 5,
                                         movie_output_name = NULL,
                                         path_output_files = NULL){
  
  # Convert PDF files to 400 ppi JPEG files using ImageMagick
  

  # Crop the resulting JPEG files to the correct aspect ratio
  
  
  # Construct a string with glob to pass into the ffmpeg call
  
  
  # Render and write the MP4 movie using FFMPEG
  if (.Platform$OS.type == "unix"){
    
    system(paste("ffmpeg -r ", frame_rate, " -pattern_type glob -i '",
                 dispersion_plot_glob, "' -c:v libx264 ", movie_output_name, ".mp4"),
           sep = '')
  
  }
  
  
  
}