hysplit.dispersion.animation <- function(dispersion_df = NULL,
                                         folder_name = NULL,
                                         frame_rate = 5,
                                         movie_output_name = NULL,
                                         path_output_files = NULL){
  
  # Obtain the appropriate dispersion data frame; if the value supplied to 'dispersion_df' is not
  # null (and if a valid data frame object is given) use it as the dispersion_df
  
  if (!is.null(dispersion_df)){
    
    if (names(dispersion_df) == c("particle_no", "lon", "lat", "height", "hour")){
      
      valid_names <- TRUE
      
    }
    
    if (is.integer(dispersion_df[,1]) &
          is.numeric(dispersion_df[,2]) &
          is.numeric(dispersion_df[,3]) &
          is.numeric(dispersion_df[,4]) &
          is.numeric(dispersion_df[,5])) valid_classes <- TRUE
    
    if (valid_names == FALSE | valid_classes == FALSE){
      
      stop("The supplied data frame is not a valid dispersion df object.")
      
    }
    
  }
  
  if (is.null(dispersion_df) & !is.null(folder_name)){
    
    if (.Platform$OS.type == "unix"){
      
      csv_absolute_path <- gsub("//", "/", paste(folder_name, "/dispersion.csv", sep = ''))
      
    }
    
    if (.Platform$OS.type == "windows"){
      
      if (grepl("\\\\", folder_name)) folder_name <- gsub("\\\\", "", folder_name)
      
      csv_absolute_path <- paste(folder_name, "\\dispersion.csv", sep = '')
      
    }
    
    dispersion_df <- read.csv(csv_absolute_path,
                              header = TRUE, stringsAsFactors = FALSE)
    
  }
  
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