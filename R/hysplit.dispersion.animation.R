hysplit.dispersion.animation <- function(dispersion_df = NULL,
                                         start_lat_deg,
                                         start_long_deg,
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
  
  # Build on the 'dispersion_df' data frame by identifying each particle by hour
  # of release; first determine number of particles released from source per hour
  particles_released_per_hour <- nrow(subset(dispersion_df, hour == 1))
  
  # Determine the total number of hours
  total_hours <- max(dispersion_df$hour)
  
  # For each hour of particle positions in the 'dispersion_df' data frame, classify
  # the particles by the hour of release
  for (i in 1:total_hours){
    
    if (i == 1) {
      
      dispersion_df_hour_start <- vector(mode = "numeric", length = 0)
      particle_id <- vector(mode = "numeric", length = 0)
      
    }
    
    for (j in 1:i){
      
      dispersion_df_hour_start <- c(dispersion_df_hour_start,
                                    rep(j, times = particles_released_per_hour))
      
    }
    
    for (k in 1:i)
      particle_id <- c(particle_id,
                       seq(from = ((k - 1) * particles_released_per_hour) + 1,
                           to = (k * particles_released_per_hour),
                           by = 1))
    
    
    if (i == total_hours){
      
      # Add 'hour_start' and 'particle_id' vectors to 'dispersion_df' data frame
      dispersion_df$hour_start <- dispersion_df_hour_start
      dispersion_df$particle_id <- particle_id
      
      Remove vectors objects from memory
            rm(dispersion_df_hour_start, particle_id)
      
    }
    
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
