hysplit.dispersion.animation <- function(dispersion_df = NULL,
                                         start_lat_deg,
                                         start_long_deg,
                                         folder_name = NULL,
                                         frame_rate = 5,
                                         movie_output_name = NULL,
                                         IM_exec_path = "/opt/local/bin/convert",
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
      
      # Remove vectors objects from memory
      rm(dispersion_df_hour_start, particle_id, i, j, k)
      
    }
    
  }
  
  # Begin loop to generate a large dataframe with minutely particle positions
  for (i in 1:max(unique(dispersion_df$particle_id))){
    
    # Initialize a long data frame for single particle movements for every particle
    if (i == 1){
      
      particle_df <- as.data.frame(mat.or.vec(nr = 0, nc = 5))
      colnames(particle_df) <- c("particle_id", "lon", "lat", "hour", "hour_start")
      
    }
    
    # Generate a vector of evenly spaced time increments for the duration of the particle history
    vector_hour <- seq(from = min(dispersion_df[dispersion_df$particle_no == i, ][,5]) - 1,
                       to = max(dispersion_df[dispersion_df$particle_no == i, ][,5]) - (1/60),
                       by = 1/60)
    
    # Use 'spline' function to generate latitude and longitude particle positions for every
    # minute of travel from the source location
    vector_lon <- spline(x = c(start_long_deg, dispersion_df[dispersion_df$particle_no == i, ][,2]),
                         y = c(start_lat_deg, dispersion_df[dispersion_df$particle_no == i, ][,3]),
                         n = length(dispersion_df[dispersion_df$particle_no == i, ][,2]) * 60)[[1]]
    
    vector_lat <- spline(x = c(start_long_deg, dispersion_df[dispersion_df$particle_no == i, ][,2]),
                         y = c(start_lat_deg, dispersion_df[dispersion_df$particle_no == i, ][,3]),
                         n = length(dispersion_df[dispersion_df$particle_no == i, ][,2]) * 60)[[2]]
    
    # Repeat the particle ID within the df
    vector_particle_id <- rep(i, length(vector_lon))
    
    # Repeat the particle start hour within the df
    vector_hour_start <- rep(unique(dispersion_df[dispersion_df$particle_no == i, ][,6]),
                             length(vector_lon))
    
    # Bind vectors into a data frame
    df_from_vectors <- cbind(vector_particle_id, vector_lon, vector_lat,
                             vector_hour, vector_hour_start)
    colnames(df_from_vectors) <- names(particle_df)
    
    # Bind 'df_from_vectors' data frame into long data frame 'particle_df'
    particle_df <- rbind(particle_df, df_from_vectors)
    
  }
  
  # Remove objects from memory
  rm(df_from_vectors, vector_hour, vector_hour_start,
     vector_lat, vector_lon, vector_particle_id, particles_released_per_hour, i)
  
  # Obtain vector of unique, sorted, fractional hours in 'particle_df'
  fractional_hours <- sort(unique(particle_df$hour))
  
  # Determine the extent of particle dispersion
  bbox <- make_bbox(lon = particle_df$lon, lat = particle_df$lat)
  
  # Get a map that encompasses the bounds of the dispersion data
  map <- get_map(location = bbox, maptype = "terrain",
                 source = "osm")
  
  # Get the system time to append to graphics files
  output_time <- format(Sys.time(), "%Y-%m-%d--%H-%M-%S")
  
}
