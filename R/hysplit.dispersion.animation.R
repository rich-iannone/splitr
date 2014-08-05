#' Create an animated movie from HYSPLIT dispersion model output
#' @description Create a movie file of dispersed particles from a dispersion data frame
#' @param dispersion_df specification of hours of dispersion data to plot.
#' @param df_folder_path absolute path of the folder containing a dispersion data frame.
#' @param start_lat_deg the starting latitude (in decimal degrees) for the model run(s).
#' @param start_long_deg the starting longitude (in decimal degrees) for the model run(s).
#' @param start_height_m_AGL the starting height (in meters above ground level) for the model run(s).
#' @param write_particle_CSV an option to write the subhourly particle dispersion data to a CSV file.
#' @param map_type selection provider of base maps for plotting. Choices are 'osm' (Open Street Map) and 'stamen' (Stamen Maps).
#' @param frame_rate the desired frame rate of the generated MP4 movie file.
#' @param movie_output_name the desired filename for the generate MP4 movie.
#' @param IM_exec_path an absolute system path for the ImageMagick 'convert' command.
#' @param path_output_files a full path for a location that the dispersion output files were written.
#' @export hysplit.dispersion.animation
#' @examples
#' \dontrun{
#' # Create an MP4 movie from a data frame returned by the 'hysplit.dispersion' function
#' hysplit.dispersion.animation(dispersion_df = disp.df,
#'                              start_lat_deg = 42.83752,
#'                              start_long_deg = -80.30364,
#'                              start_height_m_AGL = 15,
#'                              write_particle_CSV = TRUE,
#'                              frame_rate = 30,
#'                              movie_output_name = "dispersion-movie",
#'                              IM_exec_path = "/opt/local/bin/convert",
#'                              path_output_files = "~/Documents/SplitR/Output/Movies/"
#'}

hysplit.dispersion.animation <- function(dispersion_df = NULL,
                                         df_folder_path = NULL,
                                         start_lat_deg,
                                         start_long_deg,
                                         start_height_m_AGL,
                                         write_particle_CSV = FALSE,
                                         map_type = "stamen",
                                         frame_rate = 30,
                                         movie_output_name = NULL,
                                         IM_exec_path = "/opt/local/bin/convert",
                                         path_output_files){
  
  # Add require statements
  require(labeling)
  
  # Stop function if no path for output files is provided
  if (!exists("path_output_files")){
    stop("There must be a path for output files specified.") 
  }
  
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
  
  if (is.null(dispersion_df) & !is.null(df_folder_path)){
    
    if (.Platform$OS.type == "unix"){
      
      csv_absolute_path <- gsub("//", "/", paste(df_folder_path, "/dispersion.csv", sep = ''))
      
    }
    
    if (.Platform$OS.type == "windows"){
      
      if (grepl("\\\\", df_folder_path)) df_folder_path <- gsub("\\\\", "", df_folder_path)
      
      csv_absolute_path <- paste(df_folder_path, "\\dispersion.csv", sep = '')
      
    }
    
    dispersion_df <- read.csv(csv_absolute_path,
                              header = TRUE, stringsAsFactors = FALSE)
    
  }
  
  # Determine the extent of particle dispersion
  bbox <- make_bbox(lon = dispersion_df$lon, lat = dispersion_df$lat)
  
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
      
      particle_df <- as.data.frame(mat.or.vec(nr = 0, nc = 6))
      colnames(particle_df) <- c("particle_id", "lon", "lat", "height", "hour", "hour_start")
      
    }
    
    # Generate a vector of evenly spaced time increments for the duration of the particle history
    vector_hour <- seq(from = min(dispersion_df[dispersion_df$particle_no == i, ][,5]) - 1,
                       to = max(dispersion_df[dispersion_df$particle_no == i, ][,5]) - (1/100),
                       by = 1/100)
    
    # Use 'spline' function to generate latitude and longitude particle positions for every
    # minute of travel from the source location
    vector_lon <- spline(x = c(start_long_deg, dispersion_df[dispersion_df$particle_no == i, ][,2]),
                         y = c(start_lat_deg, dispersion_df[dispersion_df$particle_no == i, ][,3]),
                         n = length(dispersion_df[dispersion_df$particle_no == i, ][,2]) * 100)[[1]]
    
    vector_lat <- spline(x = c(start_long_deg, dispersion_df[dispersion_df$particle_no == i, ][,2]),
                         y = c(start_lat_deg, dispersion_df[dispersion_df$particle_no == i, ][,3]),
                         n = length(dispersion_df[dispersion_df$particle_no == i, ][,2]) * 100)[[2]]
    
    # Use 'spline' function to generate particle heights for every minute of travel from the
    # source location
    vector_hgt <- spline(x = c(start_height_m_AGL, dispersion_df[dispersion_df$particle_no == i, ][,4]),
                         n = length(dispersion_df[dispersion_df$particle_no == i, ][,2]) * 100)[[2]]
    
    # Repeat the particle ID within the df
    vector_particle_id <- rep(i, length(vector_lon))
    
    # Repeat the particle start hour within the df
    vector_hour_start <- rep(unique(dispersion_df[dispersion_df$particle_no == i, ][,6]),
                             length(vector_lon))
    
    # Bind vectors into a data frame
    df_from_vectors <- cbind(vector_particle_id, vector_lon, vector_lat, vector_hgt,
                             vector_hour, vector_hour_start)
    colnames(df_from_vectors) <- names(particle_df)
    
    # Bind 'df_from_vectors' data frame into long data frame 'particle_df'
    particle_df <- rbind(particle_df, df_from_vectors)
    
  }
  
  # Remove objects from memory
  rm(df_from_vectors, vector_hour, vector_hour_start, vector_hgt,
     vector_lat, vector_lon, vector_particle_id, particles_released_per_hour, i)
  
  # Round the 'hour' column in 'particle_df' to 2 decimal places
  particle_df$hour <- round(particle_df$hour, 2)
  
  # Round the 'height' column in 'particle_df' to 0 decimal places
  particle_df$height <- round(particle_df$height, 0)
  
  # Write the 'particle_df' data frame to a CSV file if it is requested
  if (write_particle_CSV == TRUE){
    
    if (.Platform$OS.type == "unix"){
      
      write.table(particle_df, file = paste(path_output_files, "/particle.csv", sep = ''),
                  sep = ",", row.names = FALSE)
      
    }
    
    if (.Platform$OS.type == "windows"){
      
      write.table(particle_df, file = paste(path_output_files, "\\particle.csv", sep = ''),
                  sep = ",", row.names = FALSE)
      
    }
    
  }  
  
  # Obtain vector of unique, sorted, fractional hours in 'particle_df'
  fractional_hours <- sort(unique(particle_df$hour))
  
  # If chosen, a Stamen 'toner' style map that encompasses the bounds
  # of the dispersion data will be downloaded
  if (map_type == "stamen"){
    map <- get_map(location = bbox, maptype = "toner",
                   source = "stamen")
  }
  
  # If chosen, an Open Street Maps 'terrain' style map that encompasses
  # the bounds of the dispersion data will be downloaded
  if (map_type == "osm"){
    map <- get_map(location = bbox, maptype = "terrain",
                   source = "osm")
  }
  
  # Get the system time to append to graphics files
  output_time <- format(Sys.time(), "%Y-%m-%d--%H-%M-%S")
  
  # Generate an image from particles at every fractional hour
  for (i in 1:length(fractional_hours)){
    
    # Create a data frame that is a subset by fractional hour
    particle_df_time <- subset(particle_df, hour == fractional_hours[i])
    
    # Generate a ggplot object from the subset data frame 'particle_df_time'
    gg <- ggmap(ggmap = map) +
      geom_point(aes(x = start_lat_deg,
                     y = start_long_deg),
                 colour = "blue", size = 50) +
      geom_point(data = particle_df_time, aes(x = lon, y = lat, colour = height,
                                              size = particle_df_time$height, alpha = 0.5)) +
      scale_colour_gradient(low = "green", high = "darkred", trans = "sqrt",
                            limits = c(0, 5000)) +
      geom_smooth(aes(x = c(start_long_deg, particle_df_time$lon),
                      y = c(start_lat_deg, particle_df_time$lat),
                      method = loess)) +
      theme(legend.position = "none")
    
    # Generate a timeline abscissa
    #     gg_time <- unique(particle_df_time$hour)
    
    
    
    # Obtain particle heights along the loess smoother line
    
    
    
    
    # Save the image to disk
    ggsave(filename = paste("dispersion-map-", output_time, "-",
                            formatC(i, width = 6, flag = "0"),
                            ".pdf", sep = ''),
           device = pdf,
           path = paste(path_output_files, sep = ''),
           width = 8, height = 6)
    
    # Convert PDF file to a JPEG file using ImageMagick, cropping whitespace
    system(paste("cd ", path_output_files, " ; ", IM_exec_path,
                 " -verbose -density 150 -trim ",
                 paste("dispersion-map-", output_time, "-",
                       formatC(i, width = 6, flag = "0"),
                       ".pdf", sep = ''),
                 " -quality 100 -sharpen 0x1.0 ",
                 paste("dispersion-map-", output_time, "-",
                       formatC(i, width = 6, flag = "0"),
                       ".jpg", sep = ''),
                 sep = ''))
    
    # Remove the PDF file from disk
    system(paste("cd ", path_output_files, " ; rm ", 
                 paste("dispersion-map-", output_time, "-",
                       formatC(i, width = 6, flag = "0"),
                       ".pdf", sep = ''),
                 sep = ''))
    
  }
  
  # Construct a string with glob to pass into the ffmpeg call
  dispersion_plot_glob <- paste("dispersion-map-", output_time, "-%06d.jpg",
                                sep = '')
  
  # Construct the movie name
  if (is.null(movie_output_name)){
    
    movie_output_name <- paste("dispersion_movie__", output_time, sep = "")
    
  }
  
  # If a movie name is provided, ensure that the filename won't be duplicated
  if (!is.null(movie_output_name)){
    
    if (paste(movie_output_name, ".mov", sep = '') %in%
          list.files(path = path_output_files)){
      
      movie_output_name <- paste(movie_output_name, "-",
                                 output_time, sep = '')
      
    }
    
  }  
  
  # Render and write the MP4 movie using ffmpeg
  if (.Platform$OS.type == "unix"){
    
    system(paste("cd ", path_output_files, " ; ffmpeg -f image2 -start_number 1 -i '",
                 dispersion_plot_glob, "' -r ", frame_rate, " ",
                 "-vcodec libx264 -pix_fmt yuv420p ",
                 movie_output_name, ".mov",
                 sep = ''))
    
  }
  
}
