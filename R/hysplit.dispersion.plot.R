#' Plot HYSPLIT dispersion model output onto maps
#' @description The function plots hourly outputs of dispersed particles onto maps.
#' @param hour specification of hours of dispersion data to plot.
#' @param dispersion_df optionally specify a data frame with dispersion data as an input.
#' @param disp_name name of previously stored dispersion project.
#' @param path_output_files a full path for a location that the dispersion output files were written.
#' @export hysplit.dispersion.plot

hysplit.dispersion.plot <- function(hours = 'all',
                                    dispersion_df = NULL,
                                    disp_name = NULL,
                                    path_output_files = NULL){
  
  # Obtain the appropriate dispersion data frame; if the value supplied to 'dispersion_df' is not
  # null (and if a valid data frame object is given) use it as the dispersion_df
  
  if (!is.null(dispersion)){
    
    if (names(disp.df) == c("particle_no", "lon", "lat", "height", "hour")){
      
      valid_names <- TRUE
      
    }
    
    if (is.integer(disp.df[,1]) &
          is.numeric(disp.df[,2]) &
          is.numeric(disp.df[,3]) &
          is.numeric(disp.df[,4]) &
          is.numeric(disp.df[,5])) valid_classes <- TRUE
    
    if (valid_names == FALSE | valid_classes == FALSE){
      
      stop("The supplied data frame is not a valid dispersion df object.")
      
    }
    
  }
    
  # If 'hours' argument has 'all' (default), determine the ending hour from
  # the dispersion data frame
  if (hours == 'all'){
   
    last_hour <- max(dispersion_df$hour)

  }
  
  # Plot a map of the dispersion data
  bbox <- make_bbox(lon = dispersion_df$lon, lat = dispersion_df$lat)
  
  map <- get_map(location = bbox, maptype = "terrain",
                 source = "osm")
  
  for (i in 1:simulation_duration_h){}
  
  disp.hour.df <- subset(dispersion_df, hour == i)
  
  gg <- ggmap(ggmap = map) +
    geom_point(aes(x = disp.hour.df$lon,
                   y = disp.hour.df$lat,
                   size = 0.5 * disp.df$height/10000,
                   alpha = 0.1)) +
    geom_smooth(aes(x = disp.hour.df$lon,
                    y = disp.hour.df$lat), method = lm) +
    theme(legend.position = "none")
  
  ggsave(filename = paste("dispersion-map-h", i, ".pdf", sep = ''),
         device = pdf,
         path = paste(path_output_files, folder_name, sep = ''),
         width = 8, height = 6)
  
}
