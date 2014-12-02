#' Plot HYSPLIT dispersion model output onto maps
#' @description The function plots hourly outputs of dispersed particles onto maps.
#' @param hours specification of hours of dispersion data to plot.
#' @param dispersion_df optionally specify a data frame with dispersion data as an input.
#' @param df_folder_path absolute path of the folder containing a dispersion data frame.
#' @param map_type selection provider of base maps for plotting. Choices are 'osm' (Open Street Map) and 'stamen' (Stamen Maps).
#' @param map_output_name a partial identifier prepended to the output map files.
#' @param path_output_files a full path for a location that the dispersion output files were written.
#' @export hysplit_dispersion_plot
#' @examples
#' \dontrun{
#' # Make a set of hourly plots from a dispersion data frame
#' hysplit_dispersion_plot(hours = "all",
#'                         dispersion_df = disp.df,
#'                         map_output_name = "new map",
#'                         path_output_files = "~/Documents/SplitR/Output/Plots/")
#'}

hysplit_dispersion_plot <- function(hours = 'all',
                                    dispersion_df = NULL,
                                    df_folder_path = NULL,
                                    map_type = "stamen",
                                    map_output_name = NULL,
                                    path_output_files = NULL){
  
  # Obtain the appropriate dispersion data frame; if the value supplied to 'dispersion_df' is not
  # null (and if a valid data frame object is given) use it as the dispersion_df
  
  if (!is.null(dispersion_df)){
    
    valid_names <- ifelse(all(c("particle_no", "lon", "lat", "height",
                                "hour", "hour_start", "particle_id") %in%
                                names(dispersion_df)), TRUE, FALSE)
    
    valid_classes <- ifelse(all(is.integer(dispersion_df[,1]) &
                                  is.numeric(dispersion_df[,2]) &
                                  is.numeric(dispersion_df[,3]) &
                                  is.numeric(dispersion_df[,4]) &
                                  is.numeric(dispersion_df[,5])), TRUE, FALSE)
    
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
  
  # If value for 'hours' argument contains 'all' (default), determine the ending hour from
  # the dispersion data frame
  if (hours == 'all'){
    last_hour <- max(dispersion_df$hour)
    hours <- 1:last_hour
  }
  
  # If value for 'hours' argument contains a vector list, validate that vector to ensure
  # those hours are within the range of hours in the dispersion data frame
  if (is.vector(hours)){
    hours_dispersion_df <- unique(dispersion_df$hour)
    hours <- hours[which(hours %in% hours_dispersion_df)]
  }
  
  # Determine the extent of particle dispersion
  bbox_data <- make_bbox(lon = dispersion_df$lon, lat = dispersion_df$lat)
  
  # Create 'bounding_box' function to provide a square bounding box that's defined
  # by the center-point lat/lon, and the distance away in kilometers
  bounding_box <- function(lat, lon, dist, in.miles = TRUE){
    
    if (in.miles){
      ang_rad <- function(miles) miles/3958.756  
    } else {
      ang_rad <- function(miles) miles/1000 
    }
    ang_rad <- function(dist_km) dist_km/1000
    `%+/-%` <- function(x, margin){x + c(-1, +1) * margin}
    deg2rad <- function(x) x/(180/pi)
    rad2deg <- function(x) x*(180/pi)
    lat_range <- function(latr, r) rad2deg(latr %+/-% r)
    lon_range <- function(lonr, dlon) rad2deg(lonr %+/-% dlon)
    
    r <- ang_rad(dist)
    latr <- deg2rad(lat)
    lonr <- deg2rad(lon)
    dlon <- asin(sin(r)/cos(latr))
    
    m <- matrix(c(lon_range(lonr = lonr, dlon = dlon),
                  lat_range(latr = latr, r = r)), nrow = 2, byrow = TRUE)
    
    dimnames(m) <- list(c("lng", "lat"), c("min", "max"))
    m
  }
  
  # Determine the distance away from the center-point to generate a bounding box
  # for the map image that encompasses the bounding box for the dispersion data;
  # this will keep generating new 'bbox_map' objects until the map extents are
  # greater than the data extents
  for (i in seq(from = 0.2, to = 1000, by = 0.2)){
    
    bbox_map <- bounding_box(lon = (bbox_data[[1]] + bbox_data[[3]])/2,
                             lat = (bbox_data[[2]] + bbox_data[[4]])/2,
                             i, in.miles = FALSE)
    
    if (bbox_map[1] <= bbox_data[[1]] &
          bbox_map[2] >= bbox_data[[3]] &
          bbox_map[3] <= bbox_data[[2]] &
          bbox_map[4] >= bbox_data[[4]]){
      
      break()
    }
  }
  
  # If chosen, a Stamen 'toner' style map that encompasses the bounds
  # of the dispersion data will be downloaded
  if (map_type == "stamen"){
    map <- get_map(location = bbox_map,
                   maptype = "toner",
                   source = "stamen")
  }
  
  # If chosen, an Open Street Maps 'terrain' style map that encompasses
  # the bounds of the dispersion data will be downloaded
  if (map_type == "osm"){
    map <- get_map(location = bbox_map,
                   maptype = "terrain",
                   source = "osm")
  }
  
  for (i in hours){
    
    # Create a data frame that is a subset by hour
    dispersion_df_hour <- subset(dispersion_df, hour == i)
    
    hour_x <- dispersion_df_hour$lon
    hour_y <- dispersion_df_hour$lat
    hour_h <- dispersion_df_hour$height
    
    df_xyh <- as.data.frame(cbind(hour_x, hour_y, hour_h))
    
    # Remove vector objects from memory
    rm(hour_x, hour_y, hour_h)
    
    # Generate a ggplot object from the 'df_xyh' data frame
    gg <- ggmap(ggmap = map, extent = "device") +
      geom_point(data = df_xyh, aes(x = hour_x, y = hour_y, colour = hour_h,
                                    size = hour_h, alpha = 0.5)) +
      scale_colour_gradient(low = "green", high = "darkred", trans = "sqrt",
                            limits = c(0, 5000)) +
      geom_smooth(data = df_xyh, aes(x = hour_x, y = hour_y, stat = "smooth"),
                  method = "loess") +
      theme(legend.position = "none",
            axis.line = element_blank(), axis.ticks = element_blank(), 
            axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), 
            axis.text.x = element_blank(), axis.text.y = element_blank(), axis.text.y = element_blank())
    
    if (is.null(map_output_name)){
      ggsave(filename = paste("dispersion-map-h", hours[i], ".pdf", sep = ''),
             device = pdf,
             path = paste0(path_output_files),
             width = 8, height = 8)
    } else if (!is.null(map_output_name)){
      ggsave(filename = paste(map_output_name, "-dispersion-map-h", hours[i], ".pdf", sep = ''),
             device = pdf,
             path = paste0(path_output_files),
             width = 8, height = 8)   
    }
  }
}
