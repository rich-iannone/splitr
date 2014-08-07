#' Plot HYSPLIT dispersion model output onto maps
#' @description The function plots hourly outputs of dispersed particles onto maps.
#' @param hours specification of hours of dispersion data to plot.
#' @param dispersion_df optionally specify a data frame with dispersion data as an input.
#' @param df_folder_path absolute path of the folder containing a dispersion data frame.
#' @param map_type selection provider of base maps for plotting. Choices are 'osm' (Open Street Map) and 'stamen' (Stamen Maps).
#' @param map_output_name a partial identifier prepended to the output map files.
#' @param path_output_files a full path for a location that the dispersion output files were written.
#' @export hysplit.dispersion.plot
#' @examples
#' \dontrun{
#' # Make a set of hourly plots from a dispersion data frame
#' hysplit.dispersion.plot(hours = "all",
#'                         dispersion_df = disp.df,
#'                         map_output_name = "new map",
#'                         path_output_files = "~/Documents/SplitR/Output/Plots/")
#'}

hysplit.dispersion.plot <- function(hours = 'all',
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
  
  # Plot a map of the dispersion data
  bbox <- make_bbox(lon = dispersion_df$lon, lat = dispersion_df$lat)
  
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
  
  for (i in hours){
    
    dispersion_df_hour <- subset(dispersion_df, hour == i)
    
    gg <- ggmap(ggmap = map) +
      geom_point(aes(x = dispersion_df_hour$lon,
                     y = dispersion_df_hour$lat)) +
      geom_smooth(aes(x = dispersion_df_hour$lon,
                      y = dispersion_df_hour$lat), method = lm) +
      theme(legend.position = "none")
    
    
    if (is.null(map_output_name)){
      
      ggsave(filename = paste("dispersion-map-h", hours[i], ".pdf", sep = ''),
             device = pdf,
             path = paste(path_output_files, sep = ''),
             width = 8, height = 6)
      
    } else if (!is.null(map_output_name)){
      
      ggsave(filename = paste(map_output_name, "-dispersion-map-h", hours[i], ".pdf", sep = ''),
             device = pdf,
             path = paste(path_output_files, sep = ''),
             width = 8, height = 6)
      
    }
    
  }
  
}
