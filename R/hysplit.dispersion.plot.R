hysplit.dispersion.plot <- function(hours = 'all', 
                                    dispersion_df = NULL,
                                    disp_name = NULL,
                                    path_output_files = NULL){
  
  
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
