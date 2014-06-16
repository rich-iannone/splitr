#' Get statistics for dispersed particles travelling through a specified region
#' @description This function computes statistics focussed on particles travelling through a specified volume of air.
#' @param dispersion_df a data frame generated from dispersion output data returned from the 'dispersion.read' function.
#' @param stats the type of statistical analysis to perform. Currently, "frequencies" is available.
#' @param lat a numeric vector containing latitude bounds
#' @param lon a numeric vector containing longitude bounds
#' @param heights a numeric vector containing height bounds
#' @export disp.stats.particle_through_region
#' @examples
#' # Get a statistical summary of particles available in region every hour
#' disp.stats.particle_through_region(dispersion_df = disp_output,
#'                                    stats = "frequencies",
#'                                    lat = c(49.05, 49.50),
#'                                    lon = c(-127.2, -128.0),
#'                                    heights = c(0, 1000))

disp.stats.particle_through_region <- function(dispersion_df,
                                               stats,
                                               lat,
                                               lon,
                                               heights){

  
  if ("frequencies" %in% stats){
    
    # Validate that the 'lat', 'lon', and 'heights' objects exist
    if (!exists("lat") | !exists("lon") | !exists("heights")) {
      incomplete_positions <- TRUE
    } else {
      incomplete_positions <- FALSE
    }
    
    # Validate that the 'lat', 'lon', and 'heights' objects are numeric vectors
    # of length 2
    if (length(lat) != 2 | length(lon) != 2 | length(heights) !=2 |
          !is.numeric(lat) | !is.numeric(lon) | !is.numeric(heights)) {
      incorrect_positions <- TRUE
    } else {
      incorrect_positions <- FALSE
    }
    
    # Stop function if incomplete or invalid values are supplied
    if (incomplete_positions == TRUE | incorrect_positions == TRUE) {
      stop("Incomplete or invalid values were supplied.") 
    }
    
    # Move the values from 'lat', 'lon', and 'heights' into separate numeric objects
    lat_lower <- min(lat)
    lat_upper <- max(lat)
    
    lon_lower <- min(lon)
    lon_upper <- max(lon)
    
    height_lower <- min(heights)
    height_upper <- max(heights)
    
    # Get the number of particles located in a polygon at each hour
    for (i in 1:length(unique(dispersion_df$hour))){
      
      if (i == 1){
        hours <- sort(unique(dispersion_df$hour))
        stats_table <- as.data.frame(mat.or.vec(nr = 0, nc = 4))
        colnames(stats_table) <- c("total_no_particles", "no_particles_in_region",
                                   "pct_particles_in_region", "hour")
      }
      
      dispersion.hour <- subset(dispersion_df, hour == hours[i])
      
      dispersion.hour.within <- subset(dispersion.hour, lon >= lon_lower)
      dispersion.hour.within <- subset(dispersion.hour.within, lon <= lon_upper)
      dispersion.hour.within <- subset(dispersion.hour.within, lat >= lat_lower)
      dispersion.hour.within <- subset(dispersion.hour.within, lat <= lat_upper)
      dispersion.hour.within <- subset(dispersion.hour.within, height >= height_lower)
      dispersion.hour.within <- subset(dispersion.hour.within, height <= height_upper)
      
      stats_small_table <- as.data.frame(mat.or.vec(nr = 1, nc = 4))
      colnames(stats_small_table) <- c("total_no_particles", "no_particles_in_region",
                                       "pct_particles_in_region", "hour")
      
      stats_small_table$total_no_particles <- nrow(dispersion.hour)
      stats_small_table$no_particles_in_region <- nrow(dispersion.hour.within)
      stats_small_table$pct_particles_in_region <-
        round(((nrow(dispersion.hour.within) / nrow(dispersion.hour)) * 100), digits = 2)
      stats_small_table$hour <- i
      
      stats_table <- rbind(stats_table, stats_small_table)
    }
    
    # Return the stats table containing the particles within region at every hour
    return(stats_table)
    
  }
  
}
