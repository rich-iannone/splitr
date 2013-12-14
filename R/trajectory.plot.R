trajectory.plot <- function(traj_df = "traj.df",
                            lon = "lon",
                            lat = "lat",
                            pollutant = "height", 
                            type = "default",
                            smooth = FALSE,
                            statistic = "mean",
                            percentile = 90, 
                            map = TRUE,
                            lon.inc = 1,
                            lat.inc = 1,
                            min.bin = 1,
                            group = NA, 
                            map.fill = TRUE,
                            map.res = "default",
                            map.cols = "grey40", 
                            map.alpha = 0.4){
  
  # Include require statements for 'maps' and 'mapdata' packages
  require(maps)
  require(mapdata)
}