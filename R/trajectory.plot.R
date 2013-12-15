trajectory.plot <- function(traj.df = "traj.df",
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
                            map.alpha = 0.4,
                            plot.type = "l"
                            ){
  
  # Include require statements for 'maps' and 'mapdata' packages
  require(openair)

  
  # Execute and pass arguments to openair 'trajPlot' function
  trajPlot(mydata = traj.df, lon = lon, lat = lat,
           pollutant = pollutant, type = type, smooth = smooth,
           statistic = statistic, percentile = percentile, map = map,
           lon.inc = lon.inc, lat.inc = lat.inc, min.bin = min.bin, group = group,
           map.fill = map.fill, map.res = map.res,
           map.cols = map.cols, map.alpha = map.alpha, plot.type = plot.type)
  
}