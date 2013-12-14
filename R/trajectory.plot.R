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
                            map.alpha = 0.4){
  
  # Include require statements for 'maps' and 'mapdata' packages
  require(maps)
  require(mapdata)
  
  len <- NULL
  traj.df <- traj.df[order(traj.df$date, traj.df$hour.inc), ]
  traj.df$len <- ave(traj.df$lat, traj.df$date, FUN = length)
  n <- max(abs(traj.df$hour.inc)) + 1
  traj.df <- subset(traj.df, len == n)
  
  # Collect plotting parameters
  extra.args <- list(...)
  method <- "scatter"
  if (!"plot.type" %in% names(extra.args)) 
    extra.args$plot.type <- "l"
  if (!"cex" %in% names(extra.args)) 
    extra.args$cex <- 0.1
  if (!"ylab" %in% names(extra.args)) 
    extra.args$ylab <- "latitude"
  if (!"xlab" %in% names(extra.args)) 
    extra.args$xlab <- "longitude"
    
  
  if (missing(pollutant)) {
    if (is.na(group)) 
      key <- FALSE
    else key <- TRUE
    if (!"main" %in% names(extra.args)) 
      extra.args$main <- NULL
    scatterPlot.args <- list(traj.df, x = lon, y = lat, z = NA, 
                             type = type, method = method, smooth = smooth, map = map, 
                             x.inc = lon.inc, y.inc = lat.inc, key = key, group = group, 
                             map.fill = map.fill, map.res = map.res, map.cols = map.cols, 
                             map.alpha = map.alpha)
  }
  else {
    if (!"main" %in% names(extra.args)) 
      extra.args$main <- pollutant
    scatterPlot.args <- list(traj.df, x = lon, y = lat, z = pollutant, 
                             type = type, method = method, smooth = smooth, map = map, 
                             x.inc = lon.inc, y.inc = lat.inc, group = group, 
                             map.fill = map.fill, map.res = map.res, map.cols = map.cols, 
                             map.alpha = map.alpha)
  }
  scatterPlot.args <- listUpdate(scatterPlot.args, extra.args)
  do.call(scatterPlot, scatterPlot.args)
  
}