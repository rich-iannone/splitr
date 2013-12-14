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
    
}