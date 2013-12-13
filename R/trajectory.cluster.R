trajectory.cluster <- function(traj_df, method = "Euclid", n.cluster = 5, plot = TRUE, 
                               type = "default", cols = "Set1", split.after = FALSE,
                               map.fill = TRUE, 
                               map.cols = "grey30",
                               map.alpha = 0.4, ...){
  
  # Require statements
  require(maps)
  require(mapdata)
  require(plyr)
  require(cluster)
  
  listUpdate <- function(a, b, drop.dots = TRUE, subset.a = NULL, subset.b = NULL) 
  {
    if (drop.dots) {
      a <- a[names(a) != "..."]
      b <- b[names(b) != "..."]
    }
    if (!is.null(subset.a)) 
      a <- a[names(a) %in% subset.a]
    if (!is.null(subset.b)) 
      b <- b[names(b) %in% subset.b]
    if (length(names(b) > 0)) 
      a <- modifyList(a, b)
    a
  }
  if (tolower(method) == "euclid") 
    method <- "distEuclid"
  else method <- "distAngle"
  extra.args <- list(...)
  extra.args$plot.type <- if ("plot.type" %in% names(extra.args)) 
    extra.args$plot.type
  else extra.args$plot.type <- "l"
  extra.args$lwd <- if ("lwd" %in% names(extra.args)) 
    extra.args$lwd
  else extra.args$lwd <- 4
  calcTraj <- function(traj_df) {
    traj_df <- traj_df[order(traj_df$date, traj_df$hour.inc), ]
    traj_df$len <- ave(traj_df$lat, traj_df$date, FUN = length)
    n <- max(abs(traj_df$hour.inc)) + 1
    traj_df <- subset(traj_df, len == n)
    len <- nrow(traj_df)/n
    x <- matrix(traj_df$lon, nrow = n)
    y <- matrix(traj_df$lat, nrow = n)
    z <- matrix(0, nrow = n, ncol = len)
    res <- matrix(0, nrow = len, ncol = len)
    res <- .Call(method, x, y, res)
    res[is.na(res)] <- 0
    dist.res <- as.dist(res)
    clusters <- pam(dist.res, n.cluster)
    cluster <- rep(clusters$clustering, each = n)
    traj_df$cluster <- factor(paste("C", cluster, sep = ""))
    traj_df
  }
  if (split.after) {
    traj_df <- ddply(traj_df, "default", calcTraj)
    traj_df <- cutData(traj_df, type)
  }
  else {
    traj_df <- cutData(traj_df, type)
    traj_df <- ddply(traj_df, type, calcTraj)
  }
  if (plot) {
    agg <- aggregate(traj_df[, c("lat", "lon", "date")],
                     traj_df[, c("cluster", "hour.inc", type)],
                     mean, na.rm = TRUE)
    class(agg$date) = class(traj_df$date)
    attr(agg$date, "tzone") <- "GMT"
    plot.args <- list(agg, x = "lon", y = "lat", group = "cluster", 
                      col = cols, type = type, map = TRUE,
                      map.fill = map.fill, 
                      map.cols = map.cols,
                      map.alpha = map.alpha)
    plot.args <- listUpdate(plot.args, extra.args)
    plt <- do.call(scatterPlot, plot.args)
  }
  invisible(traj_df)
}
