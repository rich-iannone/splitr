trajectory.cluster <- function(traj, method = "Euclid", n.cluster = 5, plot = TRUE, 
                               type = "default", cols = "Set1", split.after = FALSE,
                               map.fill = TRUE, 
                               map.cols = "grey30",
                               map.alpha = 0.4, ...){
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
  calcTraj <- function(traj) {
    traj <- traj[order(traj$date, traj$hour.inc), ]
    traj$len <- ave(traj$lat, traj$date, FUN = length)
    n <- max(abs(traj$hour.inc)) + 1
    traj <- subset(traj, len == n)
    len <- nrow(traj)/n
    x <- matrix(traj$lon, nrow = n)
    y <- matrix(traj$lat, nrow = n)
    z <- matrix(0, nrow = n, ncol = len)
    res <- matrix(0, nrow = len, ncol = len)
    res <- .Call(method, x, y, res)
    res[is.na(res)] <- 0
    dist.res <- as.dist(res)
    clusters <- pam(dist.res, n.cluster)
    cluster <- rep(clusters$clustering, each = n)
    traj$cluster <- factor(paste("C", cluster, sep = ""))
    traj
  }
  if (split.after) {
    traj <- ddply(traj, "default", calcTraj)
    traj <- cutData(traj, type)
  }
  else {
    traj <- cutData(traj, type)
    traj <- ddply(traj, type, calcTraj)
  }
  if (plot) {
    agg <- aggregate(traj[, c("lat", "lon", "date")], traj[, 
                                                           c("cluster", "hour.inc", type)], mean, na.rm = TRUE)
    class(agg$date) = class(traj$date)
    attr(agg$date, "tzone") <- "GMT"
    plot.args <- list(agg, x = "lon", y = "lat", group = "cluster", 
                      col = cols, type = type, map = TRUE, map.fill = map.fill, 
                      map.cols = map.cols, map.alpha = map.alpha)
    plot.args <- listUpdate(plot.args, extra.args)
    plt <- do.call(scatterPlot, plot.args)
  }
  invisible(traj)
}
