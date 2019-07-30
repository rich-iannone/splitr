#' Create a grid of sites
#'
#' Flexibly create a grid of sites for ensemble trajectory model runs.
#' @param lat A latitude value in decimal degrees for the point of reference on
#'   the grid.
#' @param lon A longitude value in decimal degrees for the point of reference on
#'   the grid.
#' @param grid_ref The grid reference point. The default value is `center`.
#' @param range The latitude and longitude range about the `grid_ref`.
#' @param division The division distances across the latitude and longitude
#'   ranges.
#' @param dist_units The distance units used with values supplied to
#'   `range` and `division`. The default is `degrees`.
#' @export
create_grid <- function(lat = 49.263,
                        lon = -123.250,
                        grid_ref = "center",
                        range = c(5, 5),
                        division = c(0.5, 0.5),
                        dist_units = "degrees") {
  
  n_s_points <- range[1] / division[1]
  w_e_points <- range[2] / division[2]
  
  lat_vec <- 
    sort((lat + range[1]/2) - 
           seq(0, range[1], division[1]),
         decreasing = TRUE)
  
  lon_vec <- 
    sort((lon + range[2]/2) - 
           seq(0, range[2], division[2]))
  
  for (i in 1:length(lat_vec)) {
    for (j in 1:length(lon_vec)) {
      if (i == 1 & j == 1) {
        coords <- vector("list", 2)
        names(coords) <- c("lat", "lon")
      }
      
      coords$lat[length(coords$lat) + 1] <- lat_vec[i]
      coords$lon[length(coords$lon) + 1] <- lon_vec[j]
    }
  }
  
  coords
}
