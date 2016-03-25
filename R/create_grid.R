#' Create a grid of sites
#' @description Flexibly create a grid of sites for
#' ensemble trajectory model runs.
#' @param lat_deg a latitude value in decimal degrees
#' for the point of reference on the grid.
#' @param lon_deg a longitude value in decimal degrees
#' for the point of reference on the grid.
#' @param grid_ref the grid reference point. The
#' default value is \code{center}.
#' @param n_s_dist the north-to-south distance of the
#' grid.
#' @param w_e_dist the west-to-east distance of the
#' grid.
#' @param n_s_division the north-to-south distances of
#' the those divisions in the grid.
#' @param w_e_division the west-to-east distances of
#' the those divisions in the grid.
#' @param dist_units the distance units used with
#' values supplied to  \code{n_s_dist},
#' \code{w_e_dist}, \code{n_s_division}, and
#' \code{w_e_division}. The default is \code{degrees}.
#' @export create_grid

create_grid <- function(lat_deg = 49.263,
                        lon_deg = -123.250,
                        grid_ref = "center",
                        n_s_dist = 5,
                        w_e_dist = 5,
                        n_s_division = 0.5,
                        w_e_division = 0.5,
                        dist_units = "degrees"){
  
  n_s_points <- n_s_dist / n_s_division
  w_e_points <- w_e_dist / w_e_division
  
  lat <- (lat_deg + n_s_dist/2) - seq(0, n_s_dist, n_s_division)
  lon <- (lon_deg + w_e_dist/2) - seq(0, w_e_dist, w_e_division)
  
  for (i in 1:length(lat)){
    for (j in 1:length(lon)){
      if (i == 1){
        coords <- vector("list", 2)
        names(coords) <- c("lat", "lon")
      }

      coords$lat[length(coords$lat) + 1] <- lat[i]
      coords$lon[length(coords$lon) + 1] <- lon[j]
    }
  }

  return(coords)
}
