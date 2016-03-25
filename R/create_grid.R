#' Create a grid of sites
#' @description Flexibly create a grid of sites for
#' ensemble trajectory model runs.
#' @param lat_deg 
#' @param lon_deg
#' @param grid_ref
#' @param n_s_dist
#' @param w_e_dist
#' @param n_s_division
#' @param w_e_division
#' @param dist_units
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
