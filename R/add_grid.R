#' Add lat/lon grid to a model
#' @description Create and add a grid of latitude and
#' longitude points to a model object
#' @param model a SplitR modeling object
#' @export add_grid

add_grid <- function(model,
                     lat = 49.263,
                     lon = -123.250,
                     grid_ref = "center",
                     n_s_dist = 5,
                     w_e_dist = 5,
                     n_s_division = 0.5,
                     w_e_division = 0.5,
                     dist_units = "degrees"){
 
  # Obtain the grid of lat/lon points
  grid <- 
    create_grid(
      lat = lat,
      lon = lon,
      grid_ref = grid_ref,
      n_s_dist = n_s_dist,
      w_e_dist = w_e_dist,
      n_s_division = n_s_division,
      w_e_division = w_e_division,
      dist_units = dist_units)
  
  # Add the grid points to the model object
  model$lat <- grid$lat
  model$lon <- grid$lon
  
  return(model)
}
