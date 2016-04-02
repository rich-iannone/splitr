#' Add lat/lon grid to a model
#' @description Create and add a grid of latitude and
#' longitude points to a model object
#' @param model a SplitR modeling object
#' @param lat a latitude value in decimal degrees
#' for the point of reference on the grid.
#' @param lon a longitude value in decimal degrees
#' for the point of reference on the grid.
#' @param grid_ref the grid reference point. The
#' default value is \code{center}.
#' @param range the latitude and longitude range about
#' the \code{grid_ref}.
#' @param division the division distances across the
#' latitude and longitude ranges.
#' @param dist_units the distance units used with
#' values supplied to \code{range} and
#' \code{division}. The default is \code{degrees}.
#' @export add_grid

add_grid <- function(model,
                     lat = 49.263,
                     lon = -123.250,
                     grid_ref = "center",
                     range = c(5, 5),
                     division = c(0.5, 0.5),
                     dist_units = "degrees"){
 
  # Obtain the grid of lat/lon points
  grid <- 
    create_grid(
      lat = lat,
      lon = lon,
      grid_ref = grid_ref,
      range = range,
      division = division,
      dist_units = dist_units)
  
  # Add the grid points to the model object
  model$lat <- grid$lat
  model$lon <- grid$lon
  
  return(model)
}
