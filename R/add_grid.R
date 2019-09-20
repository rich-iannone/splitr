#' Add lat/lon grid to a model
#'
#' Create and add a grid of latitude and longitude points to a model object.
#' 
#' @param model A splitr modeling object
#' @param lat A latitude value in decimal degrees for the point of reference on
#'   the grid.
#' @param lon A longitude value in decimal degrees for the point of reference on
#'   the grid.
#' @param range The latitude and longitude range about the `grid_ref`.
#' @param division The division distances across the latitude and longitude
#'   ranges.
#' @param start_day The day that the grid will become active and measuring
#'   particle concentrations. This should take the form of a single-length
#'   vector for a day (`"YYYY-MM-DD"`).
#' @param start_hour The associated hour for the `start_day` variable,
#'   taking the form of a single integer hour (from `0` to `23`).
#' @param end_day The day that the grid will cease to be active and no longer
#'   measuring particle concentrations. This should take the form of a
#'   single-length vector for a day (`"YYYY-MM-DD"`).
#' @param end_hour The associated hour for the `end_day` variable, taking
#'   the form of a single integer hour (from `0` to `23`).
#' @param duration A length of time in hours that the grid will remain active
#'   from the start date-time.
#' @param heights A vector of heights for which there will be horizontal
#'   sampling grids.
#' @param samp_type The method of reporting for the sampling grid. The default
#'   is `avg` for reporting average concentrations at every sampling
#'   interval. Other options are `snapshot` and `max` for
#'   concentrations at the time of sampling and for maximum concentrations,
#'   respectively.
#' @param samp_interval The sampling interval in units of hours.
#' @param name An identifier for this set of grid parameters.
#' 
#' @export
add_grid <- function(model,
                     lat = NULL,
                     lon = NULL,
                     range = c(5, 5),
                     division = c(0.5, 0.5),
                     start_day = NULL,
                     start_hour = NULL,
                     end_day = NULL,
                     end_hour = NULL,
                     duration = NULL,
                     heights = NULL,
                     samp_type = "avg",
                     samp_interval = 24,
                     name = NULL) {
  
  if (inherits(model, "trajectory_model")) {
    
    # Obtain the grid of lat/lon points
    grid <- 
      create_grid(
        lat = lat,
        lon = lon,
        range = range,
        division = division
      )
    
    # Add the grid points to the model object
    model$lat <- grid$lat
    model$lon <- grid$lon
    
    return(model)
  }
}
