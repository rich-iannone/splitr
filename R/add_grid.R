#' Add lat/lon grid to a model
#' @description Create and add a grid of latitude and
#' longitude points to a model object
#' @param model a SplitR modeling object
#' @param lat a latitude value in decimal degrees
#' for the point of reference on the grid.
#' @param lon a longitude value in decimal degrees
#' for the point of reference on the grid.
#' @param range the latitude and longitude range about
#' the \code{grid_ref}.
#' @param division the division distances across the
#' latitude and longitude ranges.
#' @param start_date
#' @param start_time
#' @param end_date
#' @param end_time
#' @param duration
#' @param samp_interval
#' @param samp_type
#' @param samp_rate
#' @export add_grid

add_grid <- function(model,
                     lat = 49.263,
                     lon = -123.250,
                     range = c(5, 5),
                     division = c(0.5, 0.5),
                     start_date = NULL,
                     start_hour = NULL,
                     end_date = NULL,
                     end_hour = NULL,
                     duration = NULL,
                     heights = NULL,
                     samp_interval = NULL,
                     samp_type = 0,
                     samp_rate = 6,
                     name = 0) {
  
  if (inherits(model, "traj_model")) {
    
    # Obtain the grid of lat/lon points
    grid <- 
      create_grid(
        lat = lat,
        lon = lon,
        range = range,
        division = division)
    
    # Add the grid points to the model object
    model$lat <- grid$lat
    model$lon <- grid$lon
    
    return(model)
  }
  
  if (inherits(model, "disp_model")) {
    
    if (is.null(name)) {
      if (is.null(model$grids)) {
        name <- "grid_1"
      } else {
        name <- paste0("grid_",
                       nrow(model$grids) + 1)
      }
    }
    
    if (is.null(lat)) {
      lat <- model$lat
    }
    
    if (is.null(lon)) {
      lon <- model$lon
    }
    
    if (is.null(heights)) {
      heights <- 
        c(0, 5, 10, 50, 100,
          1000, 2000, 3000, 4000, 5000,
          6000, 7000, 8000, 9000, 10000,
          11000, 12000, 13000, 14000, 15000,
          16000, 17000, 18000, 19000, 20000)
    }
    
    layers <- length(heights) - 2
    
    if (is.null(start_day)) {
      start_day <- model$start_day
    }
    
    if (is.null(start_hour)) {
      start_day <- model$start_hour
    }
    
    if (is.null(end_day) & 
        is.null(end_hour)) {
      duration <- model$duration
    }
    
    if (is.null(start_hour)) {
      start_day <- model$start_hour
    }
    
    # Write grid parameters to a data frame
    grid <- 
      data.frame(
        name = name,
        rate = rate,
        duration = duration,
        start_day = start_day,
        start_hour = start_hour,
        end_day = end_day,
        end_hour = end_hour,
        heights = heights,
        samp_interval = samp_interval,
        samp_type = samp_type,
        samp_rate = samp_rate,
        stringsAsFactors = FALSE)
    
    # Write data frame to the `grids` list
    # component of `model`
    if (is.null(model$grids)) {
      model$grids <- grid
    } else {
      model$grids <- 
        rbind(model$grids, grid)
    }
    
    return(model)
  }
  
}
