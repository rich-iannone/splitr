#' Add model parameters
#'
#' Add modelling parameters to a model object.
#' 
#' @param model A splitr modeling object.
#' @inheritParams hysplit_trajectory
#' @inheritParams hysplit_dispersion
#' @export
add_params <- function(model,
                       lat = NULL,
                       lon = NULL,
                       height = NULL,
                       duration = NULL,
                       days = NULL,
                       start_day = NULL,
                       start_hour = NULL,
                       daily_hours = NULL,
                       direction = NULL,
                       met_type = NULL,
                       vert_motion = NULL,
                       model_height = NULL,
                       traj_name = NULL,
                       exec_dir = NULL,
                       met_dir = NULL,
                       binary_path = NULL) {
  
  if (!is.null(lat)) {
    model$lat <- lat
  }
  
  if (!is.null(lon)) {
    model$lon <- lon
  }
  
  if (!is.null(height)) {
    model$height <- height
  }
  
  if (!is.null(duration)) {
    model$duration <- duration
  }
  
  if (!is.null(days)) {
    model$days <- days
  }

  if (!is.null(start_day)) {
    model$start_day <- start_day
  }
  
  if (!is.null(start_hour)) {
    model$start_hour <- start_hour
  }
  
  if (!is.null(daily_hours)) {
    model$daily_hours <- daily_hours
  }
  
  if (!is.null(direction)) {
    model$direction <- direction
  }
  
  if (!is.null(met_type)) {
    model$met_type <- met_type
  }
  
  if (!is.null(vert_motion)) {
    model$vert_motion <- vert_motion
  }
  
  if (!is.null(model_height)) {
    model$model_height <- model_height
  }
  
  if (!is.null(traj_name)) {
    model$traj_name <- traj_name
  }
  
  if (!is.null(exec_dir)) {
    model$exec_dir <- exec_dir
  }
  
  if (!is.null(met_dir)) {
    model$met_dir <- met_dir
  }
  
  if (!is.null(binary_path)) {
    model$binary_path <- binary_path
  }
  
  model
}
