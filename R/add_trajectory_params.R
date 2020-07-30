#' Add model parameters
#'
#' Add modelling parameters to a model object.
#' 
#' @param model A splitr modeling object.
#' @inheritParams hysplit_trajectory
#' @inheritParams hysplit_dispersion
#' @export
add_trajectory_params <- function(model,
                                  lat = NULL,
                                  lon = NULL,
                                  height = NULL,
                                  duration = NULL,
                                  days = NULL,
                                  daily_hours = NULL,
                                  direction = "forward",
                                  met_type = "reanalysis",
                                  vert_motion = 0,
                                  model_height = 2000,
                                  extended_met = FALSE,
                                  config = NULL,
                                  ascdata = NULL,
                                  traj_name = NULL,
                                  binary_path = NULL,
                                  met_dir = NULL,
                                  exec_dir = NULL,
                                  softrun = FALSE,
                                  clean_up = TRUE) {
  
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
  
  if (!is.null(softrun)) {
    model$softrun <- softrun
  }
  
  if (!is.null(clean_up)) {
    model$clean_up <- clean_up
  }
  
  model
}


#' @export
add_dispersion_params <- function(model,
                                  start_time = NULL,
                                  end_time = NULL,
                                  direction = NULL,
                                  met_type = NULL,
                                  vert_motion = NULL,
                                  model_height = NULL,
                                  exec_dir = NULL,
                                  met_dir = NULL,
                                  binary_path = NULL,
                                  binary_name = NULL,
                                  softrun = FALSE,
                                  clean_up = TRUE) {
  
  if (!is.null(start_time)) {
    model$start_time <- start_time
  }
  
  if (!is.null(end_time)) {
    model$end_time <- end_time
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
  
  if (!is.null(exec_dir)) {
    model$exec_dir <- exec_dir
  }
  
  if (!is.null(met_dir)) {
    model$met_dir <- met_dir
  }
  
  if (!is.null(binary_path)) {
    model$binary_path <- binary_path
  }
  
  if (!is.null(softrun)) {
    model$softrun <- softrun
  }
  
  if (!is.null(clean_up)) {
    model$clean_up <- clean_up
  }
  
  model
}
