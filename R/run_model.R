#' Add model parameters
#' @description Add modelling parameters to a model
#' object
#' @param model a SplitR modeling object
#' @export run_model

run_model <- function(model){
  
  traj_df <- 
    hysplit_trajectory(
      lat = model$lat,
      lon = model$lon,
      height = ifelse(is.null(model$height),
                      50, model$height),
      duration = ifelse(is.null(model$duration),
                        24, model$duration),
      run_period = ifelse(is.null(model$run_period),
                       "2015-07-01", model$run_period),
      daily_hours = ifelse(is.null(model$daily_hours),
                           0, model$daily_hours),
      backtrajectory = ifelse(is.null(model$backtrajectory),
                              FALSE, model$backtrajectory),
      met_type = ifelse(is.null(model$met_type),
                        "reanalysis", model$met_type),
      vert_motion = ifelse(is.null(model$vert_motion),
                           0, model$vert_motion),
      model_height = ifelse(is.null(model$model_height),
                            20000, model$model_height),
      extended_met = TRUE,
      return_traj_df = TRUE
    )
  
  model$traj_df <- traj_df
  
  return(model)
}
