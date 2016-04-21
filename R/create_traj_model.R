#' Create a trajectory model
#' @description Create a trajectory model object to
#' begin a modelling pipeline.
#' @param name an optional name for the trajectory
#' model object.
#' @export create_traj_model

create_traj_model <- function(name = NULL) {
  
  # Create the `traj_model` list object
  traj_model <- 
    list(traj_name = NULL,
         lat = NULL,
         lon = NULL,
         height = NULL,
         duration = NULL,
         run_period = NULL,
         daily_hours = NULL,
         direction = "forward",
         met_type = NULL,
         vert_motion = 0,
         model_height = 20000,
         traj_df = NULL,
         exec_dir = NULL,
         met_dir = NULL,
         binary_path = NULL)
  
  attr(traj_model, "class") <- "traj_model"
  
  if (!is.null(name)) traj_model$traj_name <- name
  
  return(traj_model)
}
