#' Create a trajectory model
#' 
#' Create a trajectory model object to begin a modeling pipeline.
#' @param name An optional name for the trajectory model object.
#' @export
create_trajectory_model <- function(name = NULL) {
  
  # Create the `traj_model` list object
  traj_model <- 
    list(
      traj_name = NULL,
      lat = NULL,
      lon = NULL,
      height = NULL,
      duration = NULL,
      days = NULL,
      daily_hours = NULL,
      direction = "forward",
      met_type = NULL,
      vert_motion = 0,
      model_height = 20000,
      traj_df = NULL,
      exec_dir = NULL,
      met_dir = NULL,
      binary_path = NULL
    )
  
  if (!is.null(name)) traj_model$traj_name <- name
  
  class(traj_model) <- "trajectory_model"
  
  traj_model
}
