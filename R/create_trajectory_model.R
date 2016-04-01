#' Create a trajectory model
#' @description Create a trajectory model object to
#' begin a modelling pipeline.
#' @export create_trajectory_model

create_trajectory_model <- function(traj_name = NULL){
  
  # Create the 'traj_model' list object
  traj_model <- 
    list(traj_name = NULL,
         lat = NULL,
         lon = NULL,
         height = NULL,
         duration = NULL,
         run_type = NULL,
         run_day = NULL,
         run_range = NULL,
         run_years = NULL,
         daily_hours = NULL,
         backtrajectory = FALSE,
         met_type = NULL,
         vert_motion = 0,
         model_height = 20000,
         return_traj_df = TRUE,
         traj_df = NULL)
  
  attr(traj_model, "class") <- "traj_model"
  
  if (!is.null(traj_name)) traj_model$traj_name <- traj_name
  
  return(traj_model)
}
