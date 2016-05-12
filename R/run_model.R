#' Run the model
#' @description Run either a trajectory model or a
#' dispersion model, depending on the class of the
#' SplitR modeling object
#' @param model a SplitR modeling object
#' @export run_model

run_model <- function(model) {
  
  if (inherits(model, "traj_model")) {

    traj_df <- 
      hysplit_trajectory(
        lat = model$lat,
        lon = model$lon,
        height = model$height,
        duration = ifelse(is.null(model$duration),
                          24, model$duration),
        run_period = model$run_period,
        daily_hours = model$daily_hours,
        direction = ifelse(is.null(model$direction),
                           "forward", model$direction),
        met_type = ifelse(is.null(model$met_type),
                          "reanalysis", model$met_type),
        vert_motion = ifelse(is.null(model$vert_motion),
                             0, model$vert_motion),
        model_height = ifelse(is.null(model$model_height),
                              20000, model$model_height),
        extended_met = TRUE,
        return_traj_df = TRUE,
        traj_name = model$traj_name,
        exec_dir = model$exec_dir,
        met_dir = model$met_dir,
        binary_path = model$binary_path
      )
    
    model$traj_df <- traj_df
    return(model)
  }
  
  if (inherits(model, "disp_model")) {
    
    disp_df <- 
      hysplit_dispersion(
        lat = model$lat,
        lon = model$lon,
        height = ifelse(is.null(model$height),
                        50, model$height),
        duration = ifelse(is.null(model$duration),
                          24, model$duration),
        start_day = model$start_day,
        start_hour = model$start_hour,
        direction = ifelse(is.null(model$direction),
                           "forward", model$direction),
        met_type = ifelse(is.null(model$met_type),
                          "reanalysis", model$met_type),
        vert_motion = ifelse(is.null(model$vert_motion),
                             0, model$vert_motion),
        model_height = ifelse(is.null(model$model_height),
                              20000, model$model_height),
        particle_num = 2500,
        particle_max = 10000,
        emissions = model$emissions,
        species = model$species,
        grids = model$grids,
        return_disp_df = TRUE,
        write_disp_CSV = TRUE
      )
   
    model$disp_df <- disp_df
    return(model) 
  }
}
