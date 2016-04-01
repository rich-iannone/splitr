#' Get the trajectory data frame from a model object
#' @description If a data frame is available within a 
#' model object, get the entire data frame or a subset
#' of it
#' @param model a SplitR modeling object
#' @export get_traj_df

get_traj_df <- function(model){

  if (is.null(model$traj_df)){
    return(NA)
  }
  
  if (!is.null(model$traj_df)){
    return(traj_df)
  }
}
