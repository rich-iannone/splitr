#' Extract model data
#'
#' If a model has been executed, the output tibble will be available within a
#' splitr modeling object and this function will return the entire output
#' tibble.
#' 
#' @param model A splitr modeling object.
#' 
#' @export
get_output_tbl <- function(model) {
  
  if (inherits(model, "trajectory_model")) {
    
    if (is.null(model$traj_df)) {
      return(NA)
    }
    
    if (!is.null(model$traj_df)) {
      return(dplyr::as.tbl(model$traj_df))
    }
  }
  
  if (inherits(model, "dispersion_model")) {
    
    if (is.null(model$disp_df)) {
      return(NA)
    }
    
    if (!is.null(model$disp_df)) {
      return(dplyr::as.tbl(model$disp_df))
    }
  }
}
