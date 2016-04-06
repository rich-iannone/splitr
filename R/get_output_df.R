#' Extract model data
#' @description If a model has been executed, the output
#' data frame will be available within a SplitR modeling
#' object and this function will get either the entire
#' output data frame or a subset of it.
#' @param model a SplitR modeling object.
#' @importFrom dplyr as.tbl
#' @export get_output_df

get_output_df <- function(model) {
  
  if (inherits(model, "traj_model")) {
    
    if (is.null(model$traj_df)) {
      return(NA)
    }
    
    if (!is.null(model$traj_df)) {
      return(dplyr::as.tbl(model$traj_df))
    }
  }
  
  if (inherits(model, "disp_model")) {
    
    if (is.null(model$disp_df)) {
      return(NA)
    }
    
    if (!is.null(model$disp_df)) {
      return(dplyr::as.tbl(model$disp_df))
    }
  }
}
