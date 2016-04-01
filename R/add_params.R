#' Add model parameters
#' @description Add modelling parameters to a model
#' object
#' @param model a SplitR modeling object
#' @export add_params

add_params <- function(model,
                       lat = NULL,
                       lon = NULL,
                       height = NULL,
                       duration = NULL,
                       run_period = NULL,
                       daily_hours = NULL,
                       backtrajectory = NULL,
                       met_type = NULL,
                       vert_motion = NULL,
                       model_height = NULL){
  
  if (!is.null(lat)){
    model$lat <- lat
  }
  
  if (!is.null(lon)){
    model$lon <- lon
  }
  
  if (!is.null(height)){
    model$height <- height
  }
  
  if (!is.null(duration)){
    model$duration <- duration
  }
  
  if (!is.null(run_period)){
    model$run_period <- run_period
  }

  if (!is.null(daily_hours)){
    model$daily_hours <- daily_hours
  }
  
  if (!is.null(backtrajectory)){
    model$backtrajectory <- backtrajectory
  }
  
  if (!is.null(met_type)){
    model$met_type <- met_type
  }
  
  if (!is.null(vert_motion)){
    model$vert_motion <- vert_motion
  }
  
  if (!is.null(model_height)){
    model$model_height <- model_height
  }
  
  return(model)
}
