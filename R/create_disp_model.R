#' Create a dispersion model
#' @description Create a dispersion model object to
#' begin a modelling pipeline.
#' @param name an optional name for the dispersion
#' model object.
#' @export create_disp_model

create_disp_model <- function(name = NULL) {
  
  # Create the 'disp_model' list object
  disp_model <- 
    list(disp_name = NULL,
         lat = NULL,
         lon = NULL,
         height = NULL,
         duration = NULL,
         start_day = NULL,
         start_hour = NULL,
         direction = "forward",
         met_type = NULL,
         emissions = NULL,
         species = NULL,
         grids = NULL,
         vert_motion = 0,
         model_height = 20000,
         disp_df = NULL)
  
  attr(disp_model, "class") <- "disp_model"
  
  if (!is.null(name)) disp_model$disp_name <- name
  
  return(disp_model)
}
