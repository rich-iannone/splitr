#' Create a dispersion model
#'
#' Create a dispersion model object to begin a modeling pipeline.
#' 
#' @export
create_dispersion_model <- function() {
  
  # Create the 'disp_model' list object
  disp_model <- 
    list(
      start_time = NULL,
      end_time = NULL,
      direction = "forward",
      met_type = NULL,
      sources = NULL,
      vert_motion = 0,
      model_height = 20000,
      disp_df = NULL
    )
  
  class(disp_model) <- "dispersion_model"
  
  disp_model
}
