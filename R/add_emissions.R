#' Add emissions parameters to a dispersion model
#'
#' Add a set of emissions parameters to a dispersion model object. Multiple sets
#' of emissions parameters can can be added to a single dispersion model object.
#' 
#' @param model a splitr modeling object.
#' @param rate the rate of emissions for the pollutant in mass units per hour.
#' @param duration the duration of emissions in hours.
#' @param start_day the day that the emissions will begin. This should take the
#'   form of a single-length vector for a day (`"YYYY-MM-DD"`).
#' @param start_hour a single daily hour as an integer hour (from `0` to
#'   `23`).
#' @param name an identifier for the group of emissions parameters.
#' 
#' @export
add_emissions <- function(model,
                          rate = NULL,
                          duration = NULL,
                          start_day = NULL,
                          start_hour = NULL,
                          name = NULL) {
  
  if (is.null(name)) {
    if (is.null(model$emissions)) {
      name <- "emissions_1"
    } else {
      name <- paste0("emissions_", nrow(model$emissions) + 1
      )
    }
  }
  
  if (is.null(rate)) {
    rate <- 1
  }
  
  if (is.null(duration)) {
    duration <- 1
  }
  
  if (is.null(start_day)) {
    start_day <- "10-05-01"
  }
  
  if (is.null(start_hour)) {
    start_hour <- 0
  }
  
  # Write emissions parameters to a data frame
  emissions <- 
    data.frame(
      name = name,
      rate = rate,
      duration = duration,
      start_day = start_day,
      start_hour = start_hour,
      stringsAsFactors = FALSE
    )
  
  # Write data frame to the `emissions` list
  # component of `model`
  if (is.null(model$emissions)) {
    model$emissions <- emissions
  } else {
    model$emissions <- rbind(model$emissions, emissions)
  }
  
  model
}
