#' Add model parameters
#' @description Add modelling parameters to a model
#' object
#' @param model a SplitR modeling object.
#' @param lat the starting latitude (in decimal 
#' degrees) for the model run(s).
#' @param lon the starting longitude (in decimal 
#' degrees) for the model run(s).
#' @param height the starting height (in meters above 
#' ground level) for the model run(s).
#' @param duration the duration of one or several
#  model runs (either \code{forward} or
#' \code{backward}) in hours.
#' @param run_period the extended period (i.e., days,
#' years) when the model will initialize and run. This
#' can take the form of a single-length vector for a
#' day (\code{"YYYY-MM-DD"}) or year (\code{YYYY}), or,
#' a vector of length 2 to specify the range of days or
#' years.
#' @param start_day is specific to a dispersion model
#' and it refers to the day that the model will 
#' initialize and run. This should take the form of a
#' single-length vector for a day (\code{"YYYY-MM-DD"}).
#' @param start_hour is specific to a dispersion model 
#' and it should indicate which hour in the
#' \code{start_day} that modeling will begin. Provide
#' this as an integer (from \code{0} to \code{23}).
#' @param daily_hours are specific to a trajectory
#' model and should consist of a single daily hour as
#' an integer (from \code{0} to \code{23}), or, a
#' vector of several daily hours represented as
#' integers.
#' @param direction an option to select whether to
#' conduct the model in the \code{forward} or 
#' \code{backward} directions.
#' @param met_type an option to select meteorological
#' data files. The options are \code{gdas1} (Global
#' Data Assimilation System 1-degree resolution data), 
#' \code{reanalysis} (NCAR/NCEP global reanalysis
#' data), and \code{narr} (North American Regional 
#' Reanalysis).
#' @param vert_motion a numbered option to
#' select the method used to simulation vertical
#' motion. The methods are: \code{0} (input model
#' data), \code{1} (isobaric), \code{2} (isentropic),
#' \code{3} (constant density), \code{4} (isosigma),
#' \code{5} (from divergence), \code{6} (remap MSL to
#' AGL), \code{7} (average data), and \code{8} (damped
#' magnitude). 
#' @param model_height the upper limit of the model
#' domain in meters.
#' @param traj_name an optional, descriptive name for
#' the output file collection.
#' @param exec_dir an optional file path for the
#' working directory of the model input and output
#' files.
#' @param met_dir an optional file path for storage and
#' access of meteorological data files.
#' @param binary_path an optional path to a HYSPLIT
#' trajectory model binary. When not specified, the
#' model binary will be chosen from several available
#' in the package (based on the user's platform).
#' @export add_params

add_params <- function(model,
                       lat = NULL,
                       lon = NULL,
                       height = NULL,
                       duration = NULL,
                       run_period = NULL,
                       start_day = NULL,
                       start_hour = NULL,
                       daily_hours = NULL,
                       direction = NULL,
                       met_type = NULL,
                       vert_motion = NULL,
                       model_height = NULL,
                       traj_name = NULL,
                       exec_dir = NULL,
                       met_dir = NULL,
                       binary_path = NULL) {
  
  if (!is.null(lat)) {
    model$lat <- lat
  }
  
  if (!is.null(lon)) {
    model$lon <- lon
  }
  
  if (!is.null(height)) {
    model$height <- height
  }
  
  if (!is.null(duration)) {
    model$duration <- duration
  }
  
  if (!is.null(run_period)) {
    model$run_period <- run_period
  }

  if (!is.null(start_day)) {
    model$start_day <- start_day
  }
  
  if (!is.null(start_hour)) {
    model$start_hour <- start_hour
  }
  
  if (!is.null(daily_hours)) {
    model$daily_hours <- daily_hours
  }
  
  if (!is.null(direction)) {
    model$direction <- direction
  }
  
  if (!is.null(met_type)) {
    model$met_type <- met_type
  }
  
  if (!is.null(vert_motion)) {
    model$vert_motion <- vert_motion
  }
  
  if (!is.null(model_height)) {
    model$model_height <- model_height
  }
  
  if (!is.null(traj_name)) {
    model$traj_name <- traj_name
  }
  
  if (!is.null(exec_dir)) {
    model$exec_dir <- exec_dir
  }
  
  if (!is.null(met_dir)) {
    model$met_dir <- met_dir
  }
  
  if (!is.null(binary_path)) {
    model$binary_path <- binary_path
  }
  
  return(model)
}
