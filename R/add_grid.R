#' Add lat/lon grid to a model
#' @description Create and add a grid of latitude and
#' longitude points to a model object
#' @param model a SplitR modeling object
#' @param lat a latitude value in decimal degrees
#' for the point of reference on the grid.
#' @param lon a longitude value in decimal degrees
#' for the point of reference on the grid.
#' @param range the latitude and longitude range about
#' the \code{grid_ref}.
#' @param division the division distances across the
#' latitude and longitude ranges.
#' @param start_day the day that the grid will become
#' active and measuring particle concentrations. This
#' should take the form of a single-length vector for a
#' day (\code{"YYYY-MM-DD"}).
#' @param start_hour the associated hour for the
#' \code{start_day} variable, taking the form of a
#' single integer hour (from \code{0} to \code{23}).
#' @param end_day the day that the grid will cease to
#' be active and no longer measuring particle
#' concentrations. This should take the form of a
#' single-length vector for a day (\code{"YYYY-MM-DD"}).
#' @param end_hour the associated hour for the
#' \code{end_day} variable, taking the form of a
#' single integer hour (from \code{0} to \code{23}).
#' @param duration a length of time in hours that the
#' grid will remain active from the start date-time.
#' @param heights a vector of heights for which there
#' will be horizontal sampling grids.
#' @param samp_interval the sampling interval in units
#' of hours.
#' @param samp_type the type of sampling that will
#' occur.
#' @param samp_rate the sampling rate.
#' @param name an identifier for this set of grid
#' parameters.
#' @import lubridate
#' @export add_grid

add_grid <- function(model,
                     lat = NULL,
                     lon = NULL,
                     range = c(5, 5),
                     division = c(0.5, 0.5),
                     start_day = NULL,
                     start_hour = NULL,
                     end_day = NULL,
                     end_hour = NULL,
                     duration = NULL,
                     heights = NULL,
                     samp_interval = 0,
                     samp_type = 6,
                     samp_rate = 0,
                     name = NULL) {
  
  if (inherits(model, "traj_model")) {
    
    # Obtain the grid of lat/lon points
    grid <- 
      create_grid(
        lat = lat,
        lon = lon,
        range = range,
        division = division)
    
    # Add the grid points to the model object
    model$lat <- grid$lat
    model$lon <- grid$lon
    
    return(model)
  }
  
  if (inherits(model, "disp_model")) {
    
    if (is.null(name)) {
      if (is.null(model$grids)) {
        name <- "grid_1"
      } else {
        name <- paste0("grid_",
                       nrow(model$grids) + 1)
      }
    }
    
    if (is.null(lat)) {
      lat <- model$lat
    }
    
    if (is.null(lon)) {
      lon <- model$lon
    }
    
    if (is.null(heights)) {
      heights <- 
        c(0, 5, 10, 50, 100,
          1000, 2000, 3000, 4000, 5000,
          6000, 7000, 8000, 9000, 10000,
          11000, 12000, 13000, 14000, 15000,
          16000, 17000, 18000, 19000, 20000)
    }
    
    heights <- 
      paste(heights, collapse = " ")
    
    layers <- length(heights) - 2
    
    if (is.null(start_day)) {
      if (!is.null(model$start_day)) {
        start_day <- model$start_day
      } else {
        start_day <- NA
      }
    }
    
    if (is.null(start_hour)) {
      if (!is.null(model$start_hour)) {
        start_hour <- model$start_hour
      } else {
        start_hour <- NA
      }
    }
    
    if (is.null(end_day) & 
        is.null(end_hour)) {
      if (!is.null(model$duration)) {
        duration <- model$duration
      } else {
        duration <- NA
      }
    }
    
    if (!is.na(duration)){
      
      # Calculate end_day and end_hour
      end_day <- 0
      end_hour <- 0
      
    } else {
      end_day <- NA
      end_hour <- NA
    }
    
    # Calculate duration if start and end times are
    # available
    if (is.na(duration) &
        !is.na(start_day) &
        !is.na(start_hour) &
        !is.na(end_day) &
        !is.na(end_hour)) {
      
      duration <-
        as.numeric(ymd_h(paste0(end_day, " ", end_hour)) - 
        ymd_h(paste0(start_day, " ", start_hour))) * 24
    }
    
    # Write grid parameters to a data frame
    grid <- 
      data.frame(
        name = name,
        rate = rate,
        duration = duration,
        start_day = start_day,
        start_hour = start_hour,
        end_day = end_day,
        end_hour = end_hour,
        heights = heights,
        samp_interval = samp_interval,
        samp_type = samp_type,
        samp_rate = samp_rate,
        stringsAsFactors = FALSE)
    
    # Write data frame to the `grids` list
    # component of `model`
    if (is.null(model$grids)) {
      model$grids <- grid
    } else {
      model$grids <- 
        rbind(model$grids, grid)
    }
    
    return(model)
  }
}
