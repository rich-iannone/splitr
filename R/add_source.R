#' Add source parameters to a dispersion model
#'
#' Add a set of source parameters to a dispersion model.
#' 
#' @param model A dispersion model object.
#' @param name An identifier for the source
#' @param lat,lon,height The source position in terms of latitude and longitude
#'   (both in decimal degrees), and height in meters above ground level.
#' @param release_start,release_end The beginning and ending times for the
#'   source emissions.
#' @param rate The particle emission rate.
#' @param pdiam The particle diameter in units of micrometers.
#' @param density The particle density in units of g/cm3.
#' @param shape_factor The particle shape factor as a value in the range
#'   `0`-`1`.
#' @param ddep_vel The dry deposition velocity in units of m/s.
#' @param ddep_mw The molecular weight in units of g/mol.
#' @param ddep_a_ratio The dry deposition A ratio.
#' @param ddep_d_ratio The dry deposition D ratio.
#' @param ddep_hl_coeff The Henry's Law value associated with dry deposition.
#' @param wdep_hl_coeff The Henry's Law value associated with wet deposition.
#' @param wdep_in_cloud The in-cloud wet deposition rate in units of L/L.
#' @param wdep_below_cloud The below cloud wet deposition rate in units of 1/s.
#' @param rad_decay The rate of radioactive decay, in units of days.
#' @param resuspension The pollutant resuspension factor in units of 1/m.
#' 
#' @export
add_source <- function(model,
                       name = NULL,
                       lat = NULL,
                       lon = NULL,
                       height = NULL,
                       release_start = NULL,
                       release_end = NULL,
                       rate = NULL,
                       pdiam = NULL,
                       density = NULL,
                       shape_factor = NULL,
                       ddep_vel = NULL,
                       ddep_mw = NULL,
                       ddep_a_ratio = NULL,
                       ddep_d_ratio = NULL,
                       ddep_hl_coeff = NULL,
                       wdep_hl_coeff = NULL,
                       wdep_in_cloud = NULL,
                       wdep_below_cloud = NULL,
                       rad_decay = NULL,
                       resuspension = NULL) {

  if (any(is.null(lat), is.null(lon), is.null(height))) {
    
    stop("The `lat`, `lon`, and `height` values must be provided.",
         call. = FALSE)
  }
  
  if (is.null(name)) {
    name <- NA_character_
  }
  
  if (is.null(release_start)) {
    release_start <- lubridate::ymd_hms("1900-01-01 00:00:00")
  }
  
  if (is.null(release_end)) {
    release_end <- lubridate::ymd_hms("1900-01-01 00:00:00")
  }
  
  arg_names <- 
    formals(add_source) %>%
    names() %>%
    base::setdiff(
      c("model", "name", "lat", "lon", "height",
        "release_start", "release_end"
      )
    )
  
  species_vals <- mget(x = arg_names)
  
  species_defaults <- c(5, 15.0, rep(1.0, 2), rep(0.0, 10))
  
  missing_values <- vapply(species_vals, is.null, logical(1), USE.NAMES = FALSE)
  
  species_vals[missing_values] <- species_defaults[missing_values]
  
  dispersion_source_line <-
    species_vals %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      name = name,
      lat = lat,
      lon = lon,
      height = height,
      release_start = release_start,
      release_end = release_end
    ) %>%
    dplyr::select(
      name, lat, lon, height, dplyr::starts_with("release"), dplyr::everything()
    )
  
  if (is.null(model$sources)) {
    model$sources <- dispersion_source_line
  } else {
    model$sources <- dplyr::bind_rows(model$sources, dispersion_source_line)
  }
  
  model
}
