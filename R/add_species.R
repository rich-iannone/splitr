#' Add species parameters to a dispersion model
#' @description Add a set of species parameters to
#' a dispersion model object. Multiple sets of
#' species parameters can can be added to a single
#' dispersion model object.
#' @param model a SplitR modeling object.
#' @param name an identifier for the species.
#' @param pdiam the particle diameter in units of
#' micrometers.
#' @param density the particle density in units of g/cm3.
#' @param shape_factor the particle shape factor as a
#' value in the range \code{0}-\code{1}.
#' @param ddep_vel the dry deposition velocity in units
#' of m/s.
#' @param ddep_mw the molecular weight in units of g/mol.
#' @param ddep_a_ratio the dry deposition A ratio.
#' @param ddep_d_ratio the dry deposition D ratio.
#' @param ddep_hl_coeff the Henry's Law value associated
#' with dry deposition.
#' @param wdep_hl_coeff the Henry's Law value associated
#' with wet deposition.
#' @param wdep_in_cloud the in-cloud wet deposition
#' rate in units of L/L.
#' @param wdep_below_cloud the below cloud wet deposition
#' rate in units of 1/s.
#' @param rad_decay the rate of radioactive decay, in
#' units of days.
#' @param resuspension the pollutant resuspension factor
#' in units of 1/m.
#' @export add_species

add_species <- function(model,
                        name = NULL,
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

  if (is.null(name)) {
    if (is.null(model$species)) {
      name <- "species_1"
    } else {
      name <- paste0("species_",
                     nrow(model$species) + 1)
    }
  }
  
  if (is.null(pdiam)) {
    pdiam <- 15.0
  }
  
  if (is.null(density)) {
    density <- 1.0
  }
  
  if (is.null(shape_factor)) {
    shape_factor <- 1.0
  }
  
  if (is.null(ddep_vel)) {
    ddep_vel <- 0.0
  }
  
  if (is.null(ddep_mw)) {
    ddep_mw <- 0.0
  }
  
  if (is.null(ddep_a_ratio)) {
    ddep_a_ratio <- 0.0
  }
  
  if (is.null(ddep_d_ratio)) {
    ddep_d_ratio <- 0.0
  }
  
  if (is.null(ddep_hl_coeff)) {
    ddep_hl_coeff <- 0.0
  }
  
  if (is.null(wdep_hl_coeff)) {
    wdep_hl_coeff <- 0.0
  }
  
  if (is.null(wdep_in_cloud)) {
    wdep_in_cloud <- 0.0
  }
  
  if (is.null(wdep_below_cloud)) {
    wdep_below_cloud <- 0.0
  }
  
  if (is.null(rad_decay)) {
    rad_decay <- 0.0
  }
  
  if (is.null(resuspension)) {
    resuspension <- 0.0
  }
  
  # Write species parameters to a data frame
  species <- 
    data.frame(
      name = name,
      pdiam = pdiam,
      density = density,
      shape_factor = shape_factor,
      ddep_vel = ddep_vel,
      ddep_mw = ddep_mw,
      ddep_a_ratio = ddep_a_ratio,
      ddep_d_ratio = ddep_d_ratio,
      ddep_hl_coeff = ddep_hl_coeff,
      wdep_hl_coeff = wdep_hl_coeff,
      wdep_in_cloud = wdep_in_cloud,
      wdep_below_cloud = wdep_below_cloud,
      rad_decay = rad_decay,
      resuspension = resuspension,
      stringsAsFactors = FALSE)
  
  # Write data frame to the `species` list
  # component of `model`
  if (is.null(model$species)) {
    model$species <- species
  } else {
    model$species <- 
      rbind(model$species, species)
  }
  
  return(model)
}
