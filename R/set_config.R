#' Create a configuration list for a trajectory or dispersion model
#'
#' @param tratio The advection stability ratio. By default, this is set to
#'   `0.75`.
#' @param initd The initial distribution. Set to `0` by default.
#' @param kpuff The horizontal puff dispersion growth. Setting to `0` results in
#'   linear growth whereas option `1` uses an empirical growth scheme. By
#'   default, this is set to `0` (linear growth).
#' @param khmax The maximum duration (in hours) for a particle or trajectory.
#'   This is set to the absolute maximum by default, which is `9999` hours.
#' @param kmixd The methodology for modeling the mixed layer depth. There are
#'   three options: `0` for using the input (the default), `1` for using
#'   temperature, and `2` for using the TKE.
#' @param kmix0 The minimum mixing depth in meters. This is `250` by default.
#' @param kzmix How to perform vertical mixing adjustments. There are three
#'   options: `0` for not doing this at all (the default), `1` uses the PBL
#'   average, and `3` scales with *TVMIX*.
#' @param kdef The modeling method for horizontal turbulence. We can use the
#'   vertical with option `0` (the default), or, use the deformation method with
#'   option `1`.
#' @param kbls How to derive the boundary layer stability. Two options are
#'   available for this. We can use fluxes (option `1`, default) or use wind
#'   temperature (option `2`).
#' @param kblt The boundary layer turbulence parameterization to use. This can
#'   either be Beljaars (option `1`), Kanthar (option `2`, default), or TKE
#'   (option `3`).
#' @param conage Particle to- or from-puff conversion at *CONAGE*. In units of
#'   hours, with a default of `48`.
#' @param numpar The number of puffs or particles released per cycle. By default
#'   this is set to `2500`.
#' @param qcycle An optional cycling of emissions, in units of hours.
#' @param efile An absolute path to an optional temporal emissions file.
#' @param tkerd The unstable turbulent kinetic energy ratio. This is set to
#'   `0.18`.
#' @param tkern The stable turbulent kinetic energy ratio. This is set to
#'   `0.18`.
#' @param ninit How to do particle initialization; with `0` no particle
#'   initialization is done, with `1` this is done once, options `2` and `3` are
#'   the *add* and *replace* methods.
#' @param ndump Should the particles be dumped to a file and, if so, how often?
#'   Using `0` results in no writing particles to file (the default), and any
#'   non-zero value performs this writing once per number of hours specified.
#' @param ncycl The *PARDUMP* output cycle time.
#' @param pinpf The particle input file name (default is `"PARINIT"`). This is
#'   useful for initialization or boundary conditions.
#' @param poutf The particle output  file name (default is `"PARDUMP"`).
#' @param mgmin The minimum meteorological subgrid size. The default value is
#'   `10`.
#' @param kmsl The starting height reference. If it is to signify a distance
#'   above ground level (AGL) then use option `0` (the default). If it is
#'   instead relative to mean sea level, then option `1` should be used.
#' @param maxpar The maximum number of particles to be carried in simulation. By
#'   default this is `10000`.
#' @param cpack The binary concentration packing. Here are the options: `0` for
#'   none, `1` for nonzero, `2` for points, and `3` for polar. Option `1` is the
#'   default option.
#' @param cmass Informs grid computation. The two options are to compute grid
#'   concentrations (with `0`, the default) or to compute grid mass (option
#'   `1`).
#' @param dxf,dyf The horizontal x- and y-grid adjustment factors for an
#'   ensemble. By default, these are both set to `1.0`.
#' @param dzf The vertical factor for an ensemble. This is `0.01` by default.
#' @param ichem The chemistry conversion module to employ. Option `0` does no
#'   chemistry (the default). With option `1` we use a matrix method, option `2`
#'   does conversion, and option `3` works on dust.
#' @param maxdim The maximum number of pollutants to carry on one particle. By
#'   default, this is `1`.
#' @param kspl The standard splitting interval in units of hours. By default,
#'   this is `1` hour.
#' @param krnd The enhanced merge interval in hours. By default this is `6`
#'   hours.
#' @param frhs The standard horizontal puff rounding fraction for the merge
#'   process. This is `1.0` by default.
#' @param frvs The vertical puff rounding fraction, which is `0.01` by default.
#' @param frts The temporal puff rounding fraction, which is `0.10` by default.
#' @param frhmax The maximum value for the horizontal rounding parameter. This
#'   is `3.0` by default.
#' @param splitf The automatic size adjustment factor for horizontal splitting.
#'   By default, the value is `1.0`.
#' @param tm_pres,tm_tpot,tm_tamb,tm_rain,tm_mixd,tm_relh,tm_sphu,tm_mixr,tm_dswf,tm_terr
#'   Options to include meteorology along trajectory points. These are the
#'   pressure variable marker flag (`tm_pres`), the potential temperature
#'   (`tm_tpot`), the ambient temperature (`tm_tamb`), the rainfall rate
#'   (`tm_rain`), the mixed layer depth (`tm_mixd`), the relative humidity
#'   (`tm_relh`), the specific humidity (`tm_sphu`), the mixing rate
#'   (`tm_mixr`), the downward short-wave flux (`tm_dswf`), and the terrain
#'   height (`tm_terr`). Setting any of these to `0` disables output, whereas
#'   `1` will enable output of these data points. By default, all are set to
#'   `0`.
#'
#' @export
set_config <- function(tratio = 0.75,
                       initd = 0,
                       kpuff = 0,
                       khmax = 9999,
                       kmixd = 0,
                       kmix0 = 250,
                       kzmix = 0,
                       kdef = 0,
                       kbls = 1,
                       kblt = 2,
                       conage = 48,
                       numpar = 2500,
                       qcycle = 0.0,
                       efile = NULL,
                       tkerd = 0.18,
                       tkern = 0.18,
                       ninit = 1,
                       ndump = 1,
                       ncycl = 1,
                       pinpf = "PARINIT",
                       poutf = "PARDUMP",
                       mgmin = 10,
                       kmsl = 0,
                       maxpar = 10000,
                       cpack = 1,
                       cmass = 0,
                       dxf = 1.0,
                       dyf = 1.0,
                       dzf = 0.01,
                       ichem = 0,
                       maxdim = 1,
                       kspl = 1,
                       krnd = 6,
                       frhs = 1.0,
                       frvs = 0.01,
                       frts = 0.10,
                       frhmax = 3.0,
                       splitf = 1.0,
                       tm_pres = 0,
                       tm_tpot = 0,
                       tm_tamb = 0,
                       tm_rain = 0,
                       tm_mixd = 0, 
                       tm_relh = 0,
                       tm_sphu = 0,
                       tm_mixr = 0,
                       tm_dswf = 0,
                       tm_terr = 0) {
  
  arg_names <- formals(set_config) %>% names()
  arg_vals <- mget(arg_names)
  
  if (is.null(arg_vals$efile)) {
    arg_vals$efile <- "''"
  } else if (!is.null(arg_vals$efile)) {
    arg_vals$efile <- paste0("'", arg_vals$efile, "'")
  }
  
  if (is.null(arg_vals$pinpf)) {
    arg_vals$pinpf <- "''"
  } else if (!is.null(arg_vals$pinpf)) {
    arg_vals$pinpf <- paste0("'", arg_vals$pinpf, "'")
  }
  
  if (is.null(arg_vals$poutf)) {
    arg_vals$poutf <- "''"
  } else if (!is.null(arg_vals$poutf)) {
    arg_vals$poutf <- paste0("'", arg_vals$poutf, "'")
  }
  
  arg_vals[!vapply(arg_vals, FUN = is.null, FUN.VALUE = logical(1))]
}

write_config_list <- function(config_list, dir) {

  paste0(
    "&SETUP\n",
    paste0(names(config_list), " = ", config_list, ",\n", collapse = ""),
    "/\n"
  ) %>%
    cat(file = file.path(dir, "SETUP.CFG"))
}
