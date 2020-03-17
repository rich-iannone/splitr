#' @noRd
download_met_files <- function(met_type,
                               days,
                               duration,
                               direction,
                               met_dir) {
  
  if (met_type == "gdas1") {
    
    met_files <-
      get_met_gdas1(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "gdas0.5") {
    
    met_files <-
      get_met_gdas0p5(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "gfs0.25") {
    
    met_files <-
      get_met_gfs0p25(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "reanalysis") {
    
    met_files <-
      get_met_reanalysis(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "nam12") {
    
    met_files <-
      get_met_nam12(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "narr") {
    
    met_files <-
      get_met_narr(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
    if (met_type == "era5") {
    
    met_files <-
      get_met_era5(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }

  met_files
}
