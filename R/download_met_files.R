#' @noRd
download_met_files <- function(met_type, days, duration, direction, met_dir) {
  
  if (met_type == "gdas1") {
    
    # Get filenames for GDAS1 files
    met_files <-
      get_gdas1_filenames(
        days = days,
        duration = duration,
        direction = direction
      )
    
    # Get the GDAS1 meteorology files
    get_met_gdas1(
      files = met_files,
      path_met_files = met_dir
    )
  }
  
  if (met_type == "reanalysis") {
    
    # Get filenames for reanalysis files
    met_files <-
      get_reanalysis_filenames(
        days = days,
        duration = duration,
        direction = direction
      )
    
    get_met_reanalysis(
      files = met_files,
      path_met_files = met_dir
    )
  }
  
  if (met_type == "narr") {
    
    # Get filenames for NARR files
    met_files <-
      get_narr_filenames(
        days = days,
        duration = duration,
        direction = direction
      )
    
    get_met_narr(
      files = met_files,
      path_met_files = met_dir
    )
  }
  
  met_files
}
