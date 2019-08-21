#' Read HYSPLIT dispersion output files
#'
#' The function takes HYSPLIT dispersion output files in a specified output
#' directory and processes all files into a data frame object.
#'
#' @param dispersion_dir The absolute path of the directory containing
#'   dispersion particle output files.
#'
#' @noRd
create_dispersion_tbl <- function(dispersion_dir) {  
  
  dispersion_file_list <-
    list.files(
      path = dispersion_dir,
      pattern = "^GIS_part_[0-9][0-9][0-9]_ps.csv",
      full.names = TRUE
    )
  
  dispersion_tbl <-
    dplyr::tibble(
      particle_i = character(0),
      hour = integer(0),
      lat = numeric(0),
      lon = numeric(0),
      height = numeric(0)
    )
  
  # Get each CSV file into a single data frame
  for (i in seq(dispersion_file_list)) {
    
    disp_tbl <-
      readr::read_csv(
        file = dispersion_file_list[i],
        col_names = c("particle_i", "lon", "lat", "height"),
        col_types = "cddd"
      ) %>%
      dplyr::mutate(hour = as.integer(i)) %>%
      dplyr::select(particle_i, hour, lat, lon, height)
    
    dispersion_tbl <-
      dplyr::bind_rows(dispersion_tbl, disp_tbl)
  }
  
  dispersion_tbl
}
