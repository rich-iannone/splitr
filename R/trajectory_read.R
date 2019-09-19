#' Read HYSPLIT trajectory output files into a data frame
#'
#' The function takes HYSPLIT trajectory output files in a specified output
#' directory and processes all files into a data frame object.
#' @param output_folder The path of the directory containing the trajectory
#'   endpoints files.
#' @return A tibble with HYSPLIT trajectory data.
#' @examples
#' \dontrun{
#' # Process all trajectory output files in the
#' # specified output directory
#' trajectory_df <-
#'   trajectory_read(
#'     output_folder = "traj--2015-06-16--23-58-44")
#' }
#' @export
trajectory_read <- function(output_folder) {  
  
  # Get file list for trajectories from the specified folder
  trajectory_file_list <- 
    list.files(
      path = output_folder,
      pattern = "^traj-.*"
    )
  
  # Initialize empty tibble with 12 columns
  traj_tbl <-
    dplyr::tibble(
      receptor = integer(0),
      year = integer(0),
      month = integer(0),
      day = integer(0),
      hour = integer(0),
      hour_along = integer(0),
      lat = numeric(0),
      lon = numeric(0), 
      height = numeric(0),
      pressure = numeric(0),
      traj_dt = lubridate::as_datetime("2015-01-01")[-1],
      traj_dt_i = lubridate::as_datetime("2015-01-01")[-1]
    )
  
  extended_col_names <- 
    c(
      "year", "month", "day", "hour", "hour_along",
      "lat", "lon", "height", "pressure",
      "theta", "air_temp", "rainfall", "mixdepth", "rh", "sp_humidity", 
      "h2o_mixrate", "terr_msl", "sun_flux"
    )
  
  standard_col_names <- 
    c(
      "year", "month", "day", "hour", "hour_along",
      "lat", "lon", "height", "pressure"
    )
  
  # Process all trajectory files
  for (file_i in trajectory_file_list) {
    
    file_i_path <- file.path(output_folder, file_i)

    file_lines <- readLines(file_i_path, encoding = "UTF-8", skipNul = TRUE)
    
    file_one_line <- readr::read_file(file_i_path)
    
    header_line <- 
      file_lines %>%
      vapply(
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE,
        function(x) tidy_grepl(x, "PRESSURE")
      ) %>%
      which()
    
    file_lines_data <- 
      file_lines[(header_line + 1):(length(file_lines))] %>%
      tidy_gsub("\\s\\s*", " ") %>%
      tidy_gsub("^ ", "")
    
    if (!file_one_line %>% tidy_grepl("AIR_TEMP")) {
      
      #
      # Standard meteorology
      #
      
      traj_tbl_i <- 
        file_lines_data %>%
        strsplit("\\s+") %>%
        lapply(
          FUN = function(x) {
            x[c(3:6, 9:13)] %>%
              as.numeric() %>%
              stats::setNames(standard_col_names) %>%
              as.list() %>%
              dplyr::as_tibble()
          }
        ) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate_at(
          .vars = dplyr::vars(year, month, day, hour, hour_along), 
          .funs = as.integer
        ) %>%
        dplyr::mutate(year_full = ifelse(year < 50, year + 2000, year + 1900)) %>%
        tidyr::unite(col = date_str, year_full, month, day, sep = "-", remove = FALSE) %>%
        tidyr::unite(col = date_h_str, date_str, hour, sep = " ", remove = FALSE) %>%
        dplyr::mutate(traj_dt = lubridate::ymd_h(date_h_str)) %>%
        dplyr::select(-c(date_h_str, date_str, year_full)) %>%
        dplyr::mutate(traj_dt_i = traj_dt[1])
      
      traj_tbl <- traj_tbl %>% dplyr::bind_rows(traj_tbl_i)
    }
    
    if (file_one_line %>% tidy_grepl("AIR_TEMP")) {
      
      #
      # Extended meteorology
      #
      
      file_lines_data_20 <- 
        file_lines_data %>%
        vapply(
          FUN.VALUE = logical(1),
          USE.NAMES = FALSE,
          function(x) {
            tidy_grepl(
              x, 
              paste0(
                "^",
                rep("[0-9\\.-]*?", 20) %>% paste(collapse = " "),
                "$"
              )
            )
          }
        )
      
      file_lines_data_02 <- 
        file_lines_data %>%
        vapply(
          FUN.VALUE = logical(1),
          USE.NAMES = FALSE,
          function(x) {
            tidy_grepl(
              x, 
              paste0(
                "^",
                rep("[0-9\\.-]*?", 2) %>% paste(collapse = " "),
                "$"
              )
            )
          }
        )
      
      traj_tbl_i <- 
        paste(file_lines_data[file_lines_data_20], file_lines_data[file_lines_data_02]) %>%
        strsplit("\\s+") %>%
        lapply(
          FUN = function(x) {
            x[c(3:6, 9:22)] %>%
              as.numeric() %>%
              stats::setNames(extended_col_names) %>%
              as.list() %>%
              dplyr::as_tibble()
          }
        ) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate_at(
          .vars = dplyr::vars(year, month, day, hour, hour_along), 
          .funs = as.integer
        ) %>%
        dplyr::mutate(year_full = ifelse(year < 50, year + 2000, year + 1900)) %>%
        tidyr::unite(col = date_str, year_full, month, day, sep = "-", remove = FALSE) %>%
        tidyr::unite(col = date_h_str, date_str, hour, sep = " ", remove = FALSE) %>%
        dplyr::mutate(traj_dt = lubridate::ymd_h(date_h_str)) %>%
        dplyr::select(-c(date_h_str, date_str, year_full)) %>%
        dplyr::mutate(traj_dt_i = traj_dt[1])
      
      traj_tbl <- traj_tbl %>% dplyr::bind_rows(traj_tbl_i)
    }
  }
  
  traj_tbl
}
