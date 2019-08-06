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
      pattern = "^traj.*"
    )
  
  # Initialize empty tibble with 12 columns
  traj_df <-
    dplyr::tibble(
      receptor = integer(0),
      year = integer(0),
      month = integer(0),
      day = integer(0),
      hour = integer(0),
      hour.inc = integer(0),
      lat = numeric(0),
      lon = numeric(0), 
      height = numeric(0),
      pressure = numeric(0),
      date2 = lubridate::as_datetime("2015-01-01")[-1],
      date = lubridate::as_datetime("2015-01-01")[-1]
    )
  
  # Process all trajectory files
  for (file_i in trajectory_file_list) {
    
    # For each trajectory file, read each line and
    # determine where the variable-length header ends
    column_widths <- 92
    
    file_i_path <- file.path(output_folder, file_i)
    
    file_lines <- readLines(file_i_path)
    widths <- nchar(file_lines)
    
    if (any(widths == 18)) {
      
      read_characters <- readChar(file_i_path, file.info(file_i_path)$size)
      
      cat(gsub("([-0-9\\. ]{19}[-0-9\\. ])\n",
               "\\1", read_characters),
          file = file_i_path,
          sep = "\n")
    }
    
    traj_temp <- 
      try(utils::read.fwf(file = file_i_path, widths = column_widths))
    
    if (class(traj_temp) != "try-error") {
      
      for (j in seq(nrow(traj_temp))) {
        
        if (length(grep("PRESSURE", traj_temp[j, 1])) != 0) {
          skip_up_to_line <- j
        } 
      }
      
      column_widths <- c(rep(6, 8), 8, rep(9, 4))
      
      traj <- try(
        utils::read.fwf(
          file = file_i_path,
          skip = skip_up_to_line,
          widths = column_widths
        )
      )
      
      if (class(traj) != "try-error") {

        traj <-
          traj %>%
          dplyr::as_tibble() %>%
          .[, -c(1, 7:8)] %>%
          tidyr::drop_na()
          
        names(traj) <-
          c("receptor", "year", "month", "day", "hour", "hour.inc",
            "lat", "lon", "height", "pressure")
        
        traj <-
          traj %>%
          dplyr::mutate(hour.inc = as.integer(hour.inc)) %>%
          dplyr::mutate(year_full = ifelse(year < 50, year + 2000, year + 1900)) %>%
          tidyr::unite(col = date_str, year_full, month, day, sep = "-", remove = FALSE) %>%
          tidyr::unite(col = date_h_str, date_str, hour, sep = " ", remove = FALSE) %>%
          dplyr::mutate(date2 = lubridate::ymd_h(date_h_str)) %>%
          dplyr::select(-c(date_h_str, date_str, year_full)) %>%
          dplyr::mutate(date = date2[1])
        
        # Continuously bind data frames together to make
        # a large df from all trajectory files
        traj_df <- traj_df %>% dplyr::bind_rows(traj)
      }
    }
  }
 
  widths_1 <-
    readLines(con = file.path(output_folder, trajectory_file_list[1])) %>%
    nchar()

  if (any(widths_1 == 173)) {
    
    # Initialize empty data frame with 9 named columns
    traj_extra_df <-
      dplyr::tibble(
        theta = numeric(0),
        air_temp = numeric(0), 
        rainfall = numeric(0),
        mixdepth = numeric(0),
        rh = numeric(0),
        sp_humidity = numeric(0),
        h2o_mixrate = numeric(0),
        terr_msl = numeric(0),
        sun_flux = numeric(0)
      )
    
    extra_column_widths <- c(rep(6, 8), 8, rep(9, 13))
    
    for (file_i in trajectory_file_list) {
      
      file_i_path <- file.path(output_folder, file_i)

      traj_extra <- 
        utils::read.fwf(
          file = file_i_path,
          skip = skip_up_to_line,
          widths = extra_column_widths
        ) %>%
        dplyr::as_tibble() %>%
        .[, 14:22] %>%
        tidyr::drop_na()
      
      names(traj_extra) <- 
        c("theta", "air_temp", "rainfall", "mixdepth", "rh", "sp_humidity", 
          "h2o_mixrate", "terr_msl", "sun_flux")
      
      # Continuously bind data frames together to make
      # a large df from all trajectory files
      traj_extra_df <- traj_extra_df %>% dplyr::bind_rows(traj_extra)
    }
    
    traj_df <- traj_df %>% dplyr::bind_cols(traj_extra_df)
  }
  
  traj_df
}
