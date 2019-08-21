#' @noRd
write_disp_control_file <- function(start_day,
                                    start_year_GMT,
                                    start_month_GMT,
                                    start_day_GMT,
                                    start_hour,
                                    lat,
                                    lon,
                                    height,
                                    direction,
                                    duration,
                                    vert_motion,
                                    model_height,
                                    met_files,
                                    species,
                                    output_filename,
                                    system_type,
                                    met_dir,
                                    exec_dir) {

  start_block <- 
    paste0(
      start_year_GMT, " ", start_month_GMT, " ",
      start_day_GMT, " ", start_hour, "\n",
      "1\n",
      lat, " ", lon, " ", height, "\n",
      ifelse(direction == "backward", "-", ""), duration, "\n",
      vert_motion, "\n",
      model_height, "\n",
      length(met_files), "\n",
      paste0(met_dir, "/\n", met_files, collapse = "\n"), "\n"
    )
  
  i <- 1
  
  release_duration <- as.numeric(species$release_end - species$release_start)
  
  emission_block <- 
    paste0(
      "1\n",
      substr(species$name, 1, 4), "\n",
      species$rate, "\n",
      release_duration, "\n",
      species$release_start %>% substr(3, 10) %>% tidy_gsub("-", " "), " ",
      formatC(lubridate::hour(species$release_start), width = 2, flag = "0"), " ", "00", "\n"
    )
  
  end_year_GMT <- to_short_year(species$release_end)
  end_month_GMT <- to_short_month(species$release_end)
  end_day_GMT <- to_short_day(species$release_end)

  grid_block <-
    paste0(
      "1", "\n",
      "0.0 0.0", "\n",
      "0.01 0.01", "\n",
      "180 360", "\n",
      "./", "\n",
      "output.bin", "\n",
      "1", "\n",
      "0.0", "\n",
      start_year_GMT, " ", start_month_GMT, " ",
      start_day_GMT, " ", start_hour, " 00", "\n",
      end_year_GMT, " ", end_month_GMT, " ",
      end_day_GMT, " ", "23 00", " \n",
      "00 01 00", "\n"
    )

  species_block <-
    paste0(
      "1", "\n",
      species$pdiam, " ",
      species$density, " ",
      species$shape_factor, "\n",
      species$ddep_vel, " ",
      species$ddep_mw, " ",
      species$ddep_a_ratio, " ",
      species$ddep_d_ratio, " ",
      species$ddep_hl_coeff, "\n",
      species$wdep_hl_coeff, " ",
      species$wdep_in_cloud, " ",
      species$wdep_below_cloud, "\n",
      species$rad_decay, "\n",
      species$resuspension, "\n"
    )
  
  paste0(
    start_block,
    emission_block,
    grid_block,
    species_block
  ) %>%
    cat(file = file.path(exec_dir, "CONTROL"), sep = "", append = FALSE)
}