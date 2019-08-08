#' @noRd
write_traj_control_file <- function(start_year_GMT,
                                    start_month_GMT,
                                    start_day_GMT,
                                    start_hour_GMT,
                                    lat,
                                    lon,
                                    height,
                                    direction,
                                    duration,
                                    vert_motion,
                                    model_height,
                                    met_files,
                                    output_filename,
                                    system_type,
                                    met_dir,
                                    exec_dir) {
  
  paste0(
    start_year_GMT, " ", start_month_GMT, " ",
    start_day_GMT, " ", start_hour_GMT, "\n",
    "1\n",
    lat, " ", lon, " ", height, "\n",
    ifelse(direction == "backward", "-", ""), duration, "\n",
    vert_motion, "\n",
    model_height, "\n",
    length(met_files), "\n",
    paste0(met_dir, "/\n", met_files, collapse = "\n"), "\n",
    exec_dir, "/\n",
    output_filename, "\n"
  ) %>%
    cat(file = file.path(exec_dir, "CONTROL"), sep = "", append = FALSE)
}
