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
  
  if (any(c("mac", "unix") %in% system_type)) {
    
    # Write start year, month, day, hour to 'CONTROL'
    cat(start_year_GMT, " ", 
        start_month_GMT, " ",
        start_day_GMT, " ",
        start_hour_GMT, "\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = FALSE
    )
    
    # Write number of starting locations to 'CONTROL'
    cat("1\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
    
    # Write starting latitude, longitude, height AGL to 'CONTROL'
    cat(lat, " ",
        lon, " ",
        height, "\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
    
    # Write direction and number of simulation hours to 'CONTROL'
    cat(ifelse(direction == "backward", "-", ""),
        duration, "\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
    
    # Write vertical motion option to 'CONTROL'
    cat(vert_motion, "\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
    
    # Write top of model domain in meters to
    # 'CONTROL'
    cat(model_height, "\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
    
    # Write number of met files used to 'CONTROL'
    cat(length(met_files), "\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
    
    # Write met file paths to 'CONTROL'
    for (i in 1:length(met_files)) {
      cat(met_dir, "/\n", met_files[i], "\n",
          file = paste0(exec_dir, "/CONTROL"),
          sep = "", append = TRUE
      )
    }
    
    # Write path for trajectory output files to 'CONTROL'
    cat(exec_dir, "/\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
    
    # Write name of output filename to 'CONTROL'
    cat(output_filename, "\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
  }
  
  if (system_type == "win") {
    
    # Write start year, month, day, hour to 'CONTROL'
    cat(start_year_GMT, " ", 
        start_month_GMT, " ",
        start_day_GMT, " ",
        start_hour_GMT, "\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = FALSE
    )
    
    # Write number of starting locations to 'CONTROL'
    cat("1\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
    
    # Write starting latitude, longitude, height AGL to 'CONTROL'
    cat(lat, " ", 
        lon, " ", 
        height, "\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
    
    # Write direction and number of simulation hours to 'CONTROL'
    cat(ifelse(direction == "backward", "-", ""),
        duration, "\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
    
    # Write vertical motion option to 'CONTROL'
    cat(vert_motion, "\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
    
    # Write top of model domain in meters to
    # 'CONTROL'
    cat(model_height, "\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
    
    # Write number of met files used to 'CONTROL'
    cat(length(met_files), "\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
    
    # Write met file paths to 'CONTROL'
    for (i in 1:length(met_files)) {
      cat(met_dir, "/\n", met_files[i], "\n",
          file = paste0(exec_dir, "/CONTROL"),
          sep = "", append = TRUE
      )
    }
    
    # Write path for trajectory output files to
    # 'CONTROL'
    cat(exec_dir, "/\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
    
    # Write name of output filename to 'CONTROL'
    cat(output_filename, "\n",
        file = paste0(exec_dir, "/CONTROL"),
        sep = "", append = TRUE
    )
  }
}
