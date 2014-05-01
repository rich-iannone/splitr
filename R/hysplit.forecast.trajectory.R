hysplit.forecast.trajectory <- function(start_lat_deg,
                                        start_long_deg,
                                        start_height_m_AGL,
                                        simulation_duration_h = 24,
                                        backtrajectory = FALSE,
                                        met_type,
                                        vertical_motion_option = 0,
                                        top_of_model_domain_m = 20000,
                                        run_start = NULL,
                                        daily_hours_to_start,
                                        path_met_files = "~/Documents/SplitR/Met/",
                                        path_output_files = paste(getwd(), "/", sep = ''),
                                        path_wd = "~/Documents/SplitR/Working/",
                                        path_executable = "~/Documents/SplitR/Exec/") {
  
  # Define package requirements
  require(lubridate)
  

  # Reset path for output files to 'Output' folder if not in a project
  if (grepl("Documents/SplitR/Projects/.*_[0-9][0-9][0-9][0-9]-.*",
            path_output_files) == FALSE){
    path_output_files <- "~/Documents/SplitR/Output/"
  }
  
  # Close the function
}
