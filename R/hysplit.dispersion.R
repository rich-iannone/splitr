hysplit.dispersion <- function(use_default_config = TRUE,
                               start_lat_deg,
                               start_long_deg,
                               start_height_m_ASL,
                               simulation_duration_h = 24,
                               backward_running = FALSE,
                               met_type,
                               vertical_motion_option = 0,
                               top_of_model_domain_m = 20000,
                               run_type,
                               run_day = NULL,
                               run_range = NULL,
                               run_years = NULL,
                               daily_hours_to_start,
                               path_met_files,
                               path_output_files,
                               path_wd,
                               path_executable){ 
  
  # Define package requirements
  require(lubridate)
  
  # Set number of starting locations to 1 for this function
  no_starting_locations <- 1
  
  # Determine whether the run_years input is a single year or a range
  
  if(exists("run_years")) run_years_single_range <-
    ifelse(nchar(run_years) == 4, "single", "range") 
  
}

