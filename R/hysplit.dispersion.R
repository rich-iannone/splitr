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
  
  # Make a vector list of run days in POSIXct format
  if (run_type == "day") {
    list_run_days <- as.POSIXct(run_day, origin = "1970-01-01", tz = "UTC")
  } else if (run_type == "range") {
    list_run_days <- seq(as.POSIXct(run_range[1], origin = "1970-01-01", tz = "UTC"),
                         as.POSIXct(run_range[2], origin = "1970-01-01", tz = "UTC"),
                         by = 86400)
  } else if (run_type == "years") {
    list_run_days <- seq(as.POSIXct(paste(substr(run_years, 1, 4),"-01-01", sep = ''), 
                                    origin = "1970-01-01", tz = "UTC"),
                         as.POSIXct(ifelse(run_years_single_range == "single",
                                           paste(substr(run_years, 1, 4),"-12-31",sep = ''),
                                           paste(substr(run_years, 6, 9),"-12-31",sep = '')), 
                                    origin = "1970-01-01", tz = "UTC"),
                         by = 86400)
  } else {stop("A run type has not been selected")}
  
}

# Make loop with all run days
for (i in 1:length(list_run_days)) {

  # Define starting time parameters
  start_year_GMT <- substr(as.character(year(list_run_days[i])),3,4)
  
  
  start_month_GMT <- formatC(as.numeric(month(list_run_days[i])),
                             width = 2, format = "d", flag = "0")
  
  
  start_day_GMT <- formatC(as.numeric(day(list_run_days[i])),
                           width = 2, format = "d", flag = "0")
  
  
  # Make nested loop with daily beginning hours
  for (j in daily_hours_to_start) {    
    
    start_hour_GMT <- j
   
    #--- Determine which met files are required for this run
    
    # Determine the start time of the model run
    start_time_GMT <- ymd_hms(paste(ifelse(start_year_GMT > 50,
                                           paste("19", start_year_GMT, sep = ''),
                                           start_year_GMT), "-",
                                    start_month_GMT, "-",
                                    start_day_GMT, " ",
                                    start_hour_GMT, ":00:00",
                                    sep = ''))
    # Determine the end time of the model run
    end_time_GMT <- as.POSIXct(ifelse(backtrajectory == TRUE, 
                                      start_time_GMT - (simulation_duration_h * 3600),
                                      start_time_GMT + (simulation_duration_h * 3600)),
                               origin = "1970-01-01", tz = "UTC")
    
    # Determine whether the start year is a leap year
    leap_year <- leap_year(ymd(paste(start_year_GMT, "-",
                                     start_month_GMT, "-",
                                     start_day_GMT, sep = '')))
    
    # Determine whether the beginning and end of the current run
    # crosses over a calendar year
    number_of_calendar_years <- ifelse(year(start_time_GMT) == year(end_time_GMT), 1, 2)
    
    # Determine whether the beginning and end of the current run
    # crosses over a calendar month
    number_of_calendar_months <- ifelse(month(start_time_GMT) == month(end_time_GMT), 1, 2)
    
    #--- Divide different requirements for met files into different cases
    
    # Set the different cases to FALSE by default
    case_within_month <- FALSE
    case_over_year <- FALSE
    case_over_month <- FALSE
    
    # Determine which of the three cases is true
    if (number_of_calendar_years == 1 & number_of_calendar_months == 1) {
      case_within_month <- TRUE
    } else if (number_of_calendar_years > 1) {
      case_over_year <- TRUE
    } else if (number_of_calendar_months > 1) {
      case_over_month <- TRUE
    } else { NULL }
    
    #--- Get vector lists of met files applicable to run from GDAS 1-degree dataset
    
    # Trap leap-year condition of missing .w5 met file for February in a '0' list value
    if (case_within_month == TRUE &
          met_type == "gdas1") met <- 
      c(paste("gdas1.",
              substr(tolower(format(start_time_GMT, "%B")), 1, 3),
              substr(year(start_time_GMT), 3, 4), ".w1", sep = ''),                                      
        paste("gdas1.",
              substr(tolower(format(start_time_GMT, "%B")), 1, 3),
              substr(year(start_time_GMT), 3, 4), ".w2", sep = ''),
        paste("gdas1.",
              substr(tolower(format(start_time_GMT, "%B")), 1, 3),
              substr(year(start_time_GMT), 3, 4), ".w3", sep = ''),
        paste("gdas1.",
              substr(tolower(format(start_time_GMT, "%B")), 1, 3),
              substr(year(start_time_GMT), 3, 4), ".w4", sep = ''),
        ifelse(month(start_time_GMT) == 2 &
                 leap_year == TRUE, 0,
               paste("gdas1.",
                     substr(tolower(format(start_time_GMT, "%B")), 1, 3),
                     substr(year(start_time_GMT), 3, 4), ".w5", sep = '')))
    
    if (case_over_year == TRUE &
          met_type == "gdas1") met <- 
      c(paste("gdas1.dec",
              substr(year(end_time_GMT), 3, 4), ".w3", sep = ''),                                      
        paste("gdas1.dec",
              substr(year(end_time_GMT), 3, 4), ".w4", sep = ''),
        paste("gdas1.dec",
              substr(year(end_time_GMT), 3, 4), ".w5", sep = ''),
        paste("gdas1.jan",
              substr(year(start_time_GMT), 3, 4), ".w1", sep = ''),
        paste("gdas1.jan",
              substr(year(start_time_GMT), 3, 4), ".w2", sep = ''),
        paste("gdas1.jan",
              substr(year(start_time_GMT), 3, 4), ".w3", sep = ''))
  }
  
  
}


