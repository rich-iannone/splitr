#--- R script for carrying out HYSPLIT trajectory model runs

# Define package requirements
require(lubridate)

# Identify file system paths (UNIX/Linux)
path_met_files <- "~/Downloads/Hysplit4-OSX/gdas1/"
path_output_files <- "~/Downloads/Hysplit4-OSX/output_trajectory/"
path_wd <- "~/Downloads/Hysplit4-OSX/working/"
path_executable <- "~/Downloads/Hysplit4-OSX/exec/hyts_std"

# Non-varying model parameters      
no_starting_locations <- 1
start_lat_deg <- 50.108
start_long_deg <- -122.942
start_height_m_ASL <- 2200.0
backtrajectory <- TRUE
simulation_duration_h <- 48
vertical_motion_option <- 0
top_of_model_domain_m <- 20000

# Define the type of run (day, range, years)
run_type <- "range"

# For all runs define the daily hours to start as a vector list
daily_hours_to_start <- c("03", "06", "09", "12", "15", "18", "21")

# For the 'day' run type, specify a single day in the format YYYY-MM-DD
run_day <- "2005-04-08"

# For the 'range' run type, define the beginning and ending period in a vector list
# containing two items of format YYYY-MM-DD
run_range <- c("2005-04-05", "2005-04-25")

# For the 'years' run type, specify a year (YYYY) or the range of years (YYYY-YYYY)
run_years <- "2004-2005"

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
                       as.POSIXct(paste(substr(run_years, 6, 9),"-12-31", sep = ''), 
                       origin = "1970-01-01", tz = "UTC"),
                       by = 86400)
} else {stop("A run type has not been selected")}

# Make loop with all run days
for (i in 1:length(list_run_days)) {

  # Define starting time parameters
  start_year_GMT <- substr(as.character(year(list_run_days[i])),3,4)
  #start_year_GMT <- "05"

  start_month_GMT <- formatC(as.numeric(month(list_run_days[i])),
                             width = 2, format = "d", flag = "0")
  #start_month_GMT <- "04"

  start_day_GMT <- formatC(as.numeric(day(list_run_days[i])),
                           width = 2, format = "d", flag = "0")
  #start_day_GMT <- "15"
# Close the day loop
}
