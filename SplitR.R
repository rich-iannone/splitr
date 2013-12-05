
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
}
