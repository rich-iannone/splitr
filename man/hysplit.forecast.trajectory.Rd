\name{Conduct HYSPLIT Forecast Trajectory Runs}
\alias{hysplit.forecast.trajectory}
\title{
Conduct a HYSPLIT forecast trajectory run.
}
\description{
The function executes a HYSPLIT forecast trajectory run using NAM forecast meteorological data (CONUS, 12 km, 3 hrly, pressure levels, 48 h).
}
\usage{
hysplit.forecast.trajectory(start_lat_deg,
                            start_long_deg,
                            start_height_m_AGL,
                            simulation_duration_h = 24,
                            backtrajectory = FALSE,
                            met_type = "nam",
                            forecast_cycle = "latest",
                            vertical_motion_option = 0,
                            top_of_model_domain_m = 20000,
                            run_start = NULL,
                            daily_hours_to_start,
                            path_met_files = "~/Documents/SplitR/Met/",
                            path_output_files = paste(getwd(), "/", sep = ''),
                            path_wd = "~/Documents/SplitR/Working/",
                            path_executable = "~/Documents/SplitR/Exec/")
                            
}
\arguments{
  \item{start_lat_deg}{
the starting latitude (in decimal degrees) for the model run(s)
}
  \item{start_long_deg}{
the starting longitude (in decimal degrees) for the model run(s)
}
  \item{start_height_m_AGL}{
the starting height (in meters above ground level) for the model run(s)
}
  \item{simulation_duration_h}{
the duration of each model run (either forward or backward) in hours
}
  \item{backtrajectory}{
an option to select whether to conduct forward trajectory model runs or to generate backtrajectories
}
  \item{met_type}{
an option to select meteorological data files. The only option currently is for "nam" (North American Mesoscale) data. 
}
  \item{vertical_motion_option}{
a numbered option to select the method used to simulation vertical motion. The methods are: (0) input model data, (1) isobaric, (2) isentropic, (3) constant density, (4) isosigma, (5) from divergence, (6) remap MSL to AGL, (7) average data, and (8) damped magnitude. 
}
  \item{daily_hours_to_start}{
should consist of a single daily hour in the format "HH", or, several daily hours in the format "c("HH", "HH", ...)".
}
  \item{path_met_files}{
a full path should be provided for the location of the meteorological data files relevant to the model options chosen. By default, the location '~/Documents/SplitR/Met' is chosen.
}
  \item{path_output_files}{
a full path should be provided for a location that the trajectory output files will be written. By default, the location '~/Documents/SplitR/Output' is chosen.
}
  \item{path_wd}{
a full path should be provided for the HYSPLIT working directory; the CONTROL file for each model run will be written to and read from this location. By default, the location '~/Documents/SplitR/Working' is chosen.
}
  \item{path_executable}{
the full path and name of the HYSPLIT executable file for trajectory runs must be provided. By default, the location '~/Documents/SplitR/Exec' is chosen.
}
}
\author{
Richard Iannone
}
\examples{
# Run a forward trajectory model with the latest NAM forecast data
hysplit.trajectory(start_lat_deg,
                   start_long_deg,
                   start_height_m_AGL,
                   simulation_duration_h = 24,
                   backtrajectory = FALSE,
                   met_type = "nam",
                   forecast_cycle = "latest",
                   vertical_motion_option = 0,
                   top_of_model_domain_m = 20000,
                   run_start = NULL,
                   daily_hours_to_start,
                   path_met_files = "~/Documents/SplitR/Met/",
                   path_output_files = paste(getwd(), "/", sep = ''),
                   path_wd = "~/Documents/SplitR/Working/",
                   path_executable = "~/Documents/SplitR/Exec/")
}
