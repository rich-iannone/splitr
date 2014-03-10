\name{Conduct HYSPLIT Runs}
\alias{hysplit.trajectory}
\title{
Conduct a single or multiple HYSPLIT trajectory runs.
}
\description{
The function executes single/multiple forward or backward HYSPLIT trajectory runs using either GDAS1 or NCAR/NCEP reanalysis meteorological data.  
}
\usage{
hysplit.trajectory(start_lat_deg,
                   start_long_deg,
                   start_height_m_AGL,
                   simulation_duration_h = 24,
                   backtrajectory = FALSE,
                   met_type,
                   vertical_motion_option = 0,
                   top_of_model_domain_m = 20000,
                   run_type,
                   run_day = NULL,
                   run_range = NULL,
                   run_years = NULL,
                   daily_hours_to_start,
                   path_met_files = "~/Documents/SplitR/Met/",
                   path_output_files = "~/Documents/SplitR/Output/",
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
an option to select meteorological data files. The options are "gdas1" (Global Data Assimilation System 1-degree resolution data) and 'reanalysis' (NCAR/NCEP global reanalysis data). 
}
  \item{vertical_motion_option}{
a numbered option to select the method used to simulation vertical motion. The methods are: (0) input model data, (1) isobaric, (2) isentropic, (3) constant density, (4) isosigma, (5) from divergence, (6) remap MSL to AGL, (7) average data, and (8) damped magnitude. 
}
  \item{run_type}{
used to select whether models should be run for a single day ("day"), for one or more years ("years"), or within a specified date range ("range").
}
  \item{run_day}{
used when 'run_type' of 'day' is selected. The date format should be provided here as "YYYY-MM-DD".
}
  \item{run_range}{
used when 'run_type' of 'range' is selected. The date format should be provided here as "c("YYYY-MM-DD", "YYYY-MM-DD")".
}
  \item{run_years}{
used when 'run_type' of 'years' is selected. The format should either be a single year ("YYYY") or a range of years ("YYYY-YYYY").
}
  \item{daily_hours_to_start}{
should consist of a single daily hour in the format "HH", or, several daily hours in the format "c("HH", "HH", ...)".
}
  \item{path_met_files}{
a full path should be provided for the location of the meteorological data files relevant to the model options chosen.
}
  \item{path_output_files}{
a full path should be provided for a location that the trajectory output files will be written.
}
  \item{path_wd}{
a full path should be provided for the HYSPLIT working directory; the CONTROL file for each model run will be written to and read from this location.
}
  \item{path_executable}{
the full path and name of the HYSPLIT executable file for trajectory runs must be provided.
}
}
\author{
Richard Iannone
}
\examples{
# test run type of 'years' with forward trajectory using NCEP/NCAR reanalaysis data
hysplit.trajectory(start_lat_deg = 50.108,
                   start_long_deg = -122.942,
                   start_height_m_AGL = 200.0,
                   simulation_duration_h = 96,
                   backtrajectory = FALSE,
                   met_type = "reanalysis",
                   vertical_motion_option = 0,
                   top_of_model_domain_m = 20000,
                   run_type = "years",
                   run_years = "2004",
                   daily_hours_to_start = c("03", "06", "09", "12", "15", "18", "21"),
                   path_met_files = "~/HYSPLIT4/met/",
                   path_output_files = "~/HYSPLIT4/output_trajectory/",
                   path_wd = "~/HYSPLIT4/working/",
                   path_executable = "~/HYSPLIT4/exec/hyts_std") 
}
