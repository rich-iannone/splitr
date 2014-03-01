\name{Conduct HYSPLIT Dispersion Runs}
\alias{hysplit.dispersion}
\title{
Conduct a single or multiple HYSPLIT dispersion runs.
}
\description{
The function executes single/multiple forward or backward HYSPLIT dispersions runs using either GDAS1 or NCAR/NCEP reanalysis meteorological data.  
}
\usage{
hysplit.dispersion(start_lat_deg = 49.289328,
                   start_long_deg = -123.117665,
                   start_height_m_AGL = 10,
                   simulation_duration_h = 24,
                   backward_running = FALSE,
                   met_type = "reanalysis",
                   vertical_motion_option = 0,
                   top_of_model_domain_m = 20000,
                   run_type = "day",
                   run_day = "2013-02-01",
                   run_range = c("2013-02-01", "2013-02-10"),
                   run_years = "2013",
                   daily_hours_to_start = "00",
                   emissions = c(1),
                   species = c(1),
                   grids = c(1),
                   path_met_files = "~/Documents/SplitR/Met/",
                   path_output_files = "~/Documents/SplitR/Output/",
                   path_wd = "~/Documents/SplitR/Working/",
                   path_executable = "~/Documents/SplitR/Exec/hycs_std")
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
  \item{backward_running}{
an option to select whether the dispersion runs should be running forward in time (the default) or in a backward running state.
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
should consist of a single daily hour in the format "HH", or, several daily hours in the format "c("HH", "HH", "HH", ...)".
}
  \item{emissions}{
the numbers corresponding to the stored emissions presets. These presets are specified using the function 'hysplit.dispersion.define("emissions")'.
}
  \item{species}{
the numbers corresponding to the stored species presets. These presets are specified using the function 'hysplit.dispersion.define("species")'.
}
  \item{grids}{
the numbers corresponding to the stored grid presets. These presets are specified using the function 'hysplit.dispersion.define("grids")'.
}
  \item{path_met_files}{
a full path should be provided for the location of the meteorological data files relevant to the model options chosen. By default, the location '~/Documents/SplitR/Met' is chosen.
}
  \item{path_output_files}{
a full path should be provided for a location that the trajectory output files will be written.  By default, the location '~/Documents/SplitR/Output' is chosen.
}
  \item{path_wd}{
a full path should be provided for the HYSPLIT working directory; the CONTROL file for each model run will be written to and read from this location.  By default, the location '~/Documents/SplitR/Working' is chosen.
}
  \item{path_executable}{
the full path and name of the HYSPLIT executable file for trajectory runs must be provided.  By default, the location '~/Documents/SplitR/Exec' is chosen.
}
}
\details{
}
\value{
}
\references{
}
\author{
Richard Iannone
}
\examples{
# Perform a dispersion run lasting 12 hours over two consecutive days, both starting at midnight
# Grid presets 1 and 2 will be used as sampling grids
# Presets for species and emissions default to the first in each of those sets
hysplit.dispersion(start_lat_deg = 49.289328, start_long_deg = -123.117665,
                   start_height_m_AGL = 15,
                   simulation_duration_h = 12, backward_running = FALSE, met_type = "reanalysis",
                   vertical_motion_option = 0, top_of_model_domain_m = 2000, grids = c(1,2),
                   run_type = "range", run_range = c("2012-02-01", "2012-02-03"),
                   daily_hours_to_start = "00",
                   path_met_files = "~/Documents/SplitR/Met/",
                   path_output_files = "~/Documents/SplitR/Output/",
                   path_wd = "~/Documents/SplitR/Working/",
                   path_executable = "~/Documents/SplitR/Exec/hycs_std")
}
