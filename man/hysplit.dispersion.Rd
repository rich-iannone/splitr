\name{Conduct HYSPLIT Dispersion Runs}
\alias{hysplit.dispersion}
%- Also NEED an '\alias' for EACH other topic documented here.
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
  \item{}{

}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Richard Iannone
}
\examples{

}
