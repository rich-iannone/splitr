\name{Plot a trajectory}
\alias{trajectory.plot}
\title{
Plot a HYSPLIT trajectory.
}
\description{
Takes output data (in the form of a data frame) from one or more HYSPLIT trajectory runs (either forward or backward) and plots the trajectories. The default behaviour is to plot onto a map.
}
\usage{
trajectory.plot(traj.df = "traj.df",
                lon = "lon",
                lat = "lat",
                pollutant = "height", 
                type = "default",
                smooth = FALSE,
                statistic = "mean",
                percentile = 90, 
                map = TRUE,
                lon.inc = 1,
                lat.inc = 1,
                min.bin = 1,
                group = NA, 
                map.fill = TRUE,
                map.res = "default",
                map.cols = "grey40", 
                map.alpha = 0.4,
                plot.type = "l")
}
\arguments{
  \item{traj.df}{
a trajectory data frame object that is automatically assigned with object name 'traj.df' by the 'trajectory.read' function
}
  \item{lon}{
the name of the longitude column in the 'traj.df' data frame. The default value of 'lon' is created by default when using the 'trajectory.read' function.
}
  \item{lat}{
the name of the latitude column in the 'traj.df' data frame. The default value of 'lat' is created by default when using the 'trajectory.read' function.
}
  \item{pollutant}{
the name of the column that represents the height in the 'traj.df' data frame. The default value of 'height' is used in this case.
}
  \item{type}{
the type of plot that is to be created. By default, the value used is 'default'.
}
  \item{smooth}{
determines whether the trajectory surface should be smoothed. The default is set to FALSE (no smoothing).
}
  \item{statistic}{
by default the plot will produce the mean trajectories.
}
  \item{percentile}{
represents the percentile concentration of the pollutant against which the all trajectories are compared.
}
  \item{map}{
determines whether a map of trajectories should be generated
}
}
\author{
Richard Iannone
}
\examples{
# Plot trajectory data in the traj.df data frame using defaults
trajectory.plot()
}
