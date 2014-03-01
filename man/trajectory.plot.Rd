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
}
\author{
Richard Iannone
}
\examples{
# Plot trajectory data in the traj.df data frame using defaults
trajectory.plot()
}
