\name{Perform cluster analysis on HYSPLIT trajectories}
\alias{trajectory.cluster}
\title{
Perform cluster analysis HYSPLIT trajectory output files already processed into a data frame.
}
\description{
The function takes a data frame that was produced from HYSPLIT backtrajectory output files using the 'trajectory.read' function. The function will amend the data frame with cluster information, where the  number of clusters is specified. Furthermore, the cluster trajectories can be plotted onto a map.  
}
\usage{
trajectory.cluster(traj_df, method = "Euclid", n.cluster = 5,
                   plot = TRUE, type = "default", cols = "Set1",
                   split.after = FALSE, map.fill = TRUE,
                   map.cols = "grey30", map.alpha = 0.4)
}
\arguments{
  \item{traj_df}{
a data frame object that was produced from HYSPLIT output files using the 'trajectory.read' function.
}d
  \item{method}{
the method of either "Euclid" or "Angle" that is used to calculate the distance matrix for the back trajectories.
}
  \item{n.cluster}{
an integer indicating the number of clusters to calculate.
}
  \item{plot}{
a boolean that determines whether a cluster trajectory plot will be produced.
}
  \item{type}{
determines how the data are conditioned before plotting. The setting of "default" produces a single plot with all of the input data. The option “season" will produce four plots: one for each season. The option “year” will produce a plot for each year of data. The option “weekday” will produce a plot with weekday data only. Cluster calculations will be carried out for cut data in each level of "type".
}
  \item{cols}{
determines the colours to be used for plotting. Options include “default”, “heat”, “increment”, “jet”. There are also RColorBrewer colour options. A vector list of colour names can be supplied, where the length of list must match the integer supplied in the 'n.cluster' argument.
}
  \item{split.after}{
a boolean that allows for trajectories being calculated for each level of type independently or extracted after the cluster calculations have been applied to the whole data set.
}
  \item{map.fill}{
a boolean that determines whether the map is to be filled in with a colour.
}
  \item{map.cols}{
provided that map.fill is TRUE, the fill colour can be specified using R built in colour names.
}
  \item{map.alpha}{
a value in the range of 0-1 that controls transpency of the map fill. Values closer to 1 are increasingly opaque whereas setting map.alpha to 0 makes the map colour fully transparent.
}
}
\author{
Richard Iannone
}
\examples{
# Perform a cluster analysis of backtrajectory runs, choosing 4 clusters
# and producing a map
traj.cluster <- trajectory.cluster(traj = traj.df, method = "Euclid",
                                   n.cluster = 4, plot = TRUE, type = "default",
                                   cols = "Set1", split.after = FALSE,
                                   map.fill = TRUE, map.cols = "grey50",
                                   map.alpha = 0.4)
}
