\name{Perform cluster analysis on HYSPLIT trajectories}
\alias{trajectory.cluster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perform cluster analysis HYSPLIT trajectory output files already processed into a data frame.
}
\description{
The function takes a data frame that was produced from HYSPLIT output files using the 'trajectory.read' function. The function will amend the data frame with cluster information, where the  number of clusters is specified. Furthermore, the cluster trajectories can be plotted onto a map.  
}
\usage{
trajectory.cluster(traj, method = "Euclid", n.cluster = 5,
                   plot = TRUE, type = "default", cols = "Set1",
                   split.after = FALSE, map.fill = TRUE,
                   map.cols = "grey30", map.alpha = 0.4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{
a data frame object that was produced from HYSPLIT output files using the 'trajectory.read' function.
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
# Perform a cluster analysis of backtrajectory runs, choosing 4 clusters
# and producing a map
traj.cluster <- trajectory.cluster(traj = traj.df, method = "Euclid",
                                   n.cluster = 4, plot = TRUE, type = "default",
                                   cols = "Set1", split.after = FALSE,
                                   map.fill = TRUE, map.cols = "grey50",
                                   map.alpha = 0.4)
}
