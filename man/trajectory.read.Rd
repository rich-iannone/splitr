\name{Read HYSPLIT trajectory output files}
\alias{trajectory.read}
\title{
Read HYSPLIT trajectory output files into data frame for further analysis.
}
\description{
The function takes HYSPLIT trajectory output files in a specified output directory and processes all files into a data frame object. The resulting data frame is useful for plotting of trajectories and for conducting statistical analyses such as clustering.  
}
\usage{
trajectory.read(path_trajectory_files)
}
\arguments{
  \item{path_trajectory_files}{
a full path should be provided for a location that contains all relevant trajectory output files.
}
}
\author{
Richard Iannone
}
\examples{
# Process all trajectory output files in the specified output directory
trajectory.df <- trajectory.read(path_trajectory_files =
                                 "~/HYSPLIT4/out/trajectory")
}
