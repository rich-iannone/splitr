\name{Read HYSPLIT trajectory output files}
\alias{trajectory.read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read HYSPLIT trajectory output files into data frame for further analysis.
}
\description{
The function takes HYSPLIT trajectory output files in a specified output directory and processes all files into a data frame object. The resulting data frame is useful for plotting of trajectories and for conducting statistical analyses such as clustering.  
}
\usage{
trajectory.read(path_trajectory_files)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path_trajectory_files}{
a full path should be provided for a location that contains all relevant trajectory output files.
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
# Process all trajectory output files in the specified output directory
trajectory.df <- trajectory.read(path_trajectory_files = "~/HYSPLIT4/out/trajectory")
}
