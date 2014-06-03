\name{Create config file for dispersion runs}
\alias{hysplit.dispersion.config}
\title{
Create/modify HYSPLIT dispersion config file
}
\description{
This function creates a default SETUP.CFG configuration file and places it in the user-specified 'working' folder. This file must be located in the working directory before any HYSPLIT dispersion models can be run.
}
\usage{
hysplit.dispersion.config(path_wd)
}
\arguments{
  \item{path_wd}{
a full path should be provided for the HYSPLIT working directory; a default SETUP.CFG file be placed in that folder.
}
}
\author{
Richard Iannone
}
\examples{
# Create a default SETUP.CFG file for dispersion runs
hysplit.dispersion.config(path_wd = "C://hysplit//working//")
}
