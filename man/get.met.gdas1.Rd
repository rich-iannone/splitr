\name{Get GDAS1 meteorology data files}
\alias{get.met.gdas1}
\title{
Get GDAS 1-degree meteorology data files from the NOAA FTP server 
}
\description{
This function downloads GDAS1 meteorology data files from the NOAA FTP server and saves them, by default, to "~/Documents/SplitR/Met". Files can be downloaded either by specifying a list of filenames, or through bulk download of a month or year of files. 
}
\usage{
get.met.gdas1(files = NULL,
              years = NULL,
              months = NULL,
              path_met_files = "~/Documents/SplitR/Met/")
}
\arguments{
  \item{files}{
a vector list of filenames to be downloaded.
}
  \item{years}{
a vector list of years for which a bulk download should be initiated.
}
  \item{months}{
a vector list of months for which a bulk download should be initiated. Should be provided in the form "MM-YYYY".
}
  \item{path_met_files}{
the path to which the downloaded files will be stored.
}
}
\author{
Richard Iannone
}
\examples{
# Download a year of GDAS1 data files
get.met.gdas1(years = 2012)
}
