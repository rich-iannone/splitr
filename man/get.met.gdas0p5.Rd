\name{Get GDAS0.5 meteorology data files}
\alias{get.met.gdas0p5}
\title{
Get GDAS 0.5-degree meteorology data files from NOAA 
}
\description{
This function downloads GDAS0.5 meteorology data files from the NOAA FTP server and saves them, by default, to "~/Documents/SplitR/Met". Files can be downloaded either by specifying a list of filenames  (in the form of "[YYYY][MM][DD]_gdas0p5"), specifying a list of days, or through bulk download of a month or year of files. 
}
\usage{
get.met.gdas0p5(files = NULL,
                years = NULL,
                months = NULL,
                days = NULL,
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
  \item{months}{
a vector list of days for which a bulk download should be initiated. Should be provided in the form "MM-DD-YYYY".
}
  \item{path_met_files}{
the path to which the downloaded files will be stored.
}
}
\author{
Richard Iannone
}
\examples{
# Download a year of GDAS0.5 data files
# WARNING: this is a large amount of data
get.met.gdas0p5(years = 2012)
}
