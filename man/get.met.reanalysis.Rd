\name{Get reanalysis meteorology data files}
\alias{get.met.reanalysis}
\title{
Get reanalysis meteorology data files from the NOAA FTP server 
}
\description{
This function downloads reanalysis meteorology data files from the NOAA FTP server and saves them, by default, to "~/Documents/SplitR/Met". Files can be downloaded either by specifying a list of filenames (in the form of "RP[YYYY][MM].gbl") or through bulk download of a year of files. 
}
\usage{
get.met.reanalysis(files = NULL,
                   years = NULL,
                   path_met_files = "~/Documents/SplitR/Met/"){ 
}
\author{
Richard Iannone
}
\examples{
# Download 15 years of reanalysis data files
get.met.reanalysis(years = seq(1998, 2012, 1))
}
