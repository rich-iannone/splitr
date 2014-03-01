\name{Get executable binaries for HYSPLIT}
\alias{SplitR.get.exec}
\title{
Get executable binaries for HYSPLIT.
}
\description{
Helper function that automatically retrieves the HYSPLIT package from NOAA and moves binary executables to the /SplitR/Exec folder. This function is used in conjunction with the 'SplitR.init' function, which initializes folders and information files. Notably, this installation currently works only on the OS X platform.
}
\usage{
SplitR.get.exec()
}
\author{
Richard Iannone
}
\examples{
# Download the HYSPLIT executables and place in the /SplitR/Exec folder
SplitR.get.exec()
}
