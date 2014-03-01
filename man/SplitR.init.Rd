\name{Initialize SplitR}
\alias{SplitR.init}
\title{
Initialize SplitR folders, get executables, get supporting files 
}
\description{
The function should be run initially to create the SplitR folder structure, to download executable files, and to generate the necessary supporting files for trajectory and dispersion model runs. The installation location is in the user's 'Documents' folder. The function requires a network connection to download a disk image for installation of executable binaries in the "~/Documents/SplitR/Exec" folder.
}
\usage{
SplitR.init(install_folders = TRUE)
}
\author{
Richard Iannone
}
\examples{
# Initialize SplitR and obtain the necessary
# files to run models and conduct analyses
SplitR.init()
}
