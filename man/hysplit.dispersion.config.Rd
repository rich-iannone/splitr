\name{Create config file for dispersion runs}
\alias{hysplit.dispersion.config}
\title{
Create/modify HYSPLIT dispersion config file
}
\description{

}
\usage{
hysplit.dispersion.config(create_default_config = TRUE,
                          interactive_mode = FALSE)
}
\arguments{
  \item{create_default_config}{
creates a default SETUP.CFG configuration file and places it in the "/SplitR/Working" folder. This is done in a non-interactive manner so any value for the 'interactive_mode' argument will be disregarded.
}
  \item{interactive_mode}{
if set to TRUE, the function allows for the modification of SETUP.CFG configuration settings in an interactive manner (the 'create_default_config' argument must be set to FALSE). The SETUP.CFG file must already exist in the "/SplitR/Working" folder for the function to be called in this manner.
}
}
\details{
}
\value{
}
\references{
}
\author{
Richard Iannone
}
\examples{

}
