\name{Add dispersion preset information}
\alias{dispersion.preset.add}
\title{
Add a preset to for HYSPLIT dispersion runs
}
\description{

}
\usage{
dispersion.preset.add(type)
}
\arguments{
  \item{type}{
the type of preset to add. The three types are "emissions", "grids", and "species". Only a single type can be specified. Once specified, the function interactively asks for input on the parameters specific to each preset.
}
}
\author{
Richard Iannone
}
\examples{
# Add a preset for a 'emissions' input
dispersion.preset.add("emissions")

# Add a preset for a 'grids' input
dispersion.preset.add("grids")

# Add a preset for a 'species' input
dispersion.preset.add("species")
}
