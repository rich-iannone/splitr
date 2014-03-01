\name{Get requested preset information}
\alias{dispersion.preset.get}
\title{
Obtain requested presets in a format suitable for inclusion in a CONTROL file
}
\description{

}
\usage{
dispersion.preset.get(read, numbers)
}
\arguments{
  \item{read}{
the type of preset to get. The three preset types are "emissions", "grids", and "species". Only a single type can be specified.
}
  \item{numbers}{
the values corresponding to the presets of a specified type (provided as the 'read' character argument) to be retrieved. The argument is to be provided as a single numeric value or numeric vector. The numbers correspond to the order in which the presets appear in each of the 'emissions', 'grids', or 'species' plaintext files.
}
}
\author{
Richard Iannone
}
\examples{
# The hysplit.dispersion() function obtains presets for
# "emissions", "grids", and "species" while assembling
# a CONTROL file
dispersion.preset.get(read = "species", numbers = species)
}
