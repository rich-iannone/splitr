\name{Remove dispersion preset information}
\alias{dispersion.preset.delete}
\title{
Remove a preset for HYSPLIT dispersion model runs
}
\description{

}
\usage{
dispersion.preset.delete(read = NULL, numbers = NULL, interactive = TRUE)
}
\arguments{
  \item{read}{
the type of preset to delete. The three preset types are "emissions", "grids", and "species". Only a single type can be specified. If the argument for interactive remains TRUE (as in the default), the function will interactively prompt the user for the preset type if none is supplied. The preset must be supplied if the function is to be used in a non-interactive manner (futhermore, a single numeric value or numeric vector must be provided as the 'numbers' argument if 'interactive' is set to FALSE).
}
  \item{numbers}{

}
  \item{interactive}{

}
}
\author{
Richard Iannone
}
\examples{

}
