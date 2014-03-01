\name{Remove dispersion preset information}
\alias{dispersion.preset.delete}
\title{
Remove a preset for HYSPLIT dispersion model runs
}
\description{

}
\usage{
dispersion.preset.delete(read = NULL,
                         numbers = NULL,
                         interactive = TRUE)
}
\arguments{
  \item{read}{
the type of preset to delete. The three preset types are "emissions", "grids", and "species". Only a single type can be specified. If the argument for interactive remains TRUE (as in the default), the function will interactively prompt the user for the preset type if none is supplied. The preset must be supplied if the function is to be used in a non-interactive manner (futhermore, a single numeric value or numeric vector must be provided as the 'numbers' argument if 'interactive' is set to FALSE).
}
  \item{numbers}{
the values corresponding to the presets of a specified type (provided as the 'read' character argument) to be deleted. The argument is to be provided as a single numeric value or numeric vector. Any values provided for this argument will be ignored if 'interactive' is set to TRUE (the default). The numbers correspond to the order in which the presets appear in each of the 'emissions', 'grids', or 'species' plaintext files.
}
  \item{interactive}{
determines whether the function should run in an interactive mode, prompting the for input on which presets to delete. One-line summaries of presets for a specific type are presented to guide the user on which items are available for deletion. If the interactive mode is selected (it is the default mode), no values for the 'numbers' need to be supplied.
}
}
\author{
Richard Iannone
}
\examples{

}
