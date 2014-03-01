\name{Add dispersion preset information}
\alias{dispersion.preset.add}
\title{
Add a preset to for HYSPLIT dispersion runs
}
\description{
The function allows for the setting of presets specific to dispersion runs. Presets are effectively blocks of parameters that are stored on the disk. Having presets is useful because these groups of parameters can be called in different dispersion runs (and across projects) without having to specify dozens of parameters with every type of run. Furthermore, dispersion runs can utilize multiple presets of each type simply by calling the identifier for each preset (e.g., multiple predefined emissions locations can be added to a run or series of runs with a numeric vector). The presets are divided into three groups: "emissions", "grids", and "species". Each preset has a plaintext file that resides on disk of the same name (located in the SplitR folder). For removing presets from the associated files, the interactive function dispersion.preset.delete() can be used.
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
