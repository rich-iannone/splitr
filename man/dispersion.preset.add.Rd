\name{Add dispersion preset information}
\alias{dispersion.preset.add}
\title{
Add a preset for HYSPLIT dispersion model runs
}
\description{
The function allows for the setting of presets specific to dispersion runs. Presets are effectively blocks of parameters that are stored on the disk. Having presets is useful because these groups of parameters can be called in different dispersion runs (and across projects) without having to specify dozens of parameters with every type of run. Furthermore, dispersion runs can utilize multiple presets of each type simply by calling the identifier for each preset (e.g., multiple predefined emissions locations can be added to a run or series of runs with a numeric vector). The presets are divided into three groups: "emissions", "grids", and "species". Each preset has a plaintext file that resides on disk of the same name (located in the SplitR folder). For removing presets from the associated files, the interactive function dispersion.preset.delete() can be used.
}
\usage{
dispersion.preset.add(type, interactive = TRUE,
                      species_name = NULL,
                      particle_pdiam = NULL,
                      particle_density = NULL,
                      particle_shape_factor = NULL,
                      ddep_velocity = NULL,
                      ddep_MW = NULL,
                      ddep_A_ratio = NULL,
                      ddep_D_ratio = NULL,
                      ddep_Henrys_Law_coeff = NULL,
                      wdep_Henrys_Law_coeff = NULL,
                      wdep_in_cloud_dep = NULL,
                      wdep_below_cloud_dep = NULL,
                      rad_decay = NULL,
                      pollutant_resuspension_factor = NULL,
                      emissions_name = NULL,
                      emissions_rate = NULL,
                      emissions_duration = NULL,
                      emissions_start_time = NULL,
                      grid_center = NULL,
                      grid_spacing = NULL, 
                      grid_span = NULL,
                      grid_folder = NULL,
                      grid_filename = NULL,
                      grid_number_vertical = NULL,
                      grid_heights = NULL,
                      grid_start_time = NULL, 
                      grid_end_time = NULL,
                      sampling_interval_type_rate = NULL)
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
# Interactively add a preset for a 'emissions' input
dispersion.preset.add("emissions")

# Add a default preset for a 'species' input
dispersion.preset.add("species", interactive = TRUE,
                      species_name = 'default')
}
