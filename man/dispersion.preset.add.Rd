\name{Add dispersion preset information}
\alias{dispersion.preset.add}
\title{
Add a preset for HYSPLIT dispersion model runs
}
\description{
The function allows for the setting of presets specific to dispersion runs. Presets are effectively blocks of parameters that are stored on the disk. Having presets is useful because these groups of parameters can be called in different dispersion runs (and across projects) without having to specify dozens of parameters with every type of run. Furthermore, dispersion runs can utilize multiple presets of each type simply by calling the identifier for each preset (e.g., multiple predefined emissions locations can be added to a run or series of runs with a numeric vector). The presets are divided into three groups: "emissions", "grids", and "species". Each preset has a plaintext file that resides on disk of the same name (located in the SplitR folder). For removing presets from the associated files, the interactive function dispersion.preset.delete() can be used.

The primary means of using this function is to do so interactively, where prompts advise on the parameter types for which values are required. However, the function allows for non-interactively entering parameters for a single preset in a function call. Use of the function in this way requires some knowledge of the parameters that comprise a HYSPLIT CONTROL file. For parameters that are omitted for a given preset type, default values are used when writing the CONTROL file.
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
                      grid_name = NULL,
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
  \item{interactive}{
determines whether the function should run in an interactive mode, prompting the for input on which kinds of values to supply for each parameter.
}
  \item{species_name}{
a case sensitive name that identifies the species preset. The use of a name is useful since presets can be referenced by this name when using the 'dispersion.preset.list' function.
}
  \item{particle_pdiam}{
the particle diameter in units of µm.
}
  \item{particle_density}{
the particle density in units of g/cm3.
}
  \item{particle_shape_factor}{
the particle shape factor as a value between 0-1.
}
  \item{ddep_velocity}{
the dry deposition velocity in units of m/s.
}
  \item{ddep_MW}{
the molecular weight in units of g/mol.
}
  \item{ddep_A_ratio}{
the dry deposition A ratio.
}
  \item{ddep_D_ratio}{
the dry deposition D ratio.
}
  \item{ddep_Henrys_Law_coeff}{
the Henry's Law value associated with dry deposition.
}
  \item{wdep_Henrys_Law_coeff}{
the Henry's Law value associated with wet deposition.
}
  \item{wdep_in_cloud_dep}{
the in-cloud wet deposition rate in units of L/L.
}
  \item{wdep_below_cloud_dep}{
the below cloud wet deposition rate in units of 1/s.
}
  \item{rad_decay}{
the rate of radioactive decay, in units of days.
}
  \item{pollutant_resuspension_factor}{
the pollutant resuspension factor in units of 1/m.
}
  \item{emissions_name}{
a case sensitive name that identifies the emissions preset. The use of a name is useful since presets can be referenced by this name when using the 'dispersion.preset.list' function.
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
