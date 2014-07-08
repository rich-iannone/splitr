SplitR
======

SplitR is an R package that's great for conducting trajectory and dispersion modelling with [NOAA HYSPLIT](http://ready.arl.noaa.gov/HYSPLIT.php).

## Installation

Easy installation of SplitR from GitHub is entirely possible with the `devtools` package:

```R
require(devtools)
install_github('SplitR', 'rich-iannone')
```

## Description

SplitR allows you set up and run HYSPLIT in a very fast, easy, and organized manner. You can set up a few or, perhaps, thousands of trajectory or dispersion runs by using a single function. Because SplitR is an R interface to HYSPLIT, we can store output in data frames and take advantage of the vast selection of R packages to perform cluster analyses, generate and save plots, and more. This package simplifies the process of running HYSPLIT models by automating the downloading and storage of meteorological data files, providing a simple means to initiate a range of runs, and by providing outputs that can be easily applied to statistical analyses.

This package is in active development but is quite usable at present. Some of its features are:

- simple setup of trajectory and dispersion model runs (forward or backward)
- automated retrieval of meteorological data files
- organization of modelling presets and model output
- numerous batching options: set up models to run continuously with multiple start times throughout a single year, several years, or a predefined time range
- trajectory plotting and visualization of particle positions in dispersion runs

## Setup

Use of SplitR requires the installation of HYSPLIT. It can be obtained from the Air Resources Laboratory (ARL) of the National Oceanic and Atmospheric Administration (NOAA) [READY website](http://ready.arl.noaa.gov/HYSPLIT.php). 

For Windows installations, there are both [registered](http://ready.arl.noaa.gov/hyreg/HYSPLIT_pchysplit.php) and [unregistered](http://ready.arl.noaa.gov/HYSPLIT_hytrial.php) versions of HYSPLIT. The only difference in functionality between the two is that the unregistered version cannot perform model runs with forecast meteorological data. SplitR has the capability to perform forecast model runs so obtain the registered version if you intend to use that functionality. Regardless of version, both come with a standard installer that will place the main HYSPLIT folder (named 'hysplit') in the location of your choice. Within this main folder, there are 15 subdirectories.

For a Mac OS X installation, there is a [single distribution](http://ready.arl.noaa.gov/hyreg/HYSPLIT_applehysp.php) that has all functionality included.

Take note of the paths for the HYSPLIT executables and the working directory (in the subfolders 'exec' and 'working'). You can optionally create additional subfolders for locating HYSPLIT-ready meteorological data files, or, opt to place those in the HYSPLIT working directory. The examples below assume a Windows installation of HYSPLIT in the directory `C:\hysplit4\`. If working from the examples, please modify the example paths accordingly to match your platform and HYSPLIT folder paths.

## Example: HYSPLIT Trajectory Runs

To perform a series HYSPLIT trajectory model runs, use the SplitR `hysplit.trajectory` function:

```R
trajectory.df <- 
  hysplit.trajectory(traj_name = "t2",
                     return_traj_df = TRUE,
                     start_lat_deg = 42.83752, start_long_deg = -80.30364,
                     start_height_m_AGL = 5, simulation_duration_h = 24,
                     backtrajectory = FALSE,
                     met_type = "gdas1",
                     vertical_motion_option = 0,
                     top_of_model_domain_m = 20000,
                     run_type = "day", run_day = "2012-03-12",
                     daily_hours_to_start = c("00", "06", "12", "18"),
                     path_met_files =  "C:\\hysplit4\\met\\",
                     path_output_files = "C:\\hysplit4\\output\\",
                     path_wd = "C:\\hysplit4\\working\\",
                     path_executable = "C:\\hysplit4\\exec\\") 
```

This use of `hysplit.trajectory` sets up four trajectory runs that start at 00:00, 06:00, 12:00, and 18:00 UTC on March 12, 2012. The `traj_name` argument allows for the inclusion of a descriptive name for the set of runs. Setting `return_traj_df` to `TRUE` will instruct the function to return a data frame containing detailed trajectory information. Such a data frame (named here as the object `trajectory.df`) will be useful for conducting further analyses. The initial times for the model runs are set using `run_type = "day"`, `run_day = "2012-03-12"`, and `daily_hours_to_start = c("00", "06", "12", "18")`. The model runs are forward runs (moving forward in time, set here using `backtrajectory = FALSE`) and not backtrajectory runs (set with `backtrajectory = TRUE`). These runs are 24 h in duration (`simulation_duration_h = 24`). The starting location of 42.83752ºN and 80.30364ºW is set using `start_lat_deg = 42.83752` and `start_long_deg = -80.30364`; the starting height of 5 m above ground level is set by `start_height_m_AGL = 5`. The meteorological options include the type of met data to use (1º GDAS data is used here with `met_type = "gdas1"`--there is also the option to use NCEP reanalysis data with the `met_type = "reanalysis"` setting), the vertical motion option (here, set as `vertical_motion_option = 0` which instructs HYSPLIT to use the vertical motion available in the met data files), and, the top of the model domain (set as 20,000 meters with `top_of_model_domain_m = 20000`). Four paths require specification:

- path to the meteorological data files (`path_met_files`)
- path to the output files (`path_output_files`)
- path to the working directory (`path_wd`)
- path to the executable directory, specifically here to that path containing the `hyts_std` executable (`path_executable`)

All paths should exist (i.e., SplitR won't create directories) and the paths provided in the above `hysplit.trajectory()` example should serve as examples for these path settings. If running in a Mac or Linux environment, use appropriate paths with forward slashes (paths incorporating a tilde are acceptable).

The necessary meteorological data files relevant to the period being modelled will be downloaded from the NOAA FTP server (arlftp.arlhq.noaa.gov) if they are not present in the directory specified as the `path_met_files` argument. Note that SplitR does not currently provide an option to automatically delete these downloaded data files after the relevant model runs have been completed, so, keep in mind that available disk space may be issue with longer sequences of model runs (e.g., a GDAS1 met file for a week-long period can take up to 600 MB of disk space).

After this, four files should be generated:

- `traj(forward)-12-03-12-00-lat_42.83752_long_-80.30364-height_5-24h`
- `traj(forward)-12-03-12-06-lat_42.83752_long_-80.30364-height_5-24h`
- `traj(forward)-12-03-12-12-lat_42.83752_long_-80.30364-height_5-24h`
- `traj(forward)-12-03-12-18-lat_42.83752_long_-80.30364-height_5-24h`

On Mac/Linux, these files will be associated with a .zip archive that is named according to the value of `traj_name` (if provided) and the date/time of execution. The location of the archive will be that of the path provided in the `path_output_files` argument. In a Windows environment, a similarly named *folder* will be created in the path provided in the `path_output_files` argument and that will contain the output files.

If the the option to generate a data frame of trajectory information wasn't taken during the invocation of `hysplit.trajectory`, this can be done later by using the SplitR `trajectory.read` function:

```R
trajectory.df <- trajectory.read(archive_folder = "C:\\hysplit4\\working\\t2--2014-06-17--02-39-29",
                                 year = NULL,
                                 start_height_m_AGL = NULL)
```
Here, the name of the archive or folder is specified in the `archive_folder` argument. With the resultant data frame, statistical analyses for the trajectories can be generated (e.g., average heights of trajectories after specified time periods, etc.). Furthermore, the `trajectory.df` data frame is fully compatible with the excellent 'openair' package that is available on CRAN. Plotting of the trajectory output data frame requires use of openair's `trajPlot` function:

```R
trajPlot(trajectory.df, map.fill = FALSE)
```

Here are the trajectories from those model runs:

<img src="inst/trajectories.png" width="75%">


## Example: HYSPLIT Dispersion Runs

Before performing any dispersion model runs in SplitR, you need to initialize the working directory with a SETUP.CFG file:

```R
hysplit.dispersion.config(path_wd = "C:\\hysplit4\\working\\")
```

Presets for 'species', 'grids', and 'emissions' then need to be set. All additions of presets are made with the use of the `dispersion.preset.add`. That function can be run interactively with `interactive = TRUE` and remembering to set the `type` argumemnt as either `type = "species"`, `type = "grids"`, or `type = "emissions"`. An example of an interactive session for creating a 'species' preset is given below (angled brackets represent user input).

```R
dispersion.preset.add(type = "species", interactive = TRUE,
                      path_wd = "C:\\hysplit4\\working\\")
```

```
What is the name of the species?  <test>
Is the species a gas or particle? [gas/particle] <gas>
Use the default parameters for a gas-phase species? [y/n] <y>
The plan. Adding species: test
-------------------------
Particle Properties // diameter: 0 µm | density: 0 g/cm3 | shape factor: 0 -- not a particle species
Dry Deposition // deposition velocity: 0 m/s | molecular weight: 0 g/mol
                  A ratio: 0 | D ratio: 0 | Henry's Law: 0 M/a -- no dry deposition
Wet Deposition // Henry's Law coeff.: 0 M/a | in-cloud deposition: 0 L/L
                  below-cloud deposition: 0 1/s -- no wet deposition
Radioactive Decay // half-life: 0 days
Pollutant Resuspension // factor: 0 1/m
------------------------------
This is what will be set. Okay? [y/n]: <y>
```

Also, the function can be used to create a 'species' preset non-interactively:

```R
dispersion.preset.add(type = "species", interactive = FALSE,
                      species_name = "test",
                      particle_pdiam = 0,
                      particle_density = 0,
                      particle_shape_factor = 0,
                      ddep_velocity = 0,
                      ddep_MW = 0,
                      ddep_A_ratio = 0,
                      ddep_D_ratio = 0,
                      ddep_Henrys_Law_coeff = 0,
                      wdep_Henrys_Law_coeff = 0,
                      wdep_in_cloud_dep = 0,
                      wdep_below_cloud_dep = 0,
                      rad_decay = 0,
                      pollutant_resuspension_factor = 0,
                      path_wd = "C:\\hysplit4\\working\\")
```

Interactively creating a 'grids' preset looks like this:

```R
dispersion.preset.add(type = 'grids', interactive = TRUE,
                      path_wd = "C:\\hysplit4\\working\\")
```

```
What is the name of the grid? <"grid">

Provide the center of the grid.
Units: degrees. Default: 49.289328, -123.117665.
Provide the latitude and longitude (<ENTER> for default values):
<42.83752, -80.30364>

Provide the spacing of adjacent grid points in the x and y directions.
Units: degrees. Default: 0.05, 0.05.
Provide latitude and longitude intervals (<ENTER> for default values):
<0.05, 0.05>

Provide the total span of the grid in the x and y directions.
Units: degrees. Default: 1.00, 1.00.
Provide latitude and longitude values (<ENTER> for default values): 
<1.00, 1.00>

Provide the number of vertical levels in the concentration grid.
This number includes the deposition layer (with height = 0 m AGL)
Default: 1.
Provide a postive integer (<ENTER> for default value): 
<1>

For the single level specified, does that refer to the ground (deposition layer) or some height 
above the ground?
Press <ENTER> to assign level to the ground layer, or, provide a height in meters above ground level: 
<ENTER>

Provide a date and time for the start of grid sampling.
Use the format YYYY-MM-DD HH:MM (<ENTER> for default value)
<2012-03-12 00:00>

Provide a date and time for the end of grid sampling.
Use the format YYYY-MM-DD HH:MM (<ENTER> for default value)
<2012-03-13 00:00>

Provide the type of grid sampling to perform.
Choices are: (1) averaging, (2) snapshot, or (3) maximum
Press <ENTER> to assign the 'averaging' method
<1>

Provide the grid sampling measurement frequency.
Use the format HH:MM
Press <ENTER> to assign a 1-hour measurement frequency
<01:00>

The plan. Adding grid: "grid"
----------------------------------
            Grid Center: 42.83752, -80.30364
           Grid Spacing: 0.05, 0.05
              Grid Span: 1, 1
 No. of Vertical Levels: 1
           Grid Heights: 0 m
      Start of Sampling: 2012-03-13 00:00
        End of Sampling: 2012-03-13 00:00
        Sampling Method: 0
     Sampling Frequency: 1 h
----------------------------------
This is what will be set. Okay? [y/n]: 
<y>
```

Also, the 'grids' preset be set non-interactively like this:

```R
dispersion.preset.add(type = 'grids', interactive = FALSE,
                       grid_name = "grid",
                       grid_center = "42.83752 -80.30364",
                       grid_spacing = "0.05 0.05",
                       grid_span = "1 1",
                       grid_folder = "C:\\hysplit4\\working\\",
                       grid_filename = "grid",
                       grid_number_vertical = "1",
                       grid_heights = "0",
                       grid_start_time = "12 03 12 00 00",
                       grid_end_time = "12 03 13 00 00",
                       sampling_interval_type_rate = "1 01 00",
                       path_wd = "C:\\hysplit4\\working\\")
```

Interactively creating an 'emissions' preset looks like this:

```R
dispersion.preset.add(type = 'emissions', interactive = TRUE,
                      path_wd = "C:\\hysplit4\\working\\")
```

```
What is the name of the emissions source? <test>

Provide the starting date and time.
Several options available:
(1) Use defined time in the format YYYY-MM-DD HH:MM
(2) Provide the number of hours or days after start of run (# h, or # d)
(3) Press <ENTER> to match run and sampling starting times
<2012-03-12 00:00>

Provide either a time duration in hours
or days, or, provide an ending date and time.
Use the formats # h, # d, or YYYY-MM-DD HH:MM
<1 d>

Provide the rate of emissions in mass units per hour.
<35>

The plan. Adding emissions source: test
----------------------------------
Start Date/Time: 2012-03-12 00:00
       Duration: 24 h
 Emissions Rate: 35 (mass units)/h
----------------------------------
This is what will be set. Okay? [y/n]: 
<y>
```

For the non-interactive creation of an 'emissions' preset:

```R
dispersion.preset.add(type = 'emissions', interactive = FALSE,
                      emissions_name = "test",
                      emissions_rate = 35,
                      emissions_duration = 24,
                      emissions_start_time = "12 03 12 00 00",
                      path_wd = "C:\\hysplit4\\working\\")
```

Once the presets have been created, they can be read using the `dispersion.preset.list` function by specifying the type of present (using the read argument) and providing the path of the working directory. For the 'species' presets:

```R
dispersion.preset.list(read = 'species',
                       path_wd = "C:\\hysplit4\\working\\")
```

```
Here are the current species presets
------------------------------------
(1) test / Particle: 0 0 0 / DDep: 0 0 0 0 0 / WDep: 0 0 0 / RD: 0 / RS: 0
------------------------------------
```

```R
dispersion.preset.list(read = 'grids',
                       path_wd = "C:\\hysplit4\\working\\")
```

```
Here are the current presets for grids
--------------------------------------
(1) grid / C: 42.83752, -80.30364 / I: 0.05, 0.05 / S: 1, 1 / 1 lv / s->e: 12 03 12 00 00 - 12 03 13 00 00 / avg: 1 01 00
--------------------------------------
```

```R
dispersion.preset.list(read = 'emissions',
                       path_wd = "C:\\hysplit4\\working\\")
```

```
Here are the current presets for emissions
------------------------------------------
(1) test / Rate: 35 (mass units)/h / Duration: 24 h / Release: 12 03 12 00 00
------------------------------------------
```

Adding more presets of any type will simply add items to each list provided by the `dispersion.preset.list` function.

To perform a HYSPLIT dispersion model run, use the SplitR `hysplit.dispersion` function:

```R
hysplit.dispersion(start_lat_deg = 42.83752, start_long_deg = -80.30364,
                   start_height_m_AGL = 5, simulation_duration_h = 24,
                   backward_running = FALSE,
                   met_type = "gdas1",
                   vertical_motion_option = 0,
                   top_of_model_domain_m = 20000,
                   run_type = "day",
                   run_day = "2012-03-12",
                   daily_hours_to_start = "00",
                   emissions = c(1),
                   species = c(1),
                   grids = c(1),
                   path_met_files =  "C:\\hysplit4\\working\\",
                   path_output_files = "C:\\hysplit4\\working\\",
                   path_wd = "C:\\hysplit4\\working\\",
                   path_executable = "C:\\hysplit4\\exec\\") 
```

This use of `hysplit.dispersion` sets up a single dispersion run that starts at 00:00 UTC on March 12, 2012. These initial times are set using `run_type = "day"`, `run_day = "2012-03-12"`, and `daily_hours_to_start = "00"`. The model run is a forward run (moving forward in time, set here using `backward_running = FALSE`) and not backwards (set with `backward_running = TRUE`). Essentially, running in forward mode means the starting location is a source of emissions; running backward means that the starting location is a receptor. This run has been set to be modelled for 24 h (`simulation_duration_h = 24`). The starting location of 42.83752ºN and 80.30364ºW is set using `start_lat_deg = 42.83752` and `start_long_deg = -80.30364`; the starting height of 5 m above ground level is set by `start_height_m_AGL = 5`. The meteorological options include the type of met data to use (1º GDAS data is used here with `met_type = "gdas1"`--there is also the option to use NCEP reanalysis data with the `met_type = "reanalysis"` setting), the vertical motion option (here, set as `vertical_motion_option = 0` which instructs HYSPLIT to use the vertical motion available in the met data files), and, the top of the model domain (set as 20,000 meters with `top_of_model_domain_m = 20000`).

Remember those presets that were added earlier? They are called up in the `emissions`, `species`, and `grids` arguments. The `c(1)` value provided for each of those corresponds to the first preset of each type of preset. If you ever need to remind yourself of which presets are currently in the system, use `dispersion.preset.list` function. Moreover, that function has an interactive mode! Just invoke it and supply just the path for the working directory:

```R
dispersion.preset.list(path_wd = "C:\\hysplit4\\working\\")
```

```
Which preset type would you like to list?
Choices are: (1) emissions, (2) grids, (3) species
Press <ENTER> to exit
<1>

Here are the current presets for emissions
------------------------------------------
(1) test / Rate: 1 (mass units)/h / Duration: 336 h / Release: 12 03 22 00 00
------------------------------------------
```

While adding presets is generally a good thing to do, there may come a point where you would like to delete some of the presets. This can be done with the `dispersion.preset.delete` function. It can be done interactively, which is probably the safer method:

```R
dispersion.preset.delete(path_wd = "C:\\hysplit4\\working\\")
```

```
What type of preset would you like to delete?
Choices are: (1) emissions, (2) grids, (3) species
Press <ENTER> for no deletion. Otherwise, enter a number or type
<1>

Here are the current presets for emissions
------------------------------------------
(1) test / Rate: 1 (mass units)/h / Duration: 336 h / Release: 12 03 22 00 00
------------------------------------------
Which preset number should be deleted?
Press <ENTER> for no deletion. Otherwise, enter a number. 
<1>
```

Getting back to the `hysplit.dispersion` function, four paths require specification:

- path to the meteorological data files (`path_met_files`)
- path to the output files (`path_output_files`)
- path to the working directory (`path_wd`)
- path to the executable directory, specifically here to that path containing the `hyts_std` executable (`path_executable`)

After executing this function (and possibly waiting awhile, since met files will need to be downloaded), 24 CSV files with particle position will become available in the working directory:

- `GIS_part_[001]_ps.csv`

Also, there will be 24 .jpg image files with particles overlaid onto a map at each hour of the model run:

- `map---disp(forward)-12-03-12-00-lat_42.83752_long_-80.30364-height_5-24h-[001].jpg`

A binary file containing gridded concentrations will be available in the working directory:

- `grid--disp(forward)-12-03-12-00-lat_42.83752_long_-80.30364-height_5-24h`

The met files that were automatically downloaded will remain in the working directory:

- `gdas1.mar12.w1`
- `gdas1.mar12.w2`
- `gdas1.mar12.w3`
- `gdas1.mar12.w4`
- `gdas1.mar12.w5`

Finally, a binary `PARDUMP` file will be written.

Here is a snapshot of the dispersion particles at hour 16 of 24:

<img src="inst/dispersion-Simcoe-hour16.jpg" width="75%">

## Future Additions to SplitR

- greater support for different types of meteorological data files
- support for dispersion from line sources
- improved graphics for trajectory and dispersion model run outputs
- options for merging data frames of backtrajectories with pollutant concentrations for advanced air quality analyses
- statistical summaries for trajectories and dispersion of particles
- ensemble model runs
- capability for the starting location to be moving during successive model runs
