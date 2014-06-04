SplitR
======

SplitR is a package that's great for conducting trajectory and dispersion modelling with [NOAA HYSPLIT](http://ready.arl.noaa.gov/HYSPLIT.php).

## Installation

Easy installation of SplitR from GitHub is entirely possible with the `devtools` package:

```coffee
require(devtools)
install_github('SplitR', 'rich-iannone')
```

## Description

SplitR allows you set up and run HYSPLIT in a very fast, easy, and organized manner. You easily set up a few or, perhaps, thousands of trajectory or dispersion runs using a single function. Because SplitR is an R interface to HYSPLIT, we can store output in data frames and do things with it (e.g., perform cluster analyses, generate and save plots, etc.). The package sets up the folders, gets the HYSPLIT binaries, automates the downloading and storage of meteorological data files, and encourages you to make presets for dispersion model runs.

This package is in active development but is quite usable at present. Some of its features are:

- automatic downloading and installation of HYSPLIT binary executables
- organization of modelling presets and output through the SplitR project framework
- automated retrieval of meteorological data files
- simple setup of trajectory and dispersion model runs (forward or backward)
- numerous batching options: set up models to run continuously with multiple start times throughout a single year, several years, or a predefined time range
- trajectory plotting and visualization of particle positions in dispersion runs
- clustering of trajectories and plotting of clusters

## Setup

Use of SplitR requires a basic installation of HYSPLIT, as obtained from the Air Resources Laboratory (ARL) of the National Oceanic and Atmospheric Administration (NOAA) [READY website](http://ready.arl.noaa.gov/HYSPLIT.php). 

For Windows installations, there are both [registered](http://ready.arl.noaa.gov/hyreg/HYSPLIT_pchysplit.php) and [unregistered](http://ready.arl.noaa.gov/HYSPLIT_hytrial.php) versions of the HYSPLIT package. The only difference in functionality between the two packages is that the unregistered version cannot perform model runs with forecast meteorological data. Either of the packages comes with an installer that will place the main HYSPLIT folder (named 'hysplit') in the location of your choice. Within this main folder, there are 15 subdirectories.

For a Mac OS X installation, there is a [single package](http://ready.arl.noaa.gov/hyreg/HYSPLIT_applehysp.php) that has all functionality included.

Take note of the paths for the HYSPLIT executables and` working directory (in the subfolders 'exec' and 'working'). You can optionally create additional subfolders for locating HYSPLIT-ready meteorological data files, or, opt to place those in the HYSPLIT working directory.

## Example for Running a Series of HYSPLIT Trajectory Runs

To perform a series HYSPLIT trajectory model runs, use the SplitR `hysplit.trajectory` function:

```coffee
hysplit.trajectory(start_lat_deg = 42.83752, start_long_deg = -80.30364,
                   start_height_m_AGL = 5, simulation_duration_h = 24,
                   backtrajectory = FALSE,
                   met_type = "gdas1",
                   vertical_motion_option = 0,
                   top_of_model_domain_m = 20000,
                   run_type = "day", run_day = "2012-03-12",
                   daily_hours_to_start = c("00", "06", "12", "18"),
                   path_met_files =  "C:\\hysplit4\\working\\",
                   path_output_files = "C:\\hysplit4\\working\\",
                   path_wd = "C:\\hysplit4\\working\\",
                   path_executable = "C:\\hysplit4\\exec\\") 
```

This use of `hysplit.trajectory` sets up four trajectory runs that start at 00:00, 06:00, 12:00, and 18:00 UTC on March 12, 2012. These initial times are set using `run_type = "day"`, `run_day = "2012-03-12"`, and `daily_hours_to_start = c("00", "06", "12", "18")`. The model runs are forward runs (moving forward in time, set here using `backtrajectory = FALSE`) and not backtrajectory runs (set with `backtrajectory = TRUE`). These runs are 24 h in duration (`simulation_duration_h = 24`). The starting location of 42.83752ºN and 80.30364ºW is set using `start_lat_deg = 42.83752` and `start_long_deg = -80.30364`; the starting height of 5 m above ground level is set by `start_height_m_AGL = 5`. The meteorological options include the type of met data to use (1º GDAS data is used here with `met_type = "gdas1"`--there is also the option to use NCEP reanalysis data with the `met_type = "reanalysis"` setting), the vertical motion option (here, set as `vertical_motion_option = 0` which instructs HYSPLIT to use the vertical motion available in the met data files), and, the top of the model domain (set as 20,000 meters with `top_of_model_domain_m = 20000`). Four paths require specification:

- path to the meteorological data files (`path_met_files`)
- path to the output files (`path_output_files`)
- path to the working directory (`path_wd`)
- path to the executable directory, specifically here to that path containing the `hyts_std` executable (`path_executable`)

All paths should exist (i.e., SplitR won't create directories) and the paths provided in the above `hysplit.trajectory()` example should serve as examples for these path settings. If running in a Mac or Linux environment, use appropriate paths with forward slashes (paths using a tilde are acceptable).

The necessary meteorological data files relevant to the period being modelled will be downloaded from the NOAA FTP server (arlftp.arlhq.noaa.gov) if they are not present in the directory specified as the `path_met_files` argument. Note that SplitR does not currently provide an option to automatically delete these downloaded data files after the relevant model runs have been completed, so, keep in mind that available disk space may be issue with longer sequences of model runs (e.g., a GDAS1 met file for a week-long period can take up to 600 MB of disk space).

After this, four files should be generated and residing in the 'working' folder:

- `traj(forward)-12-03-12-00-lat_42.83752_long_-80.30364-height_5-24h`
- `traj(forward)-12-03-12-06-lat_42.83752_long_-80.30364-height_5-24h`
- `traj(forward)-12-03-12-12-lat_42.83752_long_-80.30364-height_5-24h`
- `traj(forward)-12-03-12-18-lat_42.83752_long_-80.30364-height_5-24h`

A data frame can be generated from these output files using the SplitR `trajectory.read` function:

```coffee
trajectory.df <- trajectory.read(path_output_files = "C:\\hysplit4\\working\\",
                                 year = NULL,
                                 start_height_m_AGL = NULL)
```

With this data frame, statistical analyses for the trajectories can be generated (e.g., average heights of trajectories after specified time periods, etc.). Furthermore, the `trajectory.df` data frame is fully compatible with the excellent 'openair' package that is available on CRAN. Plotting of the trajectory output data frame requires use of openair's `trajPlot` function:

```coffee
trajPlot(trajectory.df, map.fill = FALSE)
```

Here are the trajectories from those model runs:

<img src="inst/trajectories.png" width="75%">


## Example for Running a Series of HYSPLIT Dispersion Runs

Before performing any dispersion model runs in SplitR, you need to initialize the working directory with a SETUP.CFG file:

```coffee
hysplit.dispersion.config(path_wd = "C:\\hysplit4\\working\\")
```

Presets for 'species', 'grids', and 'emissions' then need to be set. All additions of presets are made with the use of the `dispersion.preset.add`. That function can be run interactively with `interactive = TRUE` and remembering to set the `type` argumemnt as either `type = "species"`, `type = "grids"`, or `type = "emissions"`. Here is an example of an interactive session for creating a 'species' preset is given below.

```coffee
dispersion.preset.add(type = "species", interactive = TRUE,
                      path_wd = "C:\\hysplit4\\working\\")

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

```coffee
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

```coffee
dispersion.preset.add(type = 'grids', interactive = TRUE,
                      path_wd = "C:\\hysplit4\\working\\")

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
            Grid Center: 42.83752Âº, -80.30364Âº
           Grid Spacing: 0.05Âº, 0.05Âº
              Grid Span: 1Âº, 1Âº
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

```coffee
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

```coffee
dispersion.preset.add(type = 'emissions', interactive = TRUE,
                      path_wd = "C:\\Users\\riannone\\hysplit4\\working\\")

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

```coffee
dispersion.preset.add(type = 'emissions', interactive = FALSE,
                      emissions_name = "test",
                      emissions_rate = 35,
                      emissions_duration = 24,
                      emissions_start_time = "12 03 12 00 00",
                      path_wd = "C:\\hysplit4\\working\\")
```

Once the presets have been created, they can be read using the `dispersion.preset.list` function by specifying the type of present (using the read argument) and providing the path of the working directory. For the 'species' presets:

```coffee
# List the dispersion 'species' presets
dispersion.preset.list(read = 'species',
                       path_wd = "C:\\hysplit4\\working\\")

Here are the current species presets
------------------------------------
(1) test / Particle: 0 0 0 / DDep: 0 0 0 0 0 / WDep: 0 0 0 / RD: 0 / RS: 0
------------------------------------

dispersion.preset.list(read = 'grids',
                       path_wd = "C:\\hysplit4\\working\\")

Here are the current presets for grids
--------------------------------------
(1) grid / C: 42.83752, -80.30364 / I: 0.05, 0.05 / S: 1, 1 / 1 lv / s->e: 12 03 12 00 00 - 12 03 13 00 00 / avg: 1 01 00
--------------------------------------

dispersion.preset.list(read = 'emissions',
                       path_wd = "C:\\hysplit4\\working\\")

Here are the current presets for emissions
------------------------------------------
(1) test / Rate: 35 (mass units)/h / Duration: 24 h / Release: 12 03 12 00 00
------------------------------------------
```

Adding more presets of any type will simply add items to each list provided by the `dispersion.preset.list` function.

To perform a series HYSPLIT dispersion model runs, use the SplitR `hysplit.dispersion` function:

```coffee
hysplit.dispersion(start_lat_deg = 42.83752, start_long_deg = -80.30364,
                   start_height_m_AGL = 5, simulation_duration_h = 24,
                   backward_running = FALSE,
                   met_type = "gdas1",
                   vertical_motion_option = 0,
                   top_of_model_domain_m = 20000,
                   run_type = "day",
                   run_day = "2012-03-12",
                   run_range = NULL,
                   run_years = NULL,
                   daily_hours_to_start = "00",
                   emissions = c(1),
                   species = c(1),
                   grids = c(1),
                   path_met_files =  "C:\\hysplit4\\working\\",
                   path_output_files = "C:\\hysplit4\\working\\",
                   path_wd = "C:\\hysplit4\\working\\",
                   path_executable = "C:\\hysplit4\\exec\\") 
```

After this, 24 CSV files with particle position will become available in the working directory:

- `GIS_part_[001]_ps.csv`

Also, there will be 24 .jpg image files with particles overlaid onto a map at each hour of the model run:

- `map---disp(forward)-12-03-12-00-lat_42.83752_long_-80.30364-height_5-24h-[001].jpg`

A binary file containing gridded concentrations will be available in the working directory:

- `grid--disp(forward)-12-03-12-00-lat_42.83752_long_-80.30364-height_5-24h`

Finally, a binary PARDUMP file will be written:

- `PARDUMP`

Here is a snapshot of the dispersion particles at hour 16 of 24:

<img src="inst/dispersion-Simcoe-hour16.jpg" width="75%">

## Future Additions to SplitR

- greater support for different types of meteorological data files
- options for merging data frames of backtrajectories with pollutant concentrations for advanced air quality analyses
- statistical summaries for dispersion of particles
- ensemble model runs
- capability for the starting location to be moving during successive model runs
