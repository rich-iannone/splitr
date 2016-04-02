<img src="inst/SplitR.png" width="70%">

[![DOI](https://zenodo.org/badge/20543/rich-iannone/SplitR.svg)](https://zenodo.org/badge/latestdoi/20543/rich-iannone/SplitR)

**SplitR** is an **R** package for conducting trajectory and dispersion modelling with **HYSPLIT**. You can determine where air (came from | is going), or, where particulate matter (came from | is going). More information on the **HYSPLIT** model can be obtained from the **Air Resources Laboratory** (**ARL**) of the **National Oceanic and Atmospheric Administration** (**NOAA**) [READY website](http://ready.arl.noaa.gov/HYSPLIT.php).

## Installation

Install **SplitR** from **GitHub** using the `devtools` package:

```R
library(devtools)
devtools::install_github("rich-iannone/SplitR")
```

## Description

**SplitR** allows you set up and run **HYSPLIT** in a very fast, easy, and organized manner. You can set up a few or, perhaps, thousands of trajectory or dispersion runs by using a single function. Because **SplitR** is an **R** interface to **HYSPLIT**, we can store output in data frames and take advantage of the vast selection of **R** packages to perform cluster analyses, generate and save plots, and more. This package simplifies the process of running **HYSPLIT** models by automating the downloading and storage of meteorological data files, providing a simple means to initiate a range of runs, and by providing outputs that can be easily applied to statistical analyses.

Some of the things you can do with **SplitR** are:

- run multiple trajectory and dispersion model runs (forward or backward) with a single function call
- automatically download all meteorological data files for the run
- store dispersion modelling presets for easy retrieval
- run numerous models through different batching options: multiple start times throughout a single year, several years, or a predefined time range
- visualize wind trajectories and particle positions throughout trajectory and dispersion runs

## **HYSPLIT** Trajectory Runs

To perform a series **HYSPLIT** trajectory model runs, one can simply use the **SplitR** `hysplit_trajectory()` function:

```R
library(SplitR)
setwd("~/Documents/SplitR_wd")

trajectory <- 
  hysplit_trajectory(
    lat = 42.83752,
    lon = -80.30364,
    height = 50,
    duration = 24,
    run_period = "2012-03-12",
    daily_hours = c(0, 6, 12, 18),
    backtrajectory = FALSE,
    met_type = "gdas1",
    extended_met = TRUE,
    return_traj_df = TRUE,
    traj_name = "trajectory") 
```

This use of `hysplit_trajectory()` sets up four trajectory runs that start at 00:00, 06:00, 12:00, and 18:00 UTC on March 12, 2012. The `traj_name` argument allows for the inclusion of a descriptive name for the set of runs. Setting `return_traj_df` to `TRUE` will instruct the function to return a data frame containing detailed trajectory information. Such a data frame (named here as the object `trajectory`) will be have the following columns when `extended_met` is set to `FALSE`:

- `receptor` a numeric label for the receptor
- `year`, `month`, `day`, `hour` integer values for date/time components
- `hour.inc` the integer hour difference compared to the run starting time
- `lat`, `lon`, `height` the latitude, longitude, and height (meters above ground level) of the air mass along the trajectory
- `pressure` the air pressure along the trajectory (in hPa)
- `date2` a POSIXct date-time value (in UTC) for the air mass along the trajectory
- `date` a POSIXct date-time value (in UTC) for the time of release or time of incidence at the receptor site

If the model is run with `extended_met` set to `TRUE` then the following columns will also be available in the data frame:

- `theta` the potential temperature (in K) along the trajectory
- `air_temp` the ambient air temperature (in K) along the trajectory
- `rainfall` the rate of rainfall (in mm/h) along the trajectory
- `mixdepth` the mixing depth (or mixing height, in meters) along the trajectory
- `rh` the relative humidity along the trajectory
- `sp_humidity` the specific humidity (in g/kg) along the trajectory
- `h2o_mixrate` the mixed layer depth (in meters) along the trajectory
- `terr_msl` the terrain height at the location defined by `lat` and `long`
- `sun_flux` the downward solar radiation flux (in watts) along the trajectory

The receptor/origin locations are set using `lat` and `lon` for the latitude(s) and longitude(s). The starting location of 42.83752ºN and 80.30364ºW is set here using `lat = 42.83752` and `lon = -80.30364`. Equal-length vectors of `lat` and `lon` values can be used here to create an ensemble of model runs. The starting height of 5 m above ground level is set by `height = 5`.

The initial times for the model runs are set using `run_period = "2012-03-12"` and `daily_hours = c(0, 6, 12, 18)`. Several years of runs can be initiated using `run_period = c(2012, 2014)`, model runs can be performed between a range of dates as well (`run_period = c("2012-03-12", "2013-05-23")`). These runs are 24 h in duration (`duration = 24`).

The model runs are forward runs (moving forward in time, set here using `backtrajectory = FALSE`) and not backtrajectory runs (set with `backtrajectory = TRUE`).

The meteorological options include the type of met data to use. The 1º **GDAS** data is used here with `met_type = "gdas1"` but there is also the option to use **NCEP** reanalysis data with the `met_type = "reanalysis"` setting.

The necessary meteorological data files relevant to the period being modelled will be downloaded from the **NOAA** FTP server if they are not present in the working directory. After **SplitR** downloads the met files and runs the models, output files will be placed in the working directory and data frame of trajectory information will be returned.

Models can also be defined and executed using a modeling object in a **magrittr** workflow. Here's an example:

```R
library(SplitR)
library(magrittr)

setwd("~/Documents/SplitR_wd")

# Create the `trajectory model` object, add
# a grid of starting locations, add run
# parameters, and execute the model runs
trajectory_model <-
  create_trajectory_model() %>%
  add_grid(
    lat = 49.0,
    lon = -123.0,
    grid_ref = "center",
    range = c(0.8, 0.8),
    division = c(0.2, 0.2)) %>%
  add_params(
    height = 50,
    duration = 6,
    run_period = "2015-07-01",
    daily_hours = c(0, 12),
    backtrajectory = TRUE,
    met_type = "reanalysis") %>%
  run_model

# Get a data frame of the model results
trajectory_model %>% get_traj_df
```

This pipeline setup allows for more flexibility as **R** objects can be piped in for variation in the types of models created. For example, the `add_grid()` allows for the simple creation of a grid for multiple starting locations in an ensemble run. One or more `add_params()` statements can be used to write model parameters to the model object. Ending the pipeline with `run_model()` runs the model and creates results which can be extracted using `get_traj_df()`.

#### Plotting Trajectory Data

Trajectories can be plotted onto an interactive map. Use the `trajectory_plot()` function with either the `trajectory` data frame (created directly by the `hysplit_trajectory()` function), or, even better, with a trajectory model object.

```R
library(SplitR)
library(magrittr)

# Plotting using the trajectory data frame
trajectory_plot(trajectory)

# Plotting using the trajectory model object
trajectory_model %>% trajectory_plot
```

The visualization will appear in the **RStudio** Viewer:

<img src="inst/trajectory_plot.png" width="100%">

The trajectory points and paths are layers where their visibility can be toggled using the *Layers* icon at the top-right of the view. The following selection of basemaps is also provided:

- CartoDB Dark Matter
- CartoDB Positron
- ESRI World Terrain
- Stamen Toner

Clicking any of the points along the trajectory will provide an informative popup with time/position info and meteorological data for that location at that point in time:

<img src="inst/trajectory_popup.png" width="100%">

## **HYSPLIT** Dispersion Runs

Presets for `species`, `grids`, and `emissions` first need to be set. All additions of presets are made with the use of the `dispersion_preset_add()` function. It can be run interactively with `interactive = TRUE`, remembering to set the `type` argumemnt as either `type = "species"`, `type = "grids"`, or `type = "emissions"`. An example of an interactive session for creating a `species` preset is given below (angled brackets represent user input).

```R
dispersion_preset_add(type = "species", interactive = TRUE)
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

Also, the function can be used to create a `species` preset non-interactively:

```R
dispersion_preset_add(
  type = "species",
  interactive = FALSE,
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
  pollutant_resuspension_factor = 0)
```

Interactively creating a `grids` preset looks like this:

```R
dispersion_preset_add(type = 'grids', interactive = TRUE)
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
      Start of Sampling: 2012-03-12 00:00
        End of Sampling: 2012-03-13 00:00
        Sampling Method: 0
     Sampling Frequency: 1 h
----------------------------------
This is what will be set. Okay? [y/n]: 
<y>
```

Also, the `grids` preset be set non-interactively like this:

```R
dispersion_preset_add(
  type = 'grids',
  interactive = FALSE,
  grid_name = "grid",
  grid_center = "42.83752 -80.30364",
  grid_spacing = "0.05 0.05",
  grid_span = "1 1",
  grid_filename = "grid",
  grid_number_vertical = "1",
  grid_heights = "0",
  grid_start_time = "12 03 12 00 00",
  grid_end_time = "12 03 13 00 00",
  sampling_interval_type_rate = "1 01 00")
```

Interactively creating an `emissions` preset looks like this:

```R
dispersion_preset_add(type = 'emissions', interactive = TRUE)
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

For the non-interactive creation of an `emissions` preset:

```R
dispersion_preset_add(
  type = 'emissions',
  interactive = FALSE,
  emissions_name = "test",
  emissions_rate = 35,
  emissions_duration = 24,
  emissions_start_time = "12 03 12 00 00")
```

Once the presets have been created, they can be read using the `dispersion_preset_list()` function by specifying the type of present (using the read argument) and providing the path of the working directory. For the `species` presets:

```R
dispersion_preset_list(read = 'species')
```

```
Here are the current species presets
------------------------------------
(1) test / Particle: 0 0 0 / DDep: 0 0 0 0 0 / WDep: 0 0 0 / RD: 0 / RS: 0
------------------------------------
```

```R
dispersion_preset_list(read = 'grids')
```

```
Here are the current presets for grids
--------------------------------------
(1) grid / C: 42.83752, -80.30364 / I: 0.05, 0.05 / S: 1, 1 / 1 lv / s->e: 12 03 12 00 00 - 12 03 13 00 00 / avg: 1 01 00
--------------------------------------
```

```R
dispersion_preset_list(read = 'emissions')
```

```
Here are the current presets for emissions
------------------------------------------
(1) test / Rate: 35 (mass units)/h / Duration: 24 h / Release: 12 03 12 00 00
------------------------------------------
``` 

Adding more presets of any type will simply add items to each list provided by the `dispersion_preset_list()` function.

To perform a **HYSPLIT** dispersion model run, use the **SplitR** `hysplit_dispersion()` function:

```R
dispersion_2012_03_12 <-
  hysplit_dispersion(
    lat = 42.83752,
    lon = -80.30364,
    height = 5,
    duration = 24,
    met_type = "gdas1",
    run_type = "day",
    run_day = "2012-03-12",
    daily_hours = 0,
    backward_running = FALSE,
    emissions = 1,
    species = 1,
    grids = 1) 
```

This use of `hysplit_dispersion()` sets up a single dispersion run that starts at 00:00 UTC on March 12, 2012. These initial times are set using `run_type = "day"`, `run_day = "2012-03-12"`, and `daily_hours = 0`. The model run is a forward run (moving forward in time, set here using `backward_running = FALSE`) and not backwards (set with `backward_running = TRUE`). Essentially, running in forward mode means the starting location is a source of emissions; running backward means that the starting location is a receptor. This run has been set to be modelled for 24 h (`duration = 24`). The starting location of 42.83752ºN and 80.30364ºW is set using `lat = 42.83752` and `lon = -80.30364`; the starting height of 5 m above ground level is set by `height = 5`. The meteorological options include the type of met data to use (1º **GDAS** data is used here with `met_type = "gdas1"`--there is also the option to use NCEP reanalysis data with the `met_type = "reanalysis"` setting).

Remember those presets that were added earlier? They are called up in the `emissions`, `species`, and `grids` arguments. The `1` value provided for each of those corresponds to the first preset of each type of preset. If you ever need to remind yourself of which presets are currently in the system, use `dispersion_preset_list()` function. Moreover, that function has an interactive mode.

```R
dispersion_preset_list()
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

While adding presets is generally a good thing to do, there may come a point where you would like to delete some of the presets. This can be done with the `dispersion_preset_delete()` function. It can be done interactively, which is probably the safer method:

```R
dispersion_preset_delete()
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

After executing the `hysplit_dispersion()` function (and possibly waiting awhile, since met files will need to be downloaded), 24 CSV files with particle position will become available in the working directory:

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

One or more snapshot plots of the data can be generated using the `hysplit_dispersion_plot()` function. If a dispersion data frame is available, the function can be called to reference that data and generate particle plots at every hour of the model run:

```R
hysplit_dispersion_plot(
  hours = 'all',
  dispersion_df = dispersion_2012_03_12,
  map_type = "stamen",
  map_output_name = "map1")
```

If the dispersion data has been saved to disk (usually as the file `dispersion.csv` in a subfolder for the run), it's possible to point to that file in the `hysplit_dispersion_plot()` call:

```R
hysplit_dispersion_plot(
  hours = 'all',
  df_folder_path = paste0(getwd(), "dispersion--2014-08-03--13-42-28"),
  map_type = "stamen",
  map_output_name = "map2")
```
