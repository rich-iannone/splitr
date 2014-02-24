SplitR
======

SplitR is a collection of functions to help conduct trajectory and dispersion modelling with [NOAA HYSPLIT](http://ready.arl.noaa.gov/HYSPLIT.php).

## Installation

Easy installation of SplitR from GitHub is entirely possible with the `devtools` package:

```coffee
require(devtools)
install_github('SplitR', 'rich-iannone')
```

## Description

The basic idea behind SplitR is to make the process of setting up and executing HYSPLIT runs very fast and easy. Because these runs are being done in R, further analyses of the output data is possible. The framework allows for the automatic generation of input `CONTROL` files for continuous running of trajectory or dispersion runs (e.g., over one or more years, specific date ranges, etc.). Getting to a `Complete Hysplit` situation has never been this enjoyable before.

## Features

- simple setup of trajectory and dispersion model runs (forward or backward)
- automatic downloading and installation of HYSPLIT binary executables
- batching options: set and get model results for multiple start times through a single year, several years, or a defined time range
- trajectory plotting
- clustering of trajectories and plotting of clusters
- organization of modelling runs through the SplitR project framework

## Future Additions

- visualization of dispersion analyses
- retrieval of necessary meteorological data files
- greater support for different types of meteorological data files
- merging of trajectories with pollutant concentration data for advanced air quality analyses
- ensemble model runs
- capability for the starting location to be moving during successive model runs
- parallelization of models runs

