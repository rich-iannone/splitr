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

The basic idea behind SplitR is to make the process of setting up and executing HYSPLIT runs very fast and easy. Also, it would be great to use the output data in further analyses. The framework allows for the automatic generation of input `CONTROL` files for continuous running (e.g., over one or more years, specific date ranges, etc.). Getting to a `Complete Hysplit` situation has never been this enjoyable before.

## Features

- simple setup of trajectory model runs (forward or backward)
- batching options: set and get model results for multiple start times through a single year, several years, or a defined time range
- trajectory plotting
- clustering of trajectories and plotting of clusters
- organization of modelling runs through the SplitR project framework

## Future Additions

- capability for the starting location to be moving during successive model runs
- retrieval of the necessary meteorological data files
- merging of trajectories with pollutant concentration data for advanced air quality analyses
- dispersion model runs
- visualization of dispersion analyses
- parallelization of trajectory and dispersion runs

