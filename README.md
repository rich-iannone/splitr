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

SplitR allows you set up and run HYSPLIT in a very fast, easy, and organized manner. You easily set up a few or, perhaps, thousands of trajectory or dispersion runs using a single function. Because SplitR is an R interface to HYSPLIT, we can take store output in data frames, analyze the output (e.g., perform cluster analyses, generate and save plots, etc.). The packages sets up the folders, gets the HYSPLIT binaries, automates the downloading and storage of meteorological data files, and encourages you to make presets for dispersion model runs.

This package is in active development but is quite usable at present. Getting to a `Complete Hysplit` situation has never been this enjoyable before.

## Features

- simple setup of trajectory and dispersion model runs (forward or backward)
- automatic downloading and installation of HYSPLIT binary executables
- retrieval of meteorological data files
- batching options: set and get model results for multiple start times through a single year, several years, or a defined time range
- trajectory plotting
- clustering of trajectories and plotting of clusters
- organization of modelling runs through the SplitR project framework

## Future Additions

- visualization of particle positions in dispersion runs
- statistical summaries for dispersion of particles
- greater support for different types of meteorological data files
- merging of trajectories with pollutant concentration data for advanced air quality analyses
- ensemble model runs
- capability for the starting location to be moving during successive model runs

