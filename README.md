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

This package is in active development but is quite usable at present. Try it. Getting to a `Complete Hysplit` situation has never been this enjoyable before.

## Features

- automatic downloading and installation of HYSPLIT binary executables
- organization of modelling presets and output through the SplitR project framework
- automated retrieval of meteorological data files
- simple setup of trajectory and dispersion model runs (forward or backward)
- numerous batching options: set up models to run continuously with multiple start times throughout a single year, several years, or a predefined time range
- trajectory plotting and visualization of particle positions in dispersion runs
- clustering of trajectories and plotting of clusters

## Future Additions

- greater support for different types of meteorological data files
- merging of trajectories with pollutant concentration data for advanced air quality analyses
- statistical summaries for dispersion of particles
- ensemble model runs
- capability for the starting location to be moving during successive model runs
