SplitR
======

SplitR is a collection of functions to help conduct trajectory and dispersion modelling with [NOAA HYSPLIT](http://ready.arl.noaa.gov/HYSPLIT.php).

## Installation

Easy installation of SplitR from GitHub is possible with the `devtools` package:

```coffee
require(devtools)
install_github('SplitR', 'rich-iannone')
```

## Features

The basic ideas behind SplitR are to make the process of setting up and executing Hysplit runs very fast and easy, and, to directly use the output data in further analyses. To achieve these aims, SplitR can enforce a directory structure so that input files are reusable across SplitR projects and output files are organized in named project folders. Also, the framework allows for the automatic generation of input `CONTROL` files for continuous running (e.g., over one or more years, specific date ranges, etc.). The user doesn't really need to examine the series of input `CONTROL` files but the text are included in automatically generated log files for later inspection.