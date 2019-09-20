library(splitr)
library(tidyverse)
library(here)

# Dispersion tests

dispersion_model <-
  create_dispersion_model() %>%
  add_source(
    name = "particle",
    lat = 49.0, lon = -123.0, height = 50,
    pdiam = 15, density = 1.5, shape_factor = 0.8,
    rate = 10,
    release_start = lubridate::ymd_hm("2015-07-01 00:00"),
    release_end = lubridate::ymd_hm("2015-07-01 00:00") + 
      lubridate::hours(2)
  ) %>%
  add_dispersion_params(
    start_time = lubridate::ymd_hm("2015-07-01 00:00"),
    end_time = lubridate::ymd_hm("2015-07-01 00:00") + 
      lubridate::hours(6),
    direction = "forward", 
    met_type = "reanalysis",
    met_dir = here::here("met"),
    exec_dir = here::here("out")
  ) %>%
  run_model()
