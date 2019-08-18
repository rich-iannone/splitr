library(splitr)
library(tidyverse)
library(here)

# Dispersion tests
dispersion_model <-
  create_disp_model() %>%
  add_emissions(
    rate = 5,
    duration = 6,
    start_day = "2015-07-01",
    start_hour = 0
  ) %>%
  add_species(
    pdiam = 1,
    density = 1,
    shape_factor = 1
  ) %>%
  add_grid(
    lat = 43.45,
    lon = -79.70,
    height = 50,
    range = c(0.5, 0.5),
    division = c(0.1, 0.1)
  ) %>%
  add_params(
    duration = 24,
    start_day = "2015-07-01",
    start_hour = 0,
    direction = "forward",
    met_type = "reanalysis",
    met_dir = here::here("met"),
    exec_dir = here::here("out")
  ) %>%
  run_model()


