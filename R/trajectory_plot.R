#' Plot HYSPLIT trajectory model output onto a map
#'
#' The function plots modeled wind trajectories onto a map with information
#' provided at every air transport time interval.
#' @param x Either a trajectory data frame, typically created from use of the
#'   `hysplit_trajectory`, or a trajectory model object that contains output
#'   data (i.e., after executing model runs via the `run_model` function).
#' @param show_hourly An option to show hourly positions and associated data
#'   along trajectories.
#' @param color_scheme Defines the appearance of multiple trajectories in a
#'   single plot. Current options are `cycle_hues` (the default), and
#'   `increasingly_gray`.
#' @export
trajectory_plot <- function(x,
                            show_hourly = TRUE,
                            color_scheme = "cycle_hues") {
  
  if (inherits(x, "traj_model")) {
    if (!is.null(x$traj_df)) {
      traj_df <- x$traj_df
    } else {
      stop("There is no data available for plotting.")
    }
  }
  
  if (inherits(x, "data.frame")) {
    if (all(c("receptor", "year", "month", "day",
              "hour", "hour.inc", "lat", "lon",
              "height", "pressure", "date2",
              "date") %in% colnames(x))) {
      traj_df <- x
    } else {
      stop("This data frame does not contain plottable data.")
    }
  }
  
  if (color_scheme == "cycle_hues") {
    colors <- scales::hue_pal(c = 90, l = 70)(length(sort(unique(traj_df$date))))
  }
  
  if (color_scheme == "increasingly_gray") {
    colors <- scales::grey_pal(0.7, 0.1)(length(sort(unique(traj_df$date))))
  }
  
  # Correct longitude values near prime meridian
  traj_df$lon[which(traj_df$lon > 0)] <- 
    traj_df$lon[which(traj_df$lon > 0)] - (180*2)
  
  # Arrange the data in `traj_df`
  traj_df <-
    traj_df %>%
    dplyr::arrange(receptor, date, hour.inc) %>%
    dplyr::distinct()
  
  receptors <-
    traj_df[["receptor"]] %>%
    unique()
  
  dates <- 
    traj_df[["date"]] %>%
    sort() %>%
    unique()
  
  traj_plot <- 
    leaflet::leaflet() %>%
    leaflet::addProviderTiles(
      provider = "OpenStreetMap",
      group = "OpenStreetMap"
    ) %>%
    leaflet::addProviderTiles(
      provider = "CartoDB.DarkMatter",
      group = "CartoDB Dark Matter"
    ) %>%
    leaflet::addProviderTiles(
      provider = "CartoDB.Positron",
      group = "CartoDB Positron"
    ) %>%
    leaflet::addProviderTiles(
      provider = "Esri.WorldTerrain",
      group = "ESRI World Terrain"
    ) %>%
    leaflet::addProviderTiles(
      provider = "Stamen.Toner",
      group = "Stamen Toner"
    ) %>%
    leaflet::fitBounds(
      lng1 = min(traj_df[["lon"]]),
      lat1 = min(traj_df[["lat"]]),
      lng2 = max(traj_df[["lon"]]),
      lat2 = max(traj_df[["lat"]])
    ) %>%
    leaflet::addLayersControl(
      baseGroups = c(
        "CartoDB Positron", "CartoDB Dark Matter",
        "Stamen Toner", "ESRI World Terrain"
      ),
      overlayGroups = c("trajectory_points", "trajectory_paths"),
      position = "topright"
    )
  
  # Get different trajectories by site and by date
  for (i in seq_along(receptors)) {
    
    receptor_i <- receptors[i]
    
    for (j in seq_along(dates)) {
      
      date_i <- dates[j]
      
      wind_traj_ij <-
        traj_df %>%
        dplyr::filter(
          receptor == receptor_i,
          date == date_i
        ) %>%
        dplyr::arrange(hour.inc)
      
      #
      # Create circle markers
      #
      
      popup_circle <- 
        paste0(
          "<strong>trajectory</strong> ", wind_traj_ij[["date"]],
          "<br><strong>at time</strong> ", wind_traj_ij[["date2"]],
          " (", wind_traj_ij[["hour.inc"]],
          " h)<br><strong>height</strong> ", wind_traj_ij[["height"]],
          " <font size=\"1\">m AGL</font> / ",
          "<strong>P</strong> ", wind_traj_ij[["pressure"]],
          " <font size=\"1\">hPa</font>"
        )
      
      traj_plot <-
        traj_plot %>%
        leaflet::addPolylines(
          lng = wind_traj_ij[["lon"]],
          lat = wind_traj_ij[["lat"]],
          group = "trajectory_paths",
          weight = 2,
          smoothFactor = 1,
          color = colors[j]
        ) %>%
        leaflet::addCircles(
          lng = wind_traj_ij[["lon"]],
          lat = wind_traj_ij[["lat"]],
          group = "trajectory_points",
          radius = 250,
          fill = TRUE,
          color = colors[j],
          fillColor = colors[j], 
          popup = popup_circle
        )
    }
  }
  
  traj_plot
}
