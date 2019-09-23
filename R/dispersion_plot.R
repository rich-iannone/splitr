#' Plot HYSPLIT dispersion model output onto a map
#'
#' The function plots modeled dispersion particles onto an interactive map.
#' 
#' @param x Either a dispersion data frame, typically created from use of the
#'   `hysplit_dispersion`, or a dispersion model object that contains
#'   output data (i.e., after executing model runs via the `run_model`
#'   function).
#' @param color_scheme Defines the appearance of multiple trajectories in a
#'   single plot. Current options are `cycle_hues` (the default), and
#'   `increasingly_gray`.
#'   
#' @export
dispersion_plot <- function(x,
                            color_scheme = "cycle_hues") {
  
  if (inherits(x, "dispersion_model")) {
    if (!is.null(x$disp_df)) {
      disp_df <- x$disp_df
    } else {
      stop("There is no data available for plotting.",
           call. = FALSE)
    }
  }
  
  if (inherits(x, "data.frame")) {
    if (all(c("particle_i", "hour", "lat", "lon", "height") %in% colnames(x))) {
      disp_df <- x
    } else {
      stop("This data frame does not contain plottable data.",
           call. = FALSE)
    }
  }
  
  if (color_scheme == "cycle_hues") {
    colors <- 
      scales::hue_pal(c = 90, l = 70)(
        length(sort(unique(disp_df$hour)))
      )
  }
  
  if (color_scheme == "increasingly_gray") {
    colors <-
      scales::grey_pal(0.7, 0.1)(
        length(sort(unique(disp_df$hour)))
      )
  }
  
  hours <-
    disp_df %>%
    dplyr::pull(hour) %>%
    unique()
  
  disp_plot <- 
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
      lng1 = min(disp_df[["lon"]]),
      lat1 = min(disp_df[["lat"]]),
      lng2 = max(disp_df[["lon"]]),
      lat2 = max(disp_df[["lat"]])
    )
  
  # Get different particle plots by hour of transport
  for (i in seq_along(hours)) {
    
    hour_i <- hours[i]
    
    if (i == 1) {
      groups <- vector("character")
    }
    
    # Create the groups vector
    groups <- c(groups, paste0("Hour ", i))
    
    particles_i <-
      disp_df %>%
      dplyr::filter(hour == hour_i)
    
    # Add 'CircleMarkers' for each hour
    disp_plot <-
      leaflet::addCircleMarkers(
        map = disp_plot,
        lng = particles_i[["lon"]],
        lat = particles_i[["lat"]],
        group = groups[i],
        radius = 1,
        stroke = FALSE,
        fill = TRUE,
        color = colors[i],
        fillColor = colors[i]
      )
  }
  
  disp_plot <-
    leaflet::addLayersControl(
      disp_plot,
      position = "topright",
      baseGroups = c(
        "CartoDB Positron",
        "CartoDB Dark Matter",
        "Stamen Toner",
        "ESRI World Terrain"
      ),
      overlayGroups = groups
    )
  
  disp_plot
}
