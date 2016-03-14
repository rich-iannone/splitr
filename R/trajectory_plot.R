#' Plot HYSPLIT trajectory model output onto a map
#' @description The function plots modeled wind
#' trajectories onto a map with information provided
#' at every hour of air transport.
#' @param traj_df a trajectory data frame.
#' @param show_hourly an option to show hourly
#' positions and associated data along trajectories.
#' @param color_scheme defines the appearance of
#' multiple trajectories in a single plot. Options are
#' \code{cycle_hues} (the default), and
#' \code{increasingly_gray}.
#' @import leaflet
#' @import scales
#' @export trajectory_plot

trajectory_plot <- function(traj_df,
                            show_hourly = TRUE,
                            color_scheme = "cycle_hues"){
  
  if (color_scheme == "cycle_hues"){
    colors <- 
      hue_pal(c = 90, l = 70)(
        length(sort(unique(traj_df$date))))
  }
  
  if (color_scheme == "increasingly_gray"){
    colors <-
      grey_pal(0.7, 0.1)(length(sort(unique(traj_df$date))))
  }
  
  # Correct longitude values near prime meridian
  traj_df$lon[which(traj_df$lon > 0)] <- 
    traj_df$lon[which(traj_df$lon > 0)] - (180*2)
  
  traj_plot <- leaflet()
  
  #traj_plot <- addTiles(traj_plot)
  
  traj_plot <- 
    addProviderTiles(
      traj_plot,
      "OpenStreetMap",
      group = "OpenStreetMap") 
  
  traj_plot <-
    addProviderTiles(
      traj_plot,
      "CartoDB.DarkMatter",
      group = "CartoDB Dark Matter")
  
  traj_plot <-
    addProviderTiles(
      traj_plot,
      "CartoDB.Positron",
      group = "CartoDB Positron")
  
  traj_plot <- 
    addProviderTiles(
      traj_plot,
      "Esri.WorldTerrain",
      group = "ESRI World Terrain")
  
  traj_plot <-
    addProviderTiles(
      traj_plot,
      "Stamen.Toner",
      group = "Stamen Toner")
  
  traj_plot <- 
    fitBounds(
      traj_plot,
      min(traj_df$lon),
      min(traj_df$lat),
      max(traj_df$lon),
      max(traj_df$lat))
  
  # Get different trajectories by date
  for (i in 1:length(sort(unique(traj_df$date)))){
    if (i == 1){
      sorted_dates <- sort(unique(traj_df$date))
      wind_trajectories_by_date <- list()
    }
    
    wind_trajectories_by_date[[i]] <-
      subset(traj_df, date == sorted_dates[i])
    
    wind_trajectories_by_date[[i]] <-
      wind_trajectories_by_date[[i]][
        order(wind_trajectories_by_date[[i]][,6]),]
  }
  
  if (show_hourly){
    
    # Add CircleMarkers for each trajectory
    for (i in 1:length(wind_trajectories_by_date)){
      
      if (ncol(wind_trajectories_by_date[[i]]) == 21){
        popup <- 
          paste0("<strong>trajectory </strong> ",
                 wind_trajectories_by_date[[i]][, 12],
                 "<br><strong>at time </strong> ",
                 wind_trajectories_by_date[[i]][, 11],
                 " (",
                 wind_trajectories_by_date[[i]][, 6],
                 " h)<br><strong>height</strong> ",
                 wind_trajectories_by_date[[i]][, 9],
                 " <font size=\"1\">m AGL</font> / ",
                 "<strong>terrain</strong> ", 
                 wind_trajectories_by_date[[i]][, 20],
                 " <font size=\"1\">m AMSL</font><br>",
                 "<strong>P</strong> ",
                 wind_trajectories_by_date[[i]][, 10],
                 " <font size=\"1\">hPa</font> / ",
                 "<strong>RH</strong> ", 
                 wind_trajectories_by_date[[i]][, 17],
                 "% / <strong>SH</strong> ", 
                 wind_trajectories_by_date[[i]][, 18],
                 " <font size=\"1\">g/kg</font><br>",
                 "<strong>rainfall</strong> ", 
                 wind_trajectories_by_date[[i]][, 15],
                 " <font size=\"1\">mm/h</font> ",
                 "/ <strong>MH</strong> ", 
                 wind_trajectories_by_date[[i]][, 16],
                 " m<br>",
                 "<strong>T<sub>amb</sub></strong> ", 
                 wind_trajectories_by_date[[i]][, 14],
                 " K / <strong>T<sub>pot</sub></strong> ", 
                 wind_trajectories_by_date[[i]][, 13],
                 " K<br>")
      }
      
      if (ncol(wind_trajectories_by_date[[i]]) == 12){
        popup <- 
          paste0("<strong>trajectory </strong> ",
                 wind_trajectories_by_date[[i]][, 12],
                 "<br><strong>at time </strong> ",
                 wind_trajectories_by_date[[i]][, 11],
                 " (",
                 wind_trajectories_by_date[[i]][, 6],
                 " h)<br><strong>height</strong> ",
                 wind_trajectories_by_date[[i]][, 9],
                 " <font size=\"1\">m AGL</font> / ",
                 "<strong>P</strong> ",
                 wind_trajectories_by_date[[i]][, 10],
                 " <font size=\"1\">hPa</font>")
      }
      
      traj_plot <-
        addCircles(
          traj_plot,
          wind_trajectories_by_date[[i]][,8],
          wind_trajectories_by_date[[i]][,7],
          group = "trajectory_points",
          radius = 500,
          fill = TRUE,
          fillOpacity = 1,
          opacity = 1,
          color = colors[i],
          fillColor = colors[i],
          popup = popup)
    }
  }
  
  # Create polylines for trajectory paths
  for (i in 1:length(wind_trajectories_by_date)){
    
    popup <- 
      paste0("<strong>trajectory </strong> ",
             unique(wind_trajectories_by_date[[i]][, 12]),
             "<br><strong>total duration: </strong> ",
             length(wind_trajectories_by_date[[i]][, 6]) - 1,
             " h<br>")
    
    traj_plot <-
      addPolylines(
        traj_plot,
        wind_trajectories_by_date[[i]][,8],
        wind_trajectories_by_date[[i]][,7],
        group = "trajectory_paths",
        weight = 2,
        smoothFactor = 1,
        color = colors[i],
        popup = popup)
  }
  
  traj_plot <-
    addLayersControl(
      traj_plot,
      position = "topright",
      baseGroups = c("CartoDB Positron",
                     "CartoDB Dark Matter",
                     "Stamen Toner",
                     "ESRI World Terrain"),
      overlayGroups = c("trajectory_points",
                        "trajectory_paths"))
  
  traj_plot
}
