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
    colors <- grey_pal(0.7, 0.1)(length(sort(unique(traj_df$date))))
  }
  
  traj_plot <- leaflet()
  
  traj_plot <- addTiles(traj_plot)
  
  traj_plot <- fitBounds(
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
                 " <font size=\"1\">m AGL</font><br>",
                 "<strong>terrain</strong> ", 
                 wind_trajectories_by_date[[i]][, 20],
                 " <font size=\"1\">m AMSL</font><br>",
                 "<strong>P</strong> ",
                 wind_trajectories_by_date[[i]][, 10],
                 " <font size=\"1\">hPa</font> / ",
                 "<strong>T</strong> ", 
                 wind_trajectories_by_date[[i]][, 14],
                 " K / <strong>RH</strong> ", 
                 wind_trajectories_by_date[[i]][, 17],
                 "%<br><strong>sp. humid.</strong> ", 
                 wind_trajectories_by_date[[i]][, 18],
                 " <font size=\"1\">g/kg</font> ",
                 "<strong>theta</strong> ", 
                 wind_trajectories_by_date[[i]][, 13],
                 " K<br><strong>rainfall</strong> ", 
                 wind_trajectories_by_date[[i]][, 15],
                 " / <strong>mixing depth</strong> ", 
                 wind_trajectories_by_date[[i]][, 16],
                 " m<br>")
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
        addCircleMarkers(
          traj_plot,
          wind_trajectories_by_date[[i]][,8],
          wind_trajectories_by_date[[i]][,7],
          radius = 1,
          fill = TRUE,
          fillOpacity = 1,
          opacity = 1,
          color = colors[i],
          fillColor = colors[i],
          popup = popup)
    }
  }
  
  for (i in 1:length(wind_trajectories_by_date)){
    traj_plot <-
      addPolylines(
        traj_plot,
        wind_trajectories_by_date[[i]][,8],
        wind_trajectories_by_date[[i]][,7],
        weight = 2,
        smoothFactor = 2,
        color = colors[i])
  }
  
  traj_plot
}
