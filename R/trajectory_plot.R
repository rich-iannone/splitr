#' Plot HYSPLIT trajectory model output onto a map
#' @description The function plots modeled wind
#' trajectories onto a map with information provided
#' at every air transport time interval.
#' @param traj_df a trajectory data frame, typically
#' created from use of the \code{hysplit_trajectory}
#' function with the value for \code{return_traj_df}
#' set to \code{TRUE}.
#' @param show_hourly an option to show hourly
#' positions and associated data along trajectories.
#' @param color_scheme defines the appearance of
#' multiple trajectories in a single plot. Current
#' options are \code{cycle_hues} (the default), and
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
  
  # Get different trajectories by site and by date
  for (i in 1:length(sort(unique(traj_df$receptor)))){
    
    if (i == 1){
      sorted_sites <- sort(unique(traj_df$receptor))
      wind_traj_by_site_date <- list()
    }
    
    for (j in 1:length(sort(unique(traj_df$date)))){
      
      if (j == 1){
        sorted_dates <- sort(unique(traj_df$date))
        wind_traj_by_date <- list()
      }
      
      wind_traj_by_date[[j]] <-
        subset(
          traj_df,
          receptor == sorted_sites[i] &
            date == sorted_dates[j])[
              order(subset(
                traj_df,
                receptor == sorted_sites[i] &
                  date == sorted_dates[j])[,6]),]
    }
    
    wind_traj_by_site_date[[i]] <- wind_traj_by_date
  }
  
  if (length(wind_traj_by_site_date) == 1){
    
    if (show_hourly){
      
      # Add CircleMarkers for each trajectory
      for (i in 1:length(wind_traj_by_site_date[[1]])){
        
        if (ncol(wind_traj_by_site_date[[1]][[i]]) == 21){
          popup <- 
            paste0("<strong>trajectory</strong> ",
                   wind_traj_by_site_date[[1]][[i]][, 12],
                   "<br><strong>at time</strong> ",
                   wind_traj_by_site_date[[1]][[i]][, 11],
                   " (",
                   wind_traj_by_site_date[[1]][[i]][, 6],
                   " h)<br><strong>height</strong> ",
                   wind_traj_by_site_date[[1]][[i]][, 9],
                   " <font size=\"1\">m AGL</font> / ",
                   "<strong>terrain</strong> ", 
                   wind_traj_by_site_date[[1]][[i]][, 20],
                   " <font size=\"1\">m AMSL</font><br>",
                   "<strong>P</strong> ",
                   wind_traj_by_site_date[[1]][[i]][, 10],
                   " <font size=\"1\">hPa</font> / ",
                   "<strong>RH</strong> ", 
                   wind_traj_by_site_date[[1]][[i]][, 17],
                   "% / <strong>SH</strong> ", 
                   wind_traj_by_site_date[[1]][[i]][, 18],
                   " <font size=\"1\">g/kg</font><br>",
                   "<strong>rainfall</strong> ", 
                   wind_traj_by_site_date[[1]][[i]][, 15],
                   " <font size=\"1\">mm/h</font> ",
                   "/ <strong>MH</strong> ", 
                   wind_traj_by_site_date[[1]][[i]][, 16],
                   " m<br>",
                   "<strong>T<sub>amb</sub></strong> ", 
                   wind_traj_by_site_date[[1]][[i]][, 14],
                   " K / <strong>T<sub>pot</sub></strong> ", 
                   wind_traj_by_site_date[[1]][[i]][, 13],
                   " K<br>")
        }
        
        if (ncol(wind_traj_by_site_date[[1]][[i]]) == 12){
          popup <- 
            paste0("<strong>trajectory</strong> ",
                   wind_traj_by_site_date[[1]][[i]][, 12],
                   "<br><strong>at time</strong> ",
                   wind_traj_by_site_date[[1]][[i]][, 11],
                   " (",
                   wind_traj_by_site_date[[1]][[i]][, 6],
                   " h)<br><strong>height</strong> ",
                   wind_traj_by_site_date[[1]][[i]][, 9],
                   " <font size=\"1\">m AGL</font> / ",
                   "<strong>P</strong> ",
                   wind_traj_by_site_date[[1]][[i]][, 10],
                   " <font size=\"1\">hPa</font>")
        }
        
        traj_plot <-
          addCircles(
            traj_plot,
            wind_traj_by_site_date[[1]][[i]][,8],
            wind_traj_by_site_date[[1]][[i]][,7],
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
    for (i in 1:length(wind_traj_by_site_date[[1]])){
      
      popup <- 
        paste0(
          "<strong>trajectory</strong> ",
          unique(wind_traj_by_site_date[[1]][[i]][, 12]),
          "<br><strong>total duration</strong> ",
          length(wind_traj_by_site_date[[1]][[i]][, 6]) - 
            ifelse(wind_traj_by_site_date[[1]][[i]][, 6] < 0 &
                     wind_traj_by_site_date[[1]][[i]][, 6] > 0,
                   2, 1),
          " h<br>")
      
      traj_plot <-
        addPolylines(
          traj_plot,
          wind_traj_by_site_date[[1]][[i]][,8],
          wind_traj_by_site_date[[1]][[i]][,7],
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
  
  if (length(wind_traj_by_site_date) > 1){
    
    if (show_hourly){
      
      # Add CircleMarkers for each trajectory
      for (i in 1:length(wind_traj_by_site_date)){
        for (j in 1:length(wind_traj_by_site_date[[i]])){
          
          if (ncol(wind_traj_by_site_date[[i]][[j]]) == 21){
            popup <- 
              paste0("<strong>receptor</strong> ",
                     wind_traj_by_site_date[[i]][[j]][, 1],
                     "<br><strong>trajectory</strong> ",
                     wind_traj_by_site_date[[i]][[j]][, 12],
                     "<br><strong>at time </strong> ",
                     wind_traj_by_site_date[[i]][[j]][, 11],
                     " (",
                     wind_traj_by_site_date[[i]][[j]][, 6],
                     " h)<br><strong>height</strong> ",
                     wind_traj_by_site_date[[i]][[j]][, 9],
                     " <font size=\"1\">m AGL</font> / ",
                     "<strong>terrain</strong> ", 
                     wind_traj_by_site_date[[i]][[j]][, 20],
                     " <font size=\"1\">m AMSL</font><br>",
                     "<strong>P</strong> ",
                     wind_traj_by_site_date[[i]][[j]][, 10],
                     " <font size=\"1\">hPa</font> / ",
                     "<strong>RH</strong> ", 
                     wind_traj_by_site_date[[i]][[j]][, 17],
                     "% / <strong>SH</strong> ", 
                     wind_traj_by_site_date[[i]][[j]][, 18],
                     " <font size=\"1\">g/kg</font><br>",
                     "<strong>rainfall</strong> ", 
                     wind_traj_by_site_date[[i]][[j]][, 15],
                     " <font size=\"1\">mm/h</font> ",
                     "/ <strong>MH</strong> ", 
                     wind_traj_by_site_date[[i]][[j]][, 16],
                     " m<br>",
                     "<strong>T<sub>amb</sub></strong> ", 
                     wind_traj_by_site_date[[i]][[j]][, 14],
                     " K / <strong>T<sub>pot</sub></strong> ", 
                     wind_traj_by_site_date[[i]][[j]][, 13],
                     " K<br>")
          }
          
          if (ncol(wind_traj_by_site_date[[i]][[j]]) == 12){
            popup <- 
              paste0("<strong>receptor</strong> ",
                     wind_traj_by_site_date[[i]][[j]][, 1],
                     "<br><strong>trajectory</strong> ",
                     wind_traj_by_site_date[[i]][[j]][, 12],
                     "<br><strong>at time </strong> ",
                     wind_traj_by_site_date[[i]][[j]][, 11],
                     " (",
                     wind_traj_by_site_date[[i]][[j]][, 6],
                     " h)<br><strong>height</strong> ",
                     wind_traj_by_site_date[[i]][[j]][, 9],
                     " <font size=\"1\">m AGL</font> / ",
                     "<strong>P</strong> ",
                     wind_traj_by_site_date[[i]][[j]][, 10],
                     " <font size=\"1\">hPa</font>")
          }
          
          traj_plot <-
            addCircles(
              traj_plot,
              wind_traj_by_site_date[[i]][[j]][,8],
              wind_traj_by_site_date[[i]][[j]][,7],
              group = "trajectory_points",
              radius = 500,
              fill = TRUE,
              fillOpacity = 1,
              opacity = 1,
              color = colors[j],
              fillColor = colors[j],
              popup = popup)
        }
      }
    }
    
    # Create polylines for trajectory paths
    for (i in 1:length(wind_traj_by_site_date)){
      for (j in 1:length(wind_traj_by_site_date[[i]])){
        
        popup <- 
          paste0(
            "<strong>receptor ",
            wind_traj_by_site_date[[i]][[j]][, 1],
            "<br><strong>trajectory </strong> ",
            unique(wind_traj_by_site_date[[i]][[j]][, 12]),
            "<br><strong>total duration: </strong> ",
            length(wind_traj_by_site_date[[i]][[j]][, 6]) - 
              ifelse(wind_traj_by_site_date[[i]][[j]][, 6] < 0 &
                       wind_traj_by_site_date[[i]][[j]][, 6] > 0,
                     2, 1),
            " h<br>")
        
        traj_plot <-
          addPolylines(
            traj_plot,
            wind_traj_by_site_date[[i]][[j]][,8],
            wind_traj_by_site_date[[i]][[j]][,7],
            group = "trajectory_paths",
            weight = 2,
            smoothFactor = 1,
            color = colors[j],
            popup = popup)
      }
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
}
