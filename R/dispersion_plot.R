#' Plot HYSPLIT dispersion model output onto a map
#' @description The function plots modeled dispersion
#' particles onto a map with information provided
#' at time interval.
#' @param disp_df a dispersion data frame, typically
#' created from use of the \code{hysplit_dispersion}
#' function with the value for \code{return_disp_df}
#' set to \code{TRUE}.
#' @param color_scheme defines the appearance of
#' multiple trajectories in a single plot. Current
#' options are \code{cycle_hues} (the default), and
#' \code{increasingly_gray}.
#' @import leaflet
#' @import scales
#' @export dispersion_plot

dispersion_plot <- function(disp_df,
                            color_scheme = "cycle_hues"){
  
  if (color_scheme == "cycle_hues"){
    colors <- 
      hue_pal(c = 90, l = 70)(
        length(sort(unique(disp_df$hour))))
  }
  
  if (color_scheme == "increasingly_gray"){
    colors <-
      grey_pal(0.7, 0.1)(length(sort(unique(disp_df$hour))))
  }
  
  disp_plot <- leaflet()
  
  disp_plot <- 
    addProviderTiles(
      disp_plot,
      "OpenStreetMap",
      group = "OpenStreetMap") 
  
  disp_plot <-
    addProviderTiles(
      disp_plot,
      "CartoDB.DarkMatter",
      group = "CartoDB Dark Matter")
  
  disp_plot <-
    addProviderTiles(
      disp_plot,
      "CartoDB.Positron",
      group = "CartoDB Positron")
  
  disp_plot <- 
    addProviderTiles(
      disp_plot,
      "Esri.WorldTerrain",
      group = "ESRI World Terrain")
  
  disp_plot <-
    addProviderTiles(
      disp_plot,
      "Stamen.Toner",
      group = "Stamen Toner")
  
  disp_plot <- 
    fitBounds(
      disp_plot,
      min(disp_df$lon),
      min(disp_df$lat),
      max(disp_df$lon),
      max(disp_df$lat))
  
  # Get different particle plots by hour of transport
  for (i in 1:length(sort(unique(disp_df$hour)))){
    
    if (i == 1){
      groups <- vector("character")
    }
    
    # Create the groups vector
    groups <- c(groups, paste0("Hour ", i))
    
    # Add CircleMarkers for each hour
    disp_plot <-
      addCircleMarkers(
        disp_plot,
        subset(disp_df,
               hour == sort(unique(disp_df$hour))[i])[, 2],
        subset(disp_df,
               hour == sort(unique(disp_df$hour))[i])[, 3],
        group = groups[i],
        radius = 1,
        stroke = FALSE,
        fill = TRUE,
        color = colors[i],
        fillColor = colors[i])
  }
  
  disp_plot <-
    addLayersControl(
      disp_plot,
      position = "topright",
      baseGroups = c("CartoDB Positron",
                     "CartoDB Dark Matter",
                     "Stamen Toner",
                     "ESRI World Terrain"),
      overlayGroups = groups)
  
  disp_plot
}
