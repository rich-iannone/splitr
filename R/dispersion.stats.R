dispersion.stats <- function(dispersion_df, stats){
  
  if ("fivenum" %in% stats){
    
    # Get five-number stats for heights at each hour
    for (i in 1:length(unique(dispersion_df$hour))){
      
      if (i == 1){
        hours <- sort(unique(dispersion_df$hour))
        stats_table <- as.data.frame(mat.or.vec(nr = 0, nc = 6))
        colnames(stats_table) <- c("min", "lower_hinge", "median",
                                   "upper_hinge", "max", "hour")
      }
      
      dispersion.hour <- subset(dispersion_df, hour == hours[i])
      stats_small_table <- as.data.frame(mat.or.vec(nr = 1, nc = 6))
      colnames(stats_small_table) <- c("min", "lower_hinge", "median",
                                       "upper_hinge", "max", "hour")
      
      stats_small_table$min <- fivenum(dispersion.hour$height, na.rm = TRUE)[1]
      stats_small_table$lower_hinge <- fivenum(dispersion.hour$height, na.rm = TRUE)[2]
      stats_small_table$median <- fivenum(dispersion.hour$height, na.rm = TRUE)[3]
      stats_small_table$upper_hinge <- fivenum(dispersion.hour$height, na.rm = TRUE)[4]
      stats_small_table$max <- fivenum(dispersion.hour$height, na.rm = TRUE)[5]
      stats_small_table$hour <- i
      
      stats_table <- rbind(stats_table, stats_small_table)
      
    }
    
    # Return the stats table containing the five number summary at every hour
    return(stats_table)
    
    stats_small_table$min <- fivenum(dispersion.height$height, na.rm = TRUE)[1]
    stats_small_table$lower_hinge <- fivenum(dispersion.height$height, na.rm = TRUE)[2]
    stats_small_table$median <- fivenum(dispersion.height$height, na.rm = TRUE)[3]
    stats_small_table$upper_hinge <- fivenum(dispersion.height$height, na.rm = TRUE)[4]
    stats_small_table$max <- fivenum(dispersion.height$height, na.rm = TRUE)[5]
    stats_small_table$hour <- i
    
    stats_table <- rbind(stats_table, stats_small_table)
    
  }
  
  # Return the stats table containing the five number summary at every hour
  return(stats_table)
  
}
