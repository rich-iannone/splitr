#' List HYSPLIT trajectory output archive files or folders
#' @description The function lists the HYSPLIT trajectory output files that reside in a specified directory.
#' @param output_folder the absolute path for the trajectory archive files (UNIX) or folders (Windows) is to be provided.
#' @return data frame with information on trajectory model output data archives
#' @export trajectory.list


trajectory.list <- function(output_folder){
  
  output_folder <- "~/Documents/SplitR/Output"
  
  if (.Platform$OS.type == "unix"){
    
    file_list <- list.files(path = output_folder, pattern = ".zip")
    
  }
  
  if (.Platform$OS.type == "windows"){
    
    dir_list <- list.dirs(path = output_folder, recursive = FALSE)
    
  }
  
  trajectory_output_df <- as.data.frame(mat.or.vec(nr = length(file_list), nc = 6))
  colnames(trajectory_output_df) <- c("No.", "Name", "Creation Date and Time",
                                      "Data Count", "Location", "Height, m")
  
  for (i in 1:length(file_list)){
    
    trajectory_output_df[i,1] <- i
    
    trajectory_output_df[i,2] <- gsub("^([^--]+)--.*", "\\1", file_list[i])
    
    
    datetime <- ISOdatetime(year = gsub("^.+--([0-9][0-9][0-9][0-9])-[0-9][0-9]-[0-9][0-9]-.*",
                                        "\\1", file_list[i]),
                            month = gsub("^.+--[0-9][0-9][0-9][0-9]-([0-9][0-9])-[0-9][0-9]-.*",
                                         "\\1", file_list[i]),
                            day = gsub("^.+--[0-9][0-9][0-9][0-9]-[0-9][0-9]-([0-9][0-9])-.*",
                                       "\\1", file_list[i]),
                            hour = gsub("^.+--.+--([0-9][0-9])-[0-9][0-9]-[0-9][0-9].zip",
                                        "\\1", file_list[i]),
                            min = gsub("^.+--.+--[0-9][0-9]-([0-9][0-9])-[0-9][0-9].zip",
                                       "\\1", file_list[i]),
                            sec = gsub("^.+--.+--[0-9][0-9]-[0-9][0-9]-([0-9][0-9]).zip",
                                       "\\1", file_list[i]),
                            tz = "")
    
    if (difftime(Sys.time(), datetime, units = "hours")[[1]] >= 24){
      
      if (difftime(Sys.time(), datetime, units = "days")[[1]] %% 1 >= 0.8){
        time_description <- paste("almost ",
                                  ceiling(difftime(Sys.time(), datetime, units = "days")[[1]]),
                                  " days ago", sep = '')
      } else if (difftime(Sys.time(), datetime, units = "days")[[1]] %% 1 >= 0.2 &
                   difftime(Sys.time(), datetime, units = "days")[[1]] %% 1 < 0.8){
        time_description <- paste("approx ", 
                                  floor(difftime(Sys.time(), datetime, units = "days")[[1]]),
                                  " days ago", sep = '')
      } else {
        time_description <- paste("just over ", 
                                  floor(difftime(Sys.time(), datetime, units = "days")[[1]]),
                                  " days ago", sep = '')
      }
      
    } else if (difftime(Sys.time(), datetime, units = "hours")[[1]] < 24 &
                 difftime(Sys.time(), datetime, units = "hours")[[1]] >= 1){
      time_description <- paste("about ",
                                floor(difftime(Sys.time(), datetime, units = "hours")[[1]]),
                                " hours ago", sep = '')
    } else if (difftime(Sys.time(), datetime, units = "hours")[[1]] < 1 &
                 difftime(Sys.time(), datetime, units = "mins")[[1]] >= 1){
      time_description <- paste("about ",
                                floor(difftime(Sys.time(), datetime, units = "mins")[[1]]),
                                " minutes ago", sep = '')
    } else {
      time_description <- paste("less than 1 min ago", sep = '')
    }
    
    trajectory_output_df[i,3] <- paste(as.character(datetime), " (",
                                       time_description, ")", sep = '')
    
    rm(datetime, time_description)
    
}

