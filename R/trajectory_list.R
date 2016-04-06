#' List HYSPLIT trajectory output archive files
#' or folders
#' @description The function lists the HYSPLIT
#' trajectory output files that reside in a specified
#' directory.
#' @param output_folder the absolute path of the
#' directory containing the trajectory endpoints files
#' is to be provided.
#' @return data frame with information on trajectory
#' model output data archives
#' @export trajectory_list

trajectory_list <- function(output_folder) {
  
  if (get_os() == "mac") {
    file_list <- list.files(path = output_folder,
                            pattern = ".zip")
    trajectory_output_df <-
      as.data.frame(mat.or.vec(nr = length(file_list),
                               nc = 4))
  }
  
  if (get_os() == "win") {
    dir_list <- list.dirs(path = output_folder,
                          recursive = FALSE)
    trajectory_output_df <-
      as.data.frame(mat.or.vec(nr = length(dir_list),
                               nc = 4))
  }
  
  colnames(trajectory_output_df) <- 
    c("No.", "Name",
      "Creation Date and Time",
      "Data Count")
  
  for (i in 1:length(file_list)) {
    trajectory_output_df[i,1] <- i
    if (get_os() == "mac") {
      trajectory_output_df[i,2] <- 
        gsub("^([^--]+)--.*", "\\1", file_list[i])
      
      datetime <- 
        ISOdatetime(
          year = gsub("^.+--([0-9][0-9][0-9][0-9])-[0-9][0-9]-[0-9][0-9]-.*",
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
    }
    
    if (get_os() == "win") {
      trajectory_output_df[i,2] <-
        gsub("^([^--]+)--.*", "\\1", dir_list[i])
      
      datetime <- 
        ISOdatetime(
          year = gsub("^.+--([0-9][0-9][0-9][0-9])-[0-9][0-9]-[0-9][0-9]-.*",
                      "\\1", dir_list[i]),
          month = gsub("^.+--[0-9][0-9][0-9][0-9]-([0-9][0-9])-[0-9][0-9]-.*",
                       "\\1", dir_list[i]),
          day = gsub("^.+--[0-9][0-9][0-9][0-9]-[0-9][0-9]-([0-9][0-9])-.*",
                     "\\1", dir_list[i]),
          hour = gsub("^.+--.+--([0-9][0-9])-[0-9][0-9]-[0-9][0-9].zip",
                      "\\1", dir_list[i]),
          min = gsub("^.+--.+--[0-9][0-9]-([0-9][0-9])-[0-9][0-9].zip",
                     "\\1", dir_list[i]),
          sec = gsub("^.+--.+--[0-9][0-9]-[0-9][0-9]-([0-9][0-9]).zip",
                     "\\1", dir_list[i]),
          tz = "")
    }
    
    if (difftime(Sys.time(),
                 datetime,
                 units = "hours")[[1]] >= 24) {
      if (difftime(Sys.time(),
                   datetime,
                   units = "days")[[1]] %% 1 >= 0.8) {
        time_description <- 
          paste0("almost ",
                 ceiling(difftime(Sys.time(),
                                  datetime,
                                  units = "days")[[1]]),
                 " days ago")
      } else if (difftime(Sys.time(),
                          datetime,
                          units = "days")[[1]] %% 1 >= 0.2 &
                 difftime(Sys.time(),
                          datetime,
                          units = "days")[[1]] %% 1 < 0.8) {
        time_description <- 
          paste0("ca. ", 
                 floor(difftime(Sys.time(),
                                datetime,
                                units = "days")[[1]]),
                 " days ago")
      } else {
        time_description <- 
          paste0("just over ", 
                 floor(difftime(Sys.time(),
                                datetime,
                                units = "days")[[1]]),
                 " days ago")
      }
    } else if (difftime(Sys.time(),
                        datetime,
                        units = "hours")[[1]] < 24 &
               difftime(Sys.time(),
                        datetime,
                        units = "hours")[[1]] >= 1) {
      time_description <- 
        paste0("about ",
               floor(difftime(Sys.time(),
                              datetime,
                              units = "hours")[[1]]),
               " hours ago")
    } else if (difftime(Sys.time(),
                        datetime,
                        units = "hours")[[1]] < 1 &
               difftime(Sys.time(),
                        datetime,
                        units = "mins")[[1]] >= 1) {
      time_description <- 
        paste0("about ",
               floor(difftime(Sys.time(),
                              datetime,
                              units = "mins")[[1]]),
               " minutes ago")
    } else {
      time_description <- 
        paste0("less than 1 min ago")
    }
    
    trajectory_output_df[i,3] <- 
      paste0(as.character(datetime), " (",
             time_description, ")")
    
    rm(datetime, time_description)
    
    if (get_os() == "mac") {
      trajectory_output_df[i,4] <- 
        as.numeric(
          system(
            paste0("unzip -l ",
                   list.files(path = output_folder,
                              pattern = ".zip",
                              full.names = TRUE)[i],
                   " | wc -l"),
            intern = TRUE))
    }
  }
  
  return(trajectory_output_df)  
}
