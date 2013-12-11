## Function to read in trajectory files

# Get file path for trajectory files
path_trajectory_files <- "~/Downloads/Hysplit4-OSX/output_trajectory/"

# Generate file list
trajectory_file_list <- list.files(path = path_trajectory_files)

# Initialize empty data frame with 12 named columns
traj.df <- setNames(data.frame(mat.or.vec(nr = 0, nc = 12)),
                    nm = c("receptor", "year", "month", "day",
                           "hour", "hour.inc", "lat", "lon", 
                           "height", "pressure", "date2", "date"))

# Make loop with all trajectory files
for (i in 1:length(trajectory_file_list)) {
trajectory.read <- function(path_trajectory_files){
  
  # For each trajectory file, read each line and determine where the variable-length
  # header ends
  column.widths <- c(92)
  traj_temp <- read.fwf(paste(path_trajectory_files, trajectory_file_list[i], sep = ''),
                        widths = column.widths)
  
  for (j in 1:nrow(traj_temp)) {
    if(length(grep("PRESSURE", traj_temp[j,1])) != 0) skip_up_to_line <- j
  }
  
  column.widths <- c(6, 6, 6, 6, 6, 6, 6, 6,
                     8, 9, 9, 9, 9)
  
  traj <- read.fwf(paste(path_trajectory_files, trajectory_file_list[i], sep = ''),
                   skip = skip_up_to_line,
                   widths = column.widths)
  names(traj) <- c("first", "receptor", "year", "month", "day", "hour", "zero1", "zero2", 
                   "hour.inc", "lat", "lon", "height", "pressure")
  traj$first <- NULL
  traj$zero1 <- NULL
  traj$zero2 <- NULL
  
  date2 <- mat.or.vec(nr = nrow(traj), nc = 1)
  for (k in 1:nrow(traj)) {
    date2[k] <- ISOdatetime(ifelse(traj[1,2] < 50, traj[1,2] + 2000, traj[1,2] + 1900),
                            traj[1,3], traj[1,4], traj[1,5], min = 0, sec = 0, tz = "GMT") +
      traj$hour.inc[k] * 3600}
  traj$date2 <- as.POSIXct(date2, origin = "1970-01-01", tz = "GMT")
  
  traj$date <- ISOdatetime(ifelse(traj[1,2] < 50, traj[1,2] + 2000, traj[1,2] + 1900),
                           traj[1,3], traj[1,4], traj[1,5], min = 0, sec = 0, tz = "GMT")
  
  # Continuously bind data frames together to make a large df with all trajectory  files
  traj.df <- rbind(traj.df, traj)
  
  # Close the trajectory file loop  
}
