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
  
}
