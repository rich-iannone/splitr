## Function to read in trajectory files


column.widths <- c(6, 6, 6, 6, 6, 6, 6, 6,
                   8, 9, 9, 9, 9)

traj <- read.fwf("~/Downloads/Hysplit4-OSX/output_trajectory/traj(back)-05-04-08-03-lat_50.108_long_-122.942-height_2200-24h",
                 skip = 7,
                 column.widths)
names(traj) <- c("first", "receptor", "year", "month", "day", "hour", "zero1", "zero2", 
                 "hour.inc", "lat", "lon", "height", "pressure")
traj$first <- NULL
traj$zero1 <- NULL
traj$zero2 <- NULL


date2 <- mat.or.vec(nr = nrow(traj), nc = 1)
for (i in 1:nrow(traj)) {
date2[i] <- ISOdatetime(ifelse(traj[1,2] < 50, traj[1,2] + 2000, traj[1,2] + 1900),
                     traj[1,3], traj[1,4], traj[1,5], min = 0, sec = 0, tz = "GMT") +
            traj$hour.inc[i] * 3600}
traj$date2 <- as.POSIXct(date2, origin = "1970-01-01", tz = "GMT")



# Get file path for trajectory files
path_trajectory_files <- "~/Downloads/Hysplit4-OSX/output_trajectory/"

}
