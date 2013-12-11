## Function to read in trajectory files

#traj_london <- importTraj(site = "london", year = 2009, local = NA)
#head(traj_london, 50)


# receptor year month day hour hour.inc    lat    lon height pressure               date2
#        1 2009     1   1    9        0 51.500 -0.100   10.0   1020.5 2009-01-01 09:00:00
#        1 2009     1   1    8       -1 51.533  0.133    9.6   1021.2 2009-01-01 08:00:00
#        1 2009     1   1    7       -2 51.578  0.355    9.2   1021.7 2009-01-01 07:00:00
#        1 2009     1   1    6       -3 51.633  0.565    8.7   1022.2 2009-01-01 06:00:00
#        1 2009     1   1    5       -4 51.697  0.755    8.2   1022.6 2009-01-01 05:00:00
#        1 2009     1   1    4       -5 51.767  0.920    7.7   1022.9 2009-01-01 04:00:00
# date
# 2009-01-01 09:00:00
# 2009-01-01 09:00:00
# 2009-01-01 09:00:00
# 2009-01-01 09:00:00
# 2009-01-01 09:00:00
# 2009-01-01 09:00:00

column.widths <- c(6, 6, 6, 6, 6, 6, 6, 6,
                   8, 9, 9, 9, 9)

traj <- read.fwf("/Users/riannone/Downloads/Hysplit4-OSX/output_trajectory/traj(back)-05-04-08-03-lat_50.108_long_-122.942-height_2200-24h",
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



