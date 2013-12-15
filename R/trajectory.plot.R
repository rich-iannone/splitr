trajectory.plot <- function(traj.df = "traj.df",
                            lon = "lon",
                            lat = "lat",
                            pollutant = "height", 
                            type = "default",
                            smooth = FALSE,
                            statistic = "mean",
                            percentile = 90, 
                            map = TRUE,
                            lon.inc = 1,
                            lat.inc = 1,
                            min.bin = 1,
                            group = NA, 
                            map.fill = TRUE,
                            map.res = "default",
                            map.cols = "grey40", 
                            map.alpha = 0.4,
                            ...){
  
  # Include require statements for 'maps' and 'mapdata' packages
  require(maps)
  require(mapdata)
  require(lattice)
  require(RColorBrewer)
  
  # Include dateTypes object
  dateTypes <- c("year", "hour", "month", "season", "weekday", "weekend",
                 "monthyear", "gmtbst", "bstgmt", "daylight")
  
  # Include quckText function
  quickText <- function(text, auto.text = TRUE) {
    if (!auto.text) 
      return(ans <- text)
    if (is.expression(text)) 
      return(ans <- text)
    ans <- paste("expression(paste('", text, " ", sep = "")
    ans <- gsub("NO2", "' 'NO' [2] * '", ans)
    ans <- gsub("no2", "' 'NO' [2] * '", ans)
    ans <- gsub("NOX", "' 'NO' [x] * '", ans)
    ans <- gsub("nox", "' 'NO' [x] * '", ans)
    ans <- gsub("NOx", "' 'NO' [x] * '", ans)
    ans <- gsub("NH3", "' 'NH' [3] * '", ans)
    ans <- gsub("nh3", "' 'NH' [3] * '", ans)
    ans <- gsub("co ", "' 'CO ' '", ans)
    ans <- gsub("co,", "' 'CO,' '", ans)
    ans <- gsub("nmhc", "' 'NMHC' '", ans)
    ans <- gsub("ws", "' 'wind spd.' '", ans)
    ans <- gsub("wd", "' 'wind dir.' '", ans)
    ans <- gsub("rh ", "' 'relative humidity' '", ans)
    ans <- gsub("PM10", "' 'PM' [10] * '", ans)
    ans <- gsub("pm10", "' 'PM' [10] * '", ans)
    ans <- gsub("pm1", "' 'PM' [1] * '", ans)
    ans <- gsub("PM1", "' 'PM' [1] * '", ans)
    ans <- gsub("pmc", "' 'PM' [coarse] * '", ans)
    ans <- gsub("pmcoarse", "' 'PM' [coarse] * '", ans)
    ans <- gsub("PMc", "' 'PM' [coarse] * '", ans)
    ans <- gsub("PMcoarse", "' 'PM' [coarse] * '", ans)
    ans <- gsub("pmf", "' 'PM' [fine] * '", ans)
    ans <- gsub("pmfine", "' 'PM' [fine] * '", ans)
    ans <- gsub("PMf", "' 'PM' [fine] * '", ans)
    ans <- gsub("PMfine", "' 'PM' [fine] * '", ans)
    ans <- gsub("PM2.5", "' 'PM' [2.5] * '", ans)
    ans <- gsub("pm2.5", "' 'PM' [2.5] * '", ans)
    ans <- gsub("pm25", "' 'PM' [2.5] * '", ans)
    ans <- gsub("PM2.5", "' 'PM' [2.5] * '", ans)
    ans <- gsub("PM25", "' 'PM' [2.5] * '", ans)
    ans <- gsub("pm25", "' 'PM' [2.5] * '", ans)
    ans <- gsub("O3", "' 'O' [3] * '", ans)
    ans <- gsub("o3", "' 'O' [3] * '", ans)
    ans <- gsub("ozone", "' 'O' [3] * '", ans)
    ans <- gsub("CO2", "' 'CO' [2] * '", ans)
    ans <- gsub("co2", "' 'CO' [2] * '", ans)
    ans <- gsub("SO2", "' 'SO' [2] * '", ans)
    ans <- gsub("so2", "' 'SO' [2] * '", ans)
    ans <- gsub("H2S", "' 'H' [2] * 'S''", ans)
    ans <- gsub("h2s", "' 'H' [2] * 'S''", ans)
    ans <- gsub("CH4", "' 'CH' [4] * '", ans)
    ans <- gsub("ch4", "' 'CH' [4] * '", ans)
    ans <- gsub("dgrC", "' * degree * 'C' '", ans)
    ans <- gsub("degreeC", "' * degree * 'C' '", ans)
    ans <- gsub("degreesC", "' * degree * 'C' '", ans)
    ans <- gsub("degrees", "' * degree *'", ans)
    ans <- gsub("Delta", "' * Delta *'", ans)
    ans <- gsub("delta", "' * Delta *'", ans)
    ans <- gsub("ug/m3", "' * mu * 'g m' ^-3 *'", ans)
    ans <- gsub("ug.m-3", "' * mu * 'g m' ^-3 *'", ans)
    ans <- gsub("ug m-3", "' * mu * 'g m' ^-3 *'", ans)
    ans <- gsub("ugm-3", "' * mu * 'g m' ^-3 *'", ans)
    ans <- gsub("mg/m3", "' * 'm' * 'g m' ^-3 *'", ans)
    ans <- gsub("mg.m-3", "' * 'm' * 'g m' ^-3 *'", ans)
    ans <- gsub("mg m-3", "' * 'm' * 'g m' ^-3 *'", ans)
    ans <- gsub("mgm-3", "' * 'm' * 'g m' ^-3 *'", ans)
    ans <- gsub("ng/m3", "' * 'n' * 'g m' ^-3 *'", ans)
    ans <- gsub("ng.m-3", "' * 'n' * 'g m' ^-3 *'", ans)
    ans <- gsub("ng m-3", "' * 'n' * 'g m' ^-3 *'", ans)
    ans <- gsub("ngm-3", "' * 'n' * 'g m' ^-3 *'", ans)
    ans <- gsub("m/s", "' 'm s' ^-1 *'", ans)
    ans <- gsub("m.s-1", "' 'm s' ^-1 *'", ans)
    ans <- gsub("m s-1", "' 'm s' ^-1 *'", ans)
    ans <- gsub("g/km", "' 'g km' ^-1 *'", ans)
    ans <- gsub("g/s", "' 'g s' ^-1 *'", ans)
    ans <- gsub("km/hr/s", "' 'km hr' ^-1 * ' s' ^-1 *'", ans)
    ans <- gsub("km/hour/s", "' 'km hr' ^-1 * ' s' ^-1 *'", ans)
    ans <- gsub("km/h/s", "km hr' ^-1 * ' s' ^-1 *'", ans)
    ans <- gsub("km/hr", "' 'km hr' ^-1 *'", ans)
    ans <- gsub("km/h", "' 'km hr' ^-1 *'", ans)
    ans <- gsub("km/hour", "' 'km hr' ^-1 *'", ans)
    ans <- gsub("r2", "R' ^2 *'", ans)
    ans <- gsub("R2", "R' ^2 *'", ans)
    ans <- gsub("tau ", "' * tau * '", ans)
    ans <- gsub("umol/m2/s", "' * mu * 'mol m' ^-2 * ' s' ^-1 *'", 
                ans)
    ans <- gsub("umol/m2", "' * mu * 'mol m' ^-2 *'", ans)
    ans <- paste(ans, "'))", sep = "")
    if (substr(ans, (nchar(ans) - 8), (nchar(ans) - 6)) == "] *") {
      a <- ans
      ans <- paste(substr(a, 1, (nchar(a) - 7)), substr(a, 
                                                        (nchar(a) - 5), nchar(a)), sep = "")
    }
    ans <- gsub("''", "", ans)
    ans <- gsub("' '", "", ans)
    ans <- gsub("\\*  \\*", "~", ans)
    ans <- gsub("^expression\\(paste\\( \\*", "expression(paste(", 
                ans)
    ans <- gsub("^expression\\(paste\\(\\*", "expression(paste(", 
                ans)
    if (substr(ans, (nchar(ans) - 2), (nchar(ans) - 2)) == "*") {
      a <- ans
      ans <- paste(substr(a, 1, (nchar(a) - 2)), " ' ' ", substr(a, 
                                                                 (nchar(a) - 1), nchar(a)), sep = "")
    }
    if (grepl("\n", ans)) {
      a <- ans
      ans <- paste(substr(a, 1, 17), "atop(", substr(a, 18, 
                                                     nchar(a)), sep = "")
      ans <- gsub("\n", "' , '", ans)
      temp <- paste(")", sep = "", collapse = "")
      ans <- paste(ans, temp, sep = "")
    }
    if (inherits(try(eval(parse(text = ans)), TRUE), "try-error") == 
          FALSE) {
      ans <- eval(parse(text = ans))
    }
    else {
      ans <- text
    }
  }
  
  # Include checkPrep function
  checkPrep <- function(mydata, Names, type, remove.calm = TRUE, remove.neg = TRUE, 
            strip.white = TRUE) {
    
    conds <- c("default", "year", "hour", "month", "season", 
               "weekday", "weekend", "monthyear", "gmtbst", "bstgmt", 
               "daylight")
    all.vars <- unique(c(names(mydata), conds))
    varNames <- c(Names, type)
    matching <- varNames %in% all.vars
    if (any(!matching)) {
      stop(cat("Can't find the variable(s)", varNames[!matching], 
               "\n"))
    }
    if (any(type %in% conds == FALSE)) {
      ids <- which(type %in% conds == FALSE)
      Names <- c(Names, type[ids])
    }
    if (any(type %in% names(mydata))) {
      ids <- which(type %in% names(mydata))
      Names <- unique(c(Names, type[ids]))
    }
    mydata <- mydata[, Names]
    if ("date" %in% names(mydata)) {
      ids <- which(is.na(mydata$date))
      if (length(ids) > 0) {
        mydata <- mydata[-ids, ]
        warning(paste("Missing dates detected, removing", 
                      length(ids), "lines"), call. = FALSE)
      }
    }
    mydata[] <- lapply(mydata, function(x) {
      replace(x, x == Inf | x == -Inf, NA)
    })
    if ("ws" %in% Names & is.numeric(mydata$ws)) {
      if (any(sign(mydata$ws[!is.na(mydata$ws)]) == -1)) {
        if (remove.neg) {
          warning("Wind speed <0; removing negative data")
          mydata$ws[mydata$ws < 0] <- NA
        }
      }
    }
    if ("wd" %in% Names & is.numeric(mydata$wd)) {
      if (any(sign(mydata$wd[!is.na(mydata$wd)]) == -1 | mydata$wd[!is.na(mydata$wd)] > 
                360)) {
        warning("Wind direction < 0 or > 360; removing these data")
        mydata$wd[mydata$wd < 0] <- NA
        mydata$wd[mydata$wd > 360] <- NA
      }
      if (remove.calm) {
        if ("ws" %in% names(mydata)) {
          mydata$wd[mydata$ws == 0] <- NA
          mydata$ws[mydata$ws == 0] <- NA
        }
        mydata$wd[mydata$wd == 0] <- 360
        mydata$wd <- 10 * ceiling(mydata$wd/10 - 0.5)
        mydata$wd[mydata$wd == 0] <- 360
      }
      mydata$wd[mydata$wd == 0] <- 360
    }
    if ("date" %in% Names) {
      if (length(grep("/", as.character(mydata$date[1]))) > 
            0) {
        mydata$date <- as.POSIXct(strptime(mydata$date, "%d/%m/%Y %H:%M"), 
                                  "GMT")
      }
      if (is.factor(mydata$date)) 
        mydata$date <- as.POSIXct(mydata$date, "GMT")
      mydata <- mydata[order(mydata$date), ]
      if (names(mydata)[1] != "date") {
        mydata <- cbind(subset(mydata, select = date), subset(mydata, 
                                                              select = -date))
      }
      z <- as.POSIXlt(mydata$date[1])
      zz <- attr(z, "tzone")
      if (length(zz) == 3L) {
        warning("Detected data with Daylight Saving Time, converting to UTC/GMT")
        attr(mydata$date, "tzone") <- "GMT"
      }
    }
    if (strip.white) {
      suppressWarnings(trellis.par.set(list(strip.background = list(col = "white"))))
    }
    mydata
  }
  
  # Include cutData function
  
  cutData <- function(x, type = "default", hemisphere = "northern", n.levels = 4, 
            start.day = 1, is.axis = FALSE, ...){
    
    makeCond <- function(x, type = "default") {
      conds <- c("default", "year", "hour", "month", "season", 
                 "weekday", "wd", "site", "weekend", "monthyear", 
                 "bstgmt", "gmtbst", "daylight")
      if (type %in% conds & type %in% names(x)) {
        if (is.factor(x[, type])) {
          x[, type] <- factor(x[, type])
          return(x)
        }
      }
      if (type %in% conds == FALSE) {
        if (is.factor(x[, type]) | is.character(x[, type]) | 
              class(x[, type])[1] == "Date" | "POSIXt" %in% 
              class(x[, type])) {
          x[, type] <- factor(x[, type])
        }
        else {
          temp.levels <- levels(cut(x[, type], unique(quantile(x[, 
                                                                 type], probs = seq(0, 1, length = n.levels + 
                                                                                      1), na.rm = TRUE)), include.lowest = TRUE))
          x[, type] <- cut(x[, type], unique(quantile(x[, 
                                                        type], probs = seq(0, 1, length = n.levels + 
                                                                             1), na.rm = TRUE)), include.lowest = TRUE, 
                           labels = FALSE)
          x[, type] <- as.factor(x[, type])
          temp.levels <- gsub("[(]|[)]|[[]|[]]", "", temp.levels)
          temp.levels <- gsub("[,]", " to ", temp.levels)
          levels(x[, type]) <- if (is.axis) 
            temp.levels
          else paste(type, temp.levels)
        }
      }
      if (type == "default") {
        if ("date" %in% names(x)) {
          x[, type] <- factor(paste(format(min(x$date), 
                                           "%d %B %Y"), " to ", format(max(x$date), "%d %B %Y"), 
                                    sep = ""))
          x <- x[order(x$date), ]
        }
        else {
          x[, type] <- factor("all data")
        }
      }
      if (type == "year") 
        x[, type] <- factor(format(x$date, "%Y"))
      if (type == "hour") 
        x[, type] <- factor(format(x$date, "%H"))
      if (type == "month") {
        temp <- if (is.axis) 
          "%b"
        else "%B"
        x[, type] <- format(x$date, temp)
        month.abbs <- format(seq(as.Date("2000-01-01"), as.Date("2000-12-31"), 
                                 "month"), temp)
        ids <- which(month.abbs %in% unique(x$month))
        the.months <- month.abbs[ids]
        x[, type] <- ordered(x[, type], levels = the.months)
      }
      if (type == "monthyear") {
        x[, type] <- format(x$date, "%B %Y")
        x[, type] <- ordered(x[, type], levels = unique(x[, 
                                                          type]))
      }
      if (type == "season") {
        if (!hemisphere %in% c("northern", "southern")) {
          stop("hemisphere must be 'northern' or 'southern'")
        }
        if (hemisphere == "northern") {
          x[, type] <- "winter (DJF)"
          ids <- which(as.numeric(format(x$date, "%m")) %in% 
                         3:5)
          x[, type][ids] <- "spring (MAM)"
          ids <- which(as.numeric(format(x$date, "%m")) %in% 
                         6:8)
          x[, type][ids] <- "summer (JJA)"
          ids <- which(as.numeric(format(x$date, "%m")) %in% 
                         9:11)
          x[, type][ids] <- "autumn (SON)"
          seasons <- c("spring (MAM)", "summer (JJA)", 
                       "autumn (SON)", "winter (DJF)")
          ids <- which(seasons %in% unique(x$season))
          the.season <- seasons[ids]
          x[, type] <- ordered(x[, type], levels = the.season)
        }
        if (hemisphere == "southern") {
          x[, type] <- "summer (DJF)"
          ids <- which(as.numeric(format(x$date, "%m")) %in% 
                         3:5)
          x[, type][ids] <- "autumn (MAM)"
          ids <- which(as.numeric(format(x$date, "%m")) %in% 
                         6:8)
          x[, type][ids] <- "winter (JJA)"
          ids <- which(as.numeric(format(x$date, "%m")) %in% 
                         9:11)
          x[, type][ids] <- "spring (SON)"
          seasons <- c("spring (SON)", "summer (DJF)", 
                       "autumn (MAM)", "winter (JJA)")
          ids <- which(seasons %in% unique(x$season))
          the.season <- seasons[ids]
          x[, type] <- ordered(x[, type], levels = c("spring (SON)", 
                                                     "summer (DJF)", "autumn (MAM)", "winter (JJA)"))
        }
      }
      if (type == "weekend") {
        weekday <- selectByDate(x, day = "weekday")
        weekday[, type] <- "weekday"
        weekend <- selectByDate(x, day = "weekend")
        weekend[, type] <- "weekend"
        x <- rbind(weekday, weekend)
        x[, type] <- ordered(x[, type], levels = c("weekday", 
                                                   "weekend"))
      }
      if (type == "weekday") {
        x[, type] <- format(x$date, "%A")
        weekday.names <- format(ISOdate(2000, 1, 2:8), "%A")
        if (start.day < 0 || start.day > 6) 
          stop("start.day must be between 0 and 6.")
        day.ord <- c(weekday.names[(1 + start.day):7], weekday.names[1:(1 + 
                                                                          start.day - 1)])
        ids <- which(weekday.names %in% unique(x$weekday))
        the.days <- day.ord[ids]
        x[, type] <- ordered(x[, type], levels = the.days)
      }
      if (type == "wd") {
        id <- which(is.na(x$wd))
        if (length(id) > 0) {
          x <- x[-id, ]
          warning(paste(length(id), "missing wind direction line(s) removed"))
        }
        x[, type] <- cut(x$wd, breaks = seq(22.5, 382.5, 
                                            45), labels = c("NE", "E", "SE", "S", "SW", "W", 
                                                            "NW", "N"))
        x[, type][is.na(x[, type])] <- "N"
        x[, type] <- ordered(x[, type], levels = c("N", "NE", 
                                                   "E", "SE", "S", "SW", "W", "NW"))
      }
      if (type == "site") {
        x[, type] <- x$site
        x[, type] <- factor(x[, type])
      }
      if (type == "gmtbst" | type == "bstgmt") {
        x$date <- format(x$date, usetz = TRUE, tz = "Europe/London")
        id.BST <- grep("BST", x$date)
        id.GMT <- grep("GMT", x$date)
        bst <- x[id.BST, ]
        bst[, type] <- "BST hours"
        gmt <- x[id.GMT, ]
        gmt[, type] <- "GMT hours"
        x <- rbind.fill(bst, gmt)
        x[, type] <- factor(x[, type])
        x$date <- as.POSIXct(x$date, "GMT")
        x <- x[order(x$date), ]
      }
      if (type == "daylight") {
        x <- cutDaylight(x, ...)
      }
      x
    }
    for (i in 1:length(type)) {
      x <- makeCond(x, type[i])
    }
    x
  }
  
  
  # Include listUpdate function
  listUpdate <- function(a, b, drop.dots = TRUE, subset.a = NULL, subset.b = NULL) {
    if (drop.dots) {
      a <- a[names(a) != "..."]
      b <- b[names(b) != "..."]
    }
    if (!is.null(subset.a)) 
      a <- a[names(a) %in% subset.a]
    if (!is.null(subset.b)) 
      b <- b[names(b) %in% subset.b]
    if (length(names(b) > 0)) 
      a <- modifyList(a, b)
    a
  }
  
  # Include openColours function
  
  openColours <- function(scheme = "default", n = 100) {
    
    brewer.col <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", 
                    "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", 
                    "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", 
                    "YlOrRd", "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", 
                    "RdYlBu", "RdYlGn", "Spectral", "Accent", "Dark2", "Paired", 
                    "Pastel1", "Pastel2", "Set1", "Set2", "Set3")
    brewer.n <- c(rep(9, 18), rep(9, 9), c(8, 8, 12, 9, 8, 9, 
                                           8, 12))
    schemes <- c("increment", "default", "brewer1", "heat", "jet", 
                 "hue", "greyscale", brewer.col)
    heat <- colorRampPalette(brewer.pal(9, "YlOrRd"), interpolate = "spline")
    jet <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", 
                              "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    default.col <- colorRampPalette(brewer.pal(11, "Spectral"), 
                                    interpolate = "spline")
    brewer1 <- function(n) {
      if (n >= 3 & n <= 9) {
        brewer.pal(n, "Set1")
      }
      else {
        thefun <- suppressWarnings(colorRampPalette(brewer.pal(9, 
                                                               "Set1"), interpolate = "spline"))
        thefun(n)
      }
    }
    find.brewer <- function(thecol, n) {
      n.brew <- brewer.n[scheme == brewer.col]
      if (n >= 3 & n <= n.brew) {
        brewer.pal(n, thecol)
      }
      else {
        thefun <- suppressWarnings(colorRampPalette(brewer.pal(n.brew, 
                                                               thecol), interpolate = "spline"))
        thefun(n)
      }
    }
    increment <- colorRampPalette(c("#B0FFF1", "#9CFFC7", "#87FF8E", 
                                    "#A0FF73", "#B4FF69", "#CCFF60", "#E7FF56", "#FFF84D", 
                                    "#FFCB46", "#FF9C40", "#FF6939", "#FF3333", "#CC1B62", 
                                    "#990A7C", "#520066"))
    h = c(0, 360) + 15
    l = 65
    c = 100
    if ((diff(h)%%360) < 1) {
      h[2] <- h[2] - 360/n
    }
    hue <- grDevices::hcl(h = seq(h[1], h[2], length = n), c = c, 
                          l = l)
    greyscale <- grey(seq(0.9, 0.1, length = n))
    if (length(scheme) == 1) {
      if (scheme %in% brewer.col) 
        cols <- find.brewer(scheme, n)
      if (scheme == "increment") 
        cols <- increment(n)
      if (scheme == "default") 
        cols <- rev(default.col(n))
      if (scheme == "brewer1") 
        cols <- brewer1(n)
      if (scheme %in% brewer.col) 
        cols <- find.brewer(scheme, n)
      if (scheme == "heat") 
        cols <- heat(n)
      if (scheme == "jet") 
        cols <- jet(n)
      if (scheme == "hue") 
        cols <- hue
      if (scheme == "greyscale") 
        cols <- greyscale
    }
    if (!any(scheme %in% schemes)) {
      if (length(scheme) > 1) {
        user.cols <- colorRampPalette(scheme)
        cols = user.cols(n)
      }
      else {
        cols <- rep(scheme, n)
      }
    }
    cols
  }
  
  
  # Include scatterPlot function
  
  scatterPlot <- function(mydata, x = "nox", y = "no2", z = NA, method = "scatter", 
                          group = NA, avg.time = "default", data.thresh = 0, statistic = "mean", 
                          percentile = NA, type = "default", smooth = FALSE, spline = FALSE, 
                          linear = FALSE, ci = TRUE, mod.line = FALSE, cols = "hue", 
                          plot.type = "p", key = TRUE, key.title = group, key.columns = 1, 
                          key.position = "right", strip = TRUE, log.x = FALSE, log.y = FALSE, 
                          x.inc = 10, y.inc = 10, limits = NULL, y.relation = "same", 
                          x.relation = "same", ref.x = NULL, ref.y = NULL, k = 100, 
                          map = FALSE, auto.text = TRUE, ...) 
  {
    x.nam <- x
    y.nam <- y
    thekey <- key
    xgrid <- NULL
    ygrid <- NULL
    current.strip <- trellis.par.get("strip.background")
    on.exit(trellis.par.set("strip.background", current.strip))
    if (length(cols) == 1 && cols == "greyscale") {
      trellis.par.set(list(strip.background = list(col = "white")))
      method.col <- "greyscale"
    }
    else {
      method.col <- cols
    }
    Args <- list(...)
    Args$xlab <- if ("xlab" %in% names(Args)) 
      quickText(Args$xlab, auto.text)
    else quickText(x, auto.text)
    Args$ylab <- if ("ylab" %in% names(Args)) 
      quickText(Args$ylab, auto.text)
    else quickText(y, auto.text)
    Args$key.footer <- if ("key.footer" %in% names(Args)) 
      Args$key.footer
    else NULL
    if (!"lwd" %in% names(Args)) 
      Args$lwd <- 1
    if (!"lty" %in% names(Args)) 
      Args$lty <- 1
    if (!"layout" %in% names(Args)) 
      Args$layout <- NULL
    if ("trajStat" %in% names(Args)) 
      trajStat <- Args$trajStat
    
    else trajStat <- "mean"
    Args$map.cols <- if ("map.cols" %in% names(Args)) 
      Args$map.cols
    else "grey20"
    Args$map.alpha <- if ("map.alpha" %in% names(Args)) 
      Args$map.alpha
    else 0.2
    Args$map.fill <- if ("map.fill" %in% names(Args)) 
      Args$map.fill
    else TRUE
    Args$map.res <- if ("map.res" %in% names(Args)) 
      Args$map.res
    else "default"
    Args$trans <- if ("trans" %in% names(Args)) 
      Args$trans
    else function(x) log(x)
    Args$inv <- if ("inv" %in% names(Args)) 
      Args$inv
    else function(x) exp(x)
    if (log.x) 
      nlog.x <- 10
    else nlog.x <- FALSE
    if (log.y) 
      nlog.y <- 10
    else nlog.y <- FALSE
    if (!is.na(group)) 
      types <- c(type, group)
    else types <- type
    if (avg.time != "default") {
      if (group %in% dateTypes | type %in% dateTypes) 
        stop("Can't have an averging period set and a time-based 'type' or 'group'.")
      if ("default" %in% types) 
        mydata$default <- 0
      mydata <- ddply(mydata, types, timeAverage, avg.time = avg.time, 
                      statistic = statistic, percentile = percentile, data.thresh = data.thresh)
    }
    if (is.na(z) & method == "level") 
      stop("Need to specify 'z' when using method = 'level'")
    if (any(type %in% dateTypes) | !missing(avg.time)) {
      vars <- c("date", x, y)
    }
    else {
      vars <- c(x, y)
    }
    if (!is.na(group)) {
      if (group %in% dateTypes | !missing(avg.time) | any(type %in% 
                                                            dateTypes)) {
        if (group %in% dateTypes) {
          vars <- unique(c(vars, "date"))
        }
        else {
          vars <- unique(c(vars, "date", group))
        }
      }
      else {
        vars <- unique(c(vars, group))
      }
    }
    if (!is.na(group)) 
      if (group %in% type) 
        stop("Can't have 'group' also in 'type'.")
    traj <- FALSE
    if (all(c("date", "lat", "lon", "height", "pressure") %in% 
              names(mydata)) & plot.type == "l") 
      traj <- TRUE
    if (traj) 
      vars <- c(vars, "date")
    if (!is.na(z)) 
      vars <- c(vars, z)
    mydata <- checkPrep(mydata, vars, type)
    if (class(mydata[, x])[1] != "Date" & !"POSIXt" %in% class(mydata[, 
                                                                      x])) {
      mydata <- na.omit(mydata)
    }
    x.rot <- 0
    if ("factor" %in% class(mydata[, x]) | "character" %in% class(mydata[, 
                                                                         x])) {
      x.rot <- 90
      mydata[, x] <- factor(mydata[, x])
    }
    if (!is.na(z) & method == "scatter") {
      if (z %in% dateTypes) 
        stop("You tried to use a date type for the 'z' variable. \nColour coding requires 'z' to be continuous numeric variable'")
      if (class(mydata[, z]) %in% c("integer", "numeric") == 
            FALSE) 
        stop(paste("Continuous colour coding requires ", 
                   z, " to be numeric", sep = ""))
      key <- NULL
      mydata <- cutData(mydata, type, ...)
      if (missing(cols)) 
        cols <- "default"
      thecol <- openColours(cols, 100)[cut(mydata[, z], 100, 
                                           label = FALSE)]
      mydata$col <- thecol
      group <- "NewGroupVar"
      mydata$NewGroupVar <- "NewGroupVar"
      if (!"pch" %in% names(Args)) 
        Args$pch <- 16
      nlev <- 200
      if (missing(limits)) {
        breaks <- pretty(mydata[[z]], n = nlev)
        labs <- pretty(breaks, 7)
        labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
      }
      else {
        breaks <- pretty(limits, n = nlev)
        labs <- pretty(breaks, 7)
        labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
        if (max(limits) < max(mydata[[z]], na.rm = TRUE)) {
          id <- which(mydata[[z]] > max(limits))
          mydata[[z]][id] <- max(limits)
          thecol <- openColours(cols, 100)[cut(mydata[, 
                                                      z], 100, label = FALSE)]
          mydata$col <- thecol
          labs <- pretty(breaks, 7)
          labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
          labs[length(labs)] <- paste(">", labs[length(labs)])
        }
      }
      if (thekey) {
        nlev2 <- length(breaks)
        col <- openColours(cols, (nlev2 - 1))
        breaks <- c(breaks[1:(length(breaks) - 1)], max(mydata[[z]], 
                                                        na.rm = TRUE))
        col.scale <- breaks
        legend <- list(col = col, at = col.scale, labels = list(labels = labs), 
                       space = key.position, auto.text = auto.text, 
                       footer = Args$key.footer, header = Args$key.header, 
                       height = 1, width = 1.5, fit = "all")
        legend <- makeOpenKeyLegend(TRUE, legend, "other")
      }
      else {
        legend <- NULL
      }
    }
    else {
      mydata <- cutData(mydata, type, ...)
      if (!is.na(group)) 
        mydata <- cutData(mydata, group, ...)
      legend <- NULL
    }
    if (is.na(group)) {
      mydata$MyGroupVar <- factor("MyGroupVar")
      group <- "MyGroupVar"
    }
    npol <- length(unique(mydata[, group]))
    if (!"pch" %in% names(Args)) 
      Args$pch <- seq(npol)
    myColors <- openColours(cols, npol)
    temp <- paste(type, collapse = "+")
    myform <- formula(paste(y, "~", x, "|", temp, sep = ""))
    scales <- list(x = list(log = nlog.x, rot = x.rot, relation = x.relation), 
                   y = list(log = nlog.y, relation = y.relation, rot = 0))
    if (log.x) 
      mydata <- mydata[mydata[, x] > 0, ]
    if (log.y) 
      mydata <- mydata[mydata[, y] > 0, ]
    pol.name <- sapply(levels(mydata[, group]), function(x) quickText(x, 
                                                                      auto.text))
    if (is.na(z)) {
      if (key & npol > 1) {
        if (plot.type == "p") {
          key <- list(points = list(col = myColors[1:npol]), 
                      pch = if ("pch" %in% names(Args)) Args$pch else 1, 
                      text = list(lab = pol.name, cex = 0.8), space = key.position, 
                      columns = key.columns, title = quickText(key.title, 
                                                               auto.text), cex.title = 1, border = "grey")
        }
        if (plot.type %in% c("l", "s", "S", "spline")) {
          key <- list(lines = list(col = myColors[1:npol], 
                                   lty = Args$lty, lwd = Args$lwd), text = list(lab = pol.name, 
                                                                                cex = 0.8), space = key.position, columns = key.columns, 
                      title = quickText(key.title, auto.text), cex.title = 1, 
                      border = "grey")
        }
        if (plot.type == "b") {
          key <- list(points = list(col = myColors[1:npol]), 
                      pch = if ("pch" %in% names(Args)) Args$pch else 1, 
                      lines = list(col = myColors[1:npol], lty = Args$lty, 
                                   lwd = Args$lwd), text = list(lab = pol.name, 
                                                                cex = 0.8), space = key.position, columns = key.columns, 
                      title = quickText(key.title, auto.text), cex.title = 1, 
                      border = "grey")
        }
      }
      else {
        key <- NULL
      }
    }
    if (length(type) == 1 & type[1] == "wd" & is.null(Args$layout)) {
      wds <- c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
      mydata$wd <- ordered(mydata$wd, levels = wds)
      wd.ok <- sapply(wds, function(x) {
        if (x %in% unique(mydata$wd)) 
          FALSE
        else TRUE
      })
      skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])
      mydata$wd <- factor(mydata$wd)
      Args$layout <- c(3, 3)
      if (!"skip" %in% names(Args)) 
        Args$skip <- skip
    }
    if (!"skip" %in% names(Args)) 
      Args$skip <- FALSE
    strip.dat <- strip.fun(mydata, type, auto.text)
    strip <- strip.dat[[1]]
    strip.left <- strip.dat[[2]]
    pol.name <- strip.dat[[3]]
    if (length(type) == 1 & type[1] == "default") 
      strip <- FALSE
    id <- which(names(mydata) == group)
    names(mydata)[id] <- "MyGroupVar"
    groupMax <- length(unique(factor(mydata$MyGroupVar)))
    if (method == "scatter") {
      if (missing(k)) 
        k <- NULL
      xyplot.args <- list(x = myform, data = mydata, groups = mydata$MyGroupVar, 
                          type = c("p", "g"), as.table = TRUE, scales = scales, 
                          key = key, par.strip.text = list(cex = 0.8), strip = strip, 
                          strip.left = strip.left, yscale.components = yscale.components.log10ticks, 
                          xscale.components = xscale.components.log10ticks, 
                          legend = legend, panel = panel.superpose, ..., panel.groups = function(x, 
                                                                                                 y, col.symbol, col, type, col.line, lty, lwd, 
                                                                                                 group.number, subscripts, ...) {
                            if (traj) {
                              if (!is.na(z)) {
                                ddply(mydata[subscripts, ], "date", function(x) llines(x$lon, 
                                                                                       x$lat, col.line = x$col, lwd = lwd, lty = lty))
                              } else {
                                ddply(mydata[subscripts, ], .(date), function(x) llines(x$lon, 
                                                                                        x$lat, col.line = myColors[group.number], 
                                                                                        lwd = lwd, lty = lty))
                              }
                            }
                            if (!is.na(z) & !traj) panel.xyplot(x, y, col.symbol = thecol[subscripts], 
                                                                as.table = TRUE, ...)
                            if (is.na(z) & !traj) panel.xyplot(x, y, type = plot.type, 
                                                               col.symbol = myColors[group.number], col.line = myColors[group.number], 
                                                               lty = lty, lwd = lwd, as.table = TRUE, ...)
                            if (linear & npol == 1) panel.linear(x, y, col = "black", 
                                                                 myColors[group.number], lwd = 1, lty = 5, x.nam = x.nam, 
                                                                 y.nam = y.nam, se = ci, ...)
                            if (smooth) panel.gam(x, y, col = "grey20", col.se = "black", 
                                                  lty = 1, lwd = 1, se = ci, k = k, ...)
                            if (spline) panel.smooth.spline(x, y, col = "grey20", 
                                                            lwd = lwd, ...)
                            if (map && group.number == groupMax) add.map(Args, 
                                                                         ...)
                            if (mod.line && group.number == 1) panel.modline(log.x, 
                                                                             log.y)
                            panel.abline(v = ref.x, lty = 5)
                            panel.abline(h = ref.y, lty = 5)
                          })
      default.main <- if (is.na(z)) 
        ""
      else paste(x, "vs.", y, "by levels of", z)
      Args$main <- if ("main" %in% names(Args)) 
        quickText(Args$main, auto.text)
      else quickText(default.main, auto.text)
      if (!"pch" %in% names(Args)) 
        Args$pch <- 1
      xyplot.args <- listUpdate(xyplot.args, Args)
      plt <- do.call(xyplot, xyplot.args)
    }
    if (method == "hexbin") {
      require(hexbin)
      hexbinplot.args <- list(x = myform, data = mydata, strip = strip, 
                              scales = scales, strip.left = strip.left, as.table = TRUE, 
                              yscale.components = yscale.components.log10ticks, 
                              xscale.components = xscale.components.log10ticks, 
                              par.strip.text = list(cex = 0.8), colorkey = TRUE, 
                              colramp = function(n) {
                                openColours(method.col, n)
                              }, ..., panel = function(x, ...) {
                                panel.grid(-1, -1)
                                panel.hexbinplot(x, ...)
                                if (mod.line) panel.modline(log.x, log.y)
                                if (map) add.map(Args, ...)
                                panel.abline(v = ref.x, lty = 5)
                                panel.abline(h = ref.y, lty = 5)
                              })
      Args$main <- if ("main" %in% names(Args)) 
        quickText(Args$main, auto.text)
      else quickText("", auto.text)
      if (!"pch" %in% names(Args)) 
        Args$pch <- 1
      hexbinplot.args <- listUpdate(hexbinplot.args, Args)
      plt <- do.call(hexbinplot, hexbinplot.args)
    }
    if (method == "level") {
      mydata$ygrid <- round_any(mydata[, y], y.inc)
      mydata$xgrid <- round_any(mydata[, x], x.inc)
      rhs <- c("xgrid", "ygrid", type)
      rhs <- paste(rhs, collapse = "+")
      myform <- formula(paste(z, "~", rhs))
      if (nrow(unique(subset(mydata, select = c(xgrid, ygrid)))) != 
            nrow(mydata)) {
        mydata <- aggregate(myform, data = mydata, mean, 
                            na.rm = TRUE)
      }
      smooth.grid <- function(mydata, z) {
        myform <- formula(paste(z, "~ s(xgrid, ygrid, k = ", 
                                k, ")", sep = ""))
        res <- 101
        Mgam <- gam(myform, data = mydata)
        new.data <- expand.grid(xgrid = seq(min(mydata$xgrid), 
                                            max(mydata$xgrid), length = res), ygrid = seq(min(mydata$ygrid), 
                                                                                          max(mydata$ygrid), length = res))
        pred <- predict.gam(Mgam, newdata = new.data)
        pred <- as.vector(pred)
        new.data[, z] <- pred
        x <- seq(min(mydata$xgrid), max(mydata$xgrid), length = res)
        y <- seq(min(mydata$ygrid), max(mydata$ygrid), length = res)
        wsp <- rep(x, res)
        wdp <- rep(y, rep(res, res))
        all.data <- na.omit(data.frame(xgrid = mydata$xgrid, 
                                       ygrid = mydata$ygrid, z))
        ind <- with(all.data, exclude.too.far(wsp, wdp, mydata$xgrid, 
                                              mydata$ygrid, dist = 0.05))
        new.data[ind, z] <- NA
        new.data
      }
      if (smooth) 
        mydata <- ddply(mydata, type, smooth.grid, z)
      temp <- paste(type, collapse = "+")
      myform <- formula(paste(z, "~ xgrid * ygrid |", temp, 
                              sep = ""))
      nlev <- 200
      if (missing(limits)) {
        breaks <- pretty(mydata[[z]], n = nlev)
        labs <- pretty(breaks, 7)
        labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
      }
      else {
        breaks <- pretty(limits, n = nlev)
        labs <- pretty(breaks, 7)
        labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
        if (max(limits) < max(mydata[[z]], na.rm = TRUE)) {
          id <- which(mydata[[z]] > max(limits))
          mydata[[z]][id] <- max(limits)
          labs <- pretty(breaks, 7)
          labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
          labs[length(labs)] <- paste(">", labs[length(labs)])
        }
      }
      nlev2 <- length(breaks)
      if (missing(cols)) 
        cols <- "default"
      col <- openColours(cols, (nlev2 - 1))
      breaks <- c(breaks[1:(length(breaks) - 1)], max(mydata[[z]], 
                                                      na.rm = TRUE))
      col.scale <- breaks
      if (trajStat %in% c("cwt", "pscf", "mean")) {
        legend <- list(col = col, at = col.scale, labels = list(labels = labs), 
                       space = key.position, auto.text = auto.text, 
                       footer = Args$key.footer, header = Args$key.header, 
                       height = 1, width = 1.5, fit = "all")
        legend <- makeOpenKeyLegend(key, legend, "other")
      }
      if (trajStat %in% c("frequency", "difference")) {
        if (trajStat == "frequency") {
          breaks <- c(0, 1, 5, 10, 25, 100)
          labels <- c("0 to 1", "1 to 5", "5 to 10", "10 to 25", 
                      "25 to 100")
        }
        if (trajStat == "difference") {
          breaks <- c(-15000, -10, -5, -1, 1, 5, 10, 15000)
          labels <- c("<-10", "-10 to -5", "-5 to -1", 
                      "-1 to 1", "1 to 5", "5 to 10", ">10")
        }
        n <- 7
        col <- openColours(cols, n)
        legend <- list(col = col, space = key.position, auto.text = auto.text, 
                       labels = labels, footer = Args$key.footer, header = Args$key.header, 
                       height = 0.8, width = 1.5, fit = "scale", plot.style = "other")
        col.scale <- breaks
        legend <- makeOpenKeyLegend(key, legend, "windRose")
      }
      levelplot.args <- list(x = myform, data = mydata, strip = strip, 
                             as.table = TRUE, region = TRUE, scales = scales, 
                             yscale.components = yscale.components.log10ticks, 
                             xscale.components = xscale.components.log10ticks, 
                             col.regions = col, at = col.scale, par.strip.text = list(cex = 0.8), 
                             colorkey = FALSE, legend = legend, panel = function(x, 
                                                                                 y, z, subscripts, ...) {
                               panel.grid(h = -1, v = -1)
                               panel.levelplot(x, y, z, subscripts, labels = FALSE, 
                                               ...)
                               if (mod.line) panel.modline(log.x, log.y)
                               if (map) add.map(Args, ...)
                               panel.abline(v = ref.x, lty = 5)
                               panel.abline(h = ref.y, lty = 5)
                             })
      Args$main <- if ("main" %in% names(Args)) 
        quickText(Args$main, auto.text)
      else quickText(paste(x, "vs.", y, "by levels of", z), 
                     auto.text)
      if (!"pch" %in% names(Args)) 
        Args$pch <- 1
      levelplot.args <- listUpdate(levelplot.args, Args)
      plt <- do.call(levelplot, levelplot.args)
    }
    if (method == "density") {
      prepare.grid <- function(subdata) {
        x <- subdata[, x]
        y <- subdata[, y]
        xy <- xy.coords(x, y, "xlab", "ylab")
        xlab <- xy$xlab
        ylab <- xy$ylab
        x <- cbind(xy$x, xy$y)[is.finite(xy$x) & is.finite(xy$y), 
                               , drop = FALSE]
        xlim <- range(x[, 1])
        ylim <- range(x[, 2])
        Map <- .smoothScatterCalcDensity(x, 256)
        xm <- Map$x1
        ym <- Map$x2
        dens <- Map$fhat
        grid <- expand.grid(x = xm, y = ym)
        results <- data.frame(x = grid$x, y = grid$y, z = as.vector(dens))
        results
      }
      results.grid <- ddply(mydata, type, prepare.grid)
      nlev <- 200
      breaks <- pretty(results.grid$z, n = nlev)
      nlev2 <- length(breaks)
      col <- openColours(method.col, (nlev2 - 1))
      col <- c("transparent", col)
      col.scale <- breaks
      temp <- paste(type, collapse = "+")
      myform <- formula(paste("z ~ x * y", "|", temp, sep = ""))
      levelplot.args <- list(x = myform, data = results.grid, 
                             as.table = TRUE, scales = scales, strip = strip, 
                             yscale.components = yscale.components.log10ticks, 
                             xscale.components = xscale.components.log10ticks, 
                             strip.left = strip.left, par.strip.text = list(cex = 0.8), 
                             col.regions = col, region = TRUE, at = col.scale, 
                             colorkey = FALSE, ..., panel = function(x, y, z, 
                                                                     subscripts, ...) {
                               panel.grid(-1, -1)
                               panel.levelplot(x, y, z, subscripts, pretty = TRUE, 
                                               labels = FALSE, ...)
                               if (mod.line) panel.modline(log.x, log.y)
                               if (map) add.map(Args, ...)
                               panel.abline(v = ref.x, lty = 5)
                               panel.abline(h = ref.y, lty = 5)
                             })
      Args$main <- if ("main" %in% names(Args)) 
        quickText(Args$main, auto.text)
      else quickText("", auto.text)
      if (!"pch" %in% names(Args)) 
        Args$pch <- 1
      levelplot.args <- listUpdate(levelplot.args, Args)
      plt <- do.call(levelplot, levelplot.args)
    }
    if (length(type) == 1) 
      plot(plt)
    else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    newdata <- mydata
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"
    invisible(output)
  }
  
  
  len <- NULL
  traj.df <- traj.df[order(traj.df$date, traj.df$hour.inc), ]
  traj.df$len <- ave(traj.df$lat, traj.df$date, FUN = length)
  n <- max(abs(traj.df$hour.inc)) + 1
  traj.df <- subset(traj.df, len == n)
  
  # Collect plotting parameters
  extra.args <- list(...)
  method <- "scatter"
  if (!"plot.type" %in% names(extra.args)) 
    extra.args$plot.type <- "l"
  if (!"cex" %in% names(extra.args)) 
    extra.args$cex <- 0.1
  if (!"ylab" %in% names(extra.args)) 
    extra.args$ylab <- "latitude"
  if (!"xlab" %in% names(extra.args)) 
    extra.args$xlab <- "longitude"
  
  
  if (missing(pollutant)) {
    if (is.na(group)) 
      key <- FALSE
    else key <- TRUE
    if (!"main" %in% names(extra.args)) 
      extra.args$main <- NULL
    scatterPlot.args <- list(traj.df, x = lon, y = lat, z = NA, 
                             type = type, method = method, smooth = smooth, map = map, 
                             x.inc = lon.inc, y.inc = lat.inc, key = key, group = group, 
                             map.fill = map.fill, map.res = map.res, map.cols = map.cols, 
                             map.alpha = map.alpha)
  }
  else {
    if (!"main" %in% names(extra.args)) 
      extra.args$main <- pollutant
    scatterPlot.args <- list(traj.df, x = lon, y = lat, z = pollutant, 
                             type = type, method = method, smooth = smooth, map = map, 
                             x.inc = lon.inc, y.inc = lat.inc, group = group, 
                             map.fill = map.fill, map.res = map.res, map.cols = map.cols, 
                             map.alpha = map.alpha)
  }
  scatterPlot.args <- listUpdate(scatterPlot.args, extra.args)
  do.call(scatterPlot, scatterPlot.args)
  
}