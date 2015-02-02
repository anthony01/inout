#!/usr/bin/Rscript

library(R2HTML)
source("inc_period.R")                                          # Initialize DATA.month, DATA.year & file names
                                  
WORKDAY.hours    <- 8
PERIOD           <- data.frame(c("00:00:00", "07:00:00", "08:00:00", "20:00:00", "23:00:00", "23:59:59"),
                               c( 2,          1.5,        1,          1.5,        2,          2), stringsAsFactors = FALSE)
colnames(PERIOD) <- c("start", "coeff")
N.period         <- nrow(PERIOD)

ShowRunTime <- function(start.time, units = "secs") {
        tag <- switch(units,
                      secs  = "seconds",
                      mins  = "minutes",
                      hours = "hours",
                      days  = "days",
                      weeks = "weeks"
        )
        cat(paste("run time is", as.character(round(difftime(Sys.time(), start.time, units = units))), tag))
}

CalcTotalHours <- function (x) {
        total.hours <- 0
        i           <- 1
        while (i <= nrow(x)) {
                inter.0 <- as.POSIXct(x[["time0"]][i], format = "%Y-%m-%d %H:%M:%S")
                inter.1 <- as.POSIXct(x[["time1"]][i], format = "%Y-%m-%d %H:%M:%S")
                date0   <- unlist(strsplit(as.character(inter.0), " ", fixed = TRUE))[1]
                date1   <- unlist(strsplit(as.character(inter.1), " ", fixed = TRUE))[1]
                d.day   <- as.Date(date0)
                while (d.day <= as.Date(date1)) {               # Multi-date activity handling
                        c.day <- as.character(d.day)
                        j <- 1
                        while (j < N.period) {
                                tstamp.0    <- paste(c.day, PERIOD$start[j])
                                tstamp.1    <- paste(c.day, PERIOD$start[j+1])
                                period.0    <- as.POSIXct(tstamp.0, format = "%Y-%m-%d %H:%M:%S")
                                period.1    <- as.POSIXct(tstamp.1, format = "%Y-%m-%d %H:%M:%S")
                                overlap     <- CalcOverlap(inter.0, inter.1, period.0, period.1)
                                period.coef <- max(DayCoeff(d.day), PERIOD$coeff[j])
                                total.hours <- total.hours +  overlap * period.coef
                                j <- j + 1
                        }
                        d.day <- d.day + 1
                } 
        i <- i + 1
        }
        return(total.hours)
}

DayCoeff <- function (ddate) {
        
        week.day <- weekdays(ddate)
        if (ddate %in% DAYS.off | (week.day %in% c("Saturday", "Sunday")))  # DAYS.off must be initialized in advance
                coeff <- 2
        else
                coeff <- 1
        return(coeff)
}

CalcOverlap <- function (x0, x1, y0, y1) {
        
        if      (x0 > x1 | y0 > y1) overlap <- 0                # Incorrect data
        else if (x1 < y0 | y1 < x0) overlap <- 0                # No intersection
        else {
                point   <- sort(c(x0, x1, y0, y1))
                overlap <- as.numeric(difftime(point[3], point[2], units = "hours"))
        }
        return(overlap)        
}

SumInter <- function(x, ...) {
        y <- sum(unclass(x$time1) - unclass(x$time0)) / 3600
        return(y)
}

Set2BHInterval <- function(x) {

        for (i in seq_len(nrow(x))) {
                day.start  <- as.POSIXct(paste(as.character(x$ddate[i]), "08:00:00"))
                day.end    <- as.POSIXct(paste(as.character(x$ddate[i]), "20:00:00"))
                x$time0[i] <- max(day.start, x$time0[i])
                x$time1[i] <- min(day.end,   x$time1[i])
        }
        return(x)
}

ExcludeDoubleTimes <- function(x) {
        y <- x
        
        for (i in seq_len(nrow(y)))
                if (y$time1[i] < y$time0[i])
                        y$time1[i] <- y$time0[i]                # Correct time intervals
        
        for (i in seq_len(nrow(y))) {
                j <- i
                while (j < nrow(y)) {                           # Note: y$time0[i] <= y$time0[j] (due to preordering)
                        j <- j + 1
                        if (y$time0[j] < y$time1[i]) {          # Intervals do overlap
                                if (y$time1[i] < y$time1[j]) {  # i0, i1, j0, j1
                                        if (y$src[j] == "trn") { 
                                                y$time0[j] <- y$time1[i]
                                        } else {
                                                y$time1[i] <- y$time0[j]
                                        }
                                } else {                        # i0, j0, j1, i1
                                        if (y$src[j] == "trn") { 
                                                y$time0[j] <- y$time1[j]
                                        } else {
                                                y$time1[i] <- y$time0[j]
                                        }
                                }
                        }
                }
        }
        return(y)
}

Delete.NULLs  <-  function(x.list){                             # Delete null/empty entries in a list

        x.list[unlist(lapply(x.list, length) != 0)]
}

start.time  <- Sys.time()
### Load preprocessed data

load(FILE.obj)                                                  # Read emp, y1, y2
y1$ddate <- as.Date(y1$time0)
z1       <- Set2BHInterval(y1)                                  # Adjust timestamps according to business hours interval
z1       <- z1[ ,!(names(z1) %in% c("ddate"))]                  # Remove "ddate" column

# Turnstile data analysis

y1.sum   <- sapply(split(y1, y1$empl), function(x) sapply(split(x, x$ddate), SumInter))
y1.sum   <- Delete.NULLs(y1.sum)
y1.sum2  <- sapply(y1.sum, sum)
y1.av    <- y1.sum2 / sapply(y1.sum, length)
y1.top   <- y1.av[order(-y1.av)]
y1       <- y1[ , !(names(y1) %in% c("ddate"))]                 # Clean up y1 for future use :)

# Merge Datasets

z2             <- rbind(z1, y2)                                       # Join two data sets
z2$src         <- as.factor(z2$src)
z3             <- z2[order(z2$emph, z2$time0), ]                      # Prepare for ExcludeDoubleTimes
z4             <- lapply(split(z3, z3$emph), ExcludeDoubleTimes)      # Eliminate double counted time periods
z5             <- t(sapply(z4, function(x) sapply(split(x, x$src), CalcTotalHours))) # Calculate trn & crm separately
z7             <- merge(z5, emp, by.x = "row.names", by.y = "emph", all.y = TRUE)
z7$office      <- z7$trn - z7$ref * WORKDAY.hours
z7$extra       <- ifelse(z7$office < 0, z7$office + z7$crm, z7$crm)   # Calculate hours payable
tab            <- z7[order(z7$empl), c("empl", "ref", "trn", "crm", "extra")]
tab$trn        <- floor(tab$trn)
tab$crm        <- floor(tab$crm)
tab$extra      <- round(tab$extra)
row.names(tab) <- NULL

a1             <- tab$extra
names(a1)      <- tab$empl
a2             <- sort(a1, decreasing = TRUE)
a3             <- a2[a2 > 0]

# Output summary data

cat("<meta charset='utf-8'> \n", file = FILE.out, sep = "")
cat('<link rel="stylesheet" href="//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css">',
                                 file = FILE.out, sep = "", append = TRUE)
HTML(tab, file = FILE.out, classtable = "'table table-striped table-bordered'")

ShowRunTime(start.time)
