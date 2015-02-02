rd        <- read.csv2("../data/redmine.csv")[c(1, 7, 8, 12, 19, 21, 20)]
names(rd) <- c("id", "job", "short.name", "ddate", "start.hour", "start.quater", "duration")
rd$ddate  <- as.Date(rd$ddate, format = "%d.%m.%Y")

if (FALSE) {
        rd$start.hour   <- sample(0:23, nrow(rd), replace = TRUE)
        rd$start.quater <- sample(c(0, 15, 30, 45), nrow(rd), replace = TRUE)
        rd$duration     <- rgamma(nrow(rd), 4, 2)
}
day.0     <- as.Date(paste(DATA.year, DATA.month, "01", sep = "-"))             # First day of the month
day.1     <- lastDayInMonth(day.0)                                              # Last day of the month
rd        <- rd[(rd$ddate >= day.0 & rd$ddate <= day.1), ]

rd$time0  <- as.POSIXct(paste(rd$ddate, paste(rd$start.hour, rd$start.quater, sep = ":")), format = "%Y-%m-%d %H:%M")
rd$time1  <- rd$time0 + as.difftime(rd$duration, units = "hours")
rd        <- merge(rd, emp[c("emph", "empl", "short.name")], by = "short.name")[ ,c("emph", "empl", "time0", "time1")]
rd$src    <- "crm"