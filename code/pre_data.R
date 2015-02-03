#!/usr/bin/Rscript

library(digest)
library(xlsx)
source("inc_period.R")                                          # Initialize DATA.month, DATA.year & file names

ChooseHash <- function(algo) {
        MyDigest <- function (x) {
                hash.x <- vector(mode = "character", length = length(x))
                for (i in seq_along(x)) {
                      hash.x[[i]] <- digest(as.character(x[[i]]), algo = algo)
                }
                hash.x
        }
        MyDigest
}

CheckCollision <- function(factor.x, factor.y) {
        if (length(levels(factor.x)) != length(levels(factor.y))) {
                stop("Collision detected. Try to change parameters of 'ChooseHash' function")
        }
        return(TRUE)
}

ReplaceName <- function(str.in) {
        # Replace normalized (shortened) names according to SUBST[ , 2]

        str.out <- as.character(str.in)
        if (exists("SUBST")) {
                i <- 1
                SUBST.len  <- nrow(SUBST)
                strin.hash <- MyHash(str.in)
                while (i <= SUBST.len) {
                        if (strin.hash == SUBST$hash[i]) {
                                str.out <- as.character(SUBST$newname[i])
                                break
                        } else {
                                i <- i + 1
                        }
                }
        }
        return(str.out)
}

ShorteName <- function(full_name3, mode = "short") {

        name3       <- unlist(strsplit(as.character(full_name3), " +"))
        family_name <- paste(toupper(substr(name3[1], 1, 1)), substring(tolower(name3[1]), 2), sep = "")
        if (mode == "short") {
                initial1    <- paste(toupper(substr(name3[2], 1, 1)), ".", sep = "")
                initial2    <- paste(toupper(substr(name3[3], 1, 1)), ".", sep = "")
                short_name  <- paste(family_name, initial1, initial2, sep = " ")
        } else {
                given_name  <- paste(toupper(substr(name3[2], 1, 1)), substring(tolower(name3[2]), 2), sep = "")
                short_name  <- paste(given_name, family_name, sep = " ")
        }
        return (short_name)
}

MakeIntervals <- function(x, ...) {
        y      <- data.frame()
        n.row  <- nrow(x)
        x.rec  <- 1
        y.rec  <- 1
        while (x.rec < n.row) {
                if ((x$empid[x.rec]     != x$empid[x.rec + 1]) |    # Next empl
                            (x$ddate[x.rec]     != x$ddate[x.rec + 1]) |    # Next Date
                            (x$sign [x.rec]     != -1)                 |    # Unmatched Record
                            (x$sign [x.rec + 1] !=  1)) {
                        x.rec <- x.rec + 1                          # Skip a Record
                } else {
                        y[y.rec, "empid"]     <- x[x.rec, "empid"]
                        y[y.rec, "empl"]  <- x[x.rec, "empl"]
                        y[y.rec, "ddate"]     <- x[x.rec, "ddate"]
                        y[y.rec, "time0"]     <- x$times[x.rec]
                        y[y.rec, "time1"]     <- x$times[x.rec + 1]
                        x.rec <- x.rec + 2
                        y.rec <- y.rec + 1
                }
        }
        y$ddate <- as.Date(y$ddate, as.Date("1970-01-01"))
        return (y)
}

MyHash <- ChooseHash("xxhash64")

### Initialising SUBST[ , 2]
SUBST         <- read.csv2(FILE.sub, stringsAsFactors = FALSE, header = FALSE, col.names = c("oldname", "newname"))
SUBST$oldname <- sapply(SUBST$oldname, ShorteName)
SUBST$newname <- sapply(SUBST$newname, ShorteName)
SUBST$hash    <- MyHash(SUBST$oldname)

# Turnstile Data Handling

x1.classes <- c("factor", "numeric", "factor", "character", "character", "factor", "factor", "factor", "factor")
x1         <- read.csv(FILE.trn, skip = 4, colClasses = x1.classes)[-c(2, 3, 7, 8, 9)]
names(x1)  <- c("empl", "ddate", "times", "inout")
x1$sign[x1$inout == "Вход"]  <- as.integer(-1)
x1$sign[x1$inout == "Выход"] <- as.integer(1)

x1$times <- paste(x1$ddate, x1$times)
x1$times <- as.POSIXct(x1$times, format = "%d.%m.%Y %H:%M:%S")
x1$ddate <- as.Date(x1$ddate, format = "%d.%m.%y")
x1$empid <- as.numeric(x1$empl)
x1       <- transform(x1[order(x1$empid, unclass(x1$times)), ])

# Filter out holidays & weekends from turnstile data
day.0     <- as.Date(paste(DATA.year, DATA.month, "01", sep = "-"))             # First day of the month
day.1     <- lastDayInMonth(day.0)                                              # Last day of the month
d.mon     <- seq(day.0, day.1, by = "1 day")                                    # All days in the month
week.ends <- d.mon[weekdays(d.mon) %in% c("Saturday", "Sunday")]                # Weekends of the month
d.exclude <- c(week.ends, DAYS.off[months(DAYS.off) == months(day.0)])
x1        <- x1[which(! x1$ddate %in% d.exclude), ]

y1          <- MakeIntervals(x1)
y1$time0    <- as.POSIXct(y1$time0, origin = '1970-01-01')
y1$time1    <- as.POSIXct(y1$time1, origin = '1970-01-01')
y1$time0    <- as.POSIXct(as.character(y1$time0), format = "%y-%m-%d %H:%M:%S")
y1$time1    <- as.POSIXct(as.character(y1$time1), format = "%y-%m-%d %H:%M:%S")
y1$empl     <- sapply(y1$empl, ShorteName)
y1$empl     <- as.factor(sapply(y1$empl, ReplaceName))
y1$emph     <- as.factor(MyHash(y1$empl))
CheckCollision(y1$empl, y1$emph)
y1          <- y1[ ,!(names(y1) %in% c("ddate", "empid"))]         # Drop excessive columns
y1$src      <- "trn"
y1          <- y1[complete.cases(y1), ]
y1$empl     <- NULL

# CRM Data Handling

x2           <- read.csv2(FILE.crm) [c(2, 3, 4, 10, 12)]
names(x2)    <- c("full.name", "time0", "time1", "type", "conf")
x2           <- x2[which(x2$conf != ""), ]
x2$empl      <- sapply(x2$full.name, ShorteName)
x2$empl      <- as.factor(sapply(x2$empl, ReplaceName))
x2$time0     <- as.POSIXct(paste(x2$time0, ":00", sep = ""), format = "%d.%m.%Y %H:%M:%S")
x2$time1     <- as.POSIXct(paste(x2$time1, ":00", sep = ""), format = "%d.%m.%Y %H:%M:%S")

y2           <- subset(x2, x2$type == "деловая встреча", drop = TRUE)
y2           <- y2[ , c("empl", "time0", "time1")]
y2$time0     <- as.POSIXct(y2$time0, format = "%d.%m.%Y %H:%M")
y2$time1     <- as.POSIXct(y2$time1, format = "%d.%m.%Y %H:%M")
y2$empl      <- as.factor(as.character(y2$empl))
y2$emph      <- as.factor(MyHash(y2$empl))
CheckCollision(y2$empl, y2$emph)
y2$src       <- "crm"
y2           <- y2[complete.cases(y2), ]
rownames(y2) <- NULL
y2$empl      <- NULL

# Total number of working days in the month for everybody

emp             <- read.xlsx(FILE.day, 1)
emp             <- emp[-nrow(emp), ]                              # Eliminate last row (totals)
names(emp)      <- c("full.name", "ttl", "ref", "vac", "ill", "non")
emp[is.na(emp)] <- 0
emp$ref         <- emp$ttl - emp$vac - emp$ill - emp$non
emp$empl        <- sapply(emp$full.name, ShorteName)
emp$empl        <- as.factor(sapply(emp$empl, ReplaceName))
emp$emph        <- as.factor(MyHash(emp$empl))
emp$short.name  <- sapply(emp$full.name, function(x) ShorteName(x, "given_name family_name"))
CheckCollision(emp$empl, emp$emph)

# List of club members

club      <- read.csv2(FILE.mrk)
club$emph <- MyHash(club$empl)
emp       <- merge(emp, club[c("emph", "badge")], by = "emph", all.x = TRUE)
emp       <- emp[order(emp$empl), ]
emp$badge[is.na(emp$badge)] <- 0                                # In case new records appear in 'FILE.day'
write.csv2(emp, file = FILE.mrk)                                # Update 'FILE.mrk' (add new records if any)
emp       <- emp[emp$badge == 1, ]

# Filter employees by "1C" list

y1 <- merge(emp[c("emph", "empl")], y1, by = "emph")            # Turnstile data
y2 <- merge(emp[c("emph", "empl")], y2, by = "emph")            # CRM data

# Redmine Data Preprocessing

rd        <- read.csv2(FILE.red)[c(1, 7, 8, 12, 19, 21, 20)]
names(rd) <- c("id", "job", "short.name", "ddate", "start.hour", "start.quater", "duration")
rd$ddate  <- as.Date(rd$ddate, format = "%d.%m.%Y")
rd        <- rd[(rd$ddate >= day.0 & rd$ddate <= day.1), ]      # Filter data for current month
rd$time0  <- as.POSIXct(paste(rd$ddate, paste(rd$start.hour, rd$start.quater, sep = ":")), format = "%Y-%m-%d %H:%M")
rd$time1  <- rd$time0 + as.difftime(rd$duration, units = "hours")
rd        <- merge(rd, emp[c("emph", "empl", "short.name")], by = "short.name")[ ,c("emph", "empl", "time0", "time1")]
rd$src    <- "crm"

# Join redmine & crm data

y2 <- rbind(y2, rd)

# Output preprocessed data

save(emp, y1, y2, file = FILE.obj, compress = "bzip2", compression_level = 9)
