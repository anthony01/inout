cdr0 <- read.csv2("IPOStat100.csv", stringsAsFactors = FALSE)

cdr <- cdr0$Data

file.tmp <- tempfile(pattern = "ipo_", tmpdir = tempdir(), fileext = ".csv")
tmp.con <- file(file.tmp, open = "w+")
for (i in 1:length(cdr)) writeLines(cdr[i], con = tmp.con, sep = "\n")
close(tmp.con)

#cdr <- read.csv(file = file.tmp, header = FALSE, nrows = 50); classes <- sapply(cdr, class)
cdr <- read.csv(file.tmp, header = FALSE) #, colClasses = classes)

fields <- c("Date", "Duration", "Ring", "A", "Direction", "CalledNum", "B", "IsInt", "CallID", "Continuation", "P1ID", "P1Name", "P2ID", "P2Name", "HoldTime", "ParkTime", "UsrCharged", "UsrMarkUp")
col2del <- c(8, 18, 19, 21, 22, 23, 24, 25, 26, 28, 29, 30)
cdr <- cdr[-col2del]
names(cdr) <- fields
#cdr$Date <- strptime(as.character(cdr$Date), "%Y/%m/%d %H:%M:%OS")
#cdr$Duration <- strptime(as.character(cdr$Duration), "%H:%M:%OS")
#cdr$c.A <- as.character(cdr$A)
#cdr$c.B <- as.character(cdr$B)

data.file <- gzfile("data.gz", "w")
dump("cdr", file = data.file, append = FALSE)
close(data.file)