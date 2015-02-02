file.old <- file("../data/inout201411.csv", "r")
file.new <- file("../data/turn201411.csv",  "w")
head.old <- readLines(file.old, n = 4)
writeLines(head.old, file.new)

x1          <- read.csv2(file.old, skip = 4, header = FALSE, stringsAsFactors = FALSE)
x1          <- x1[-c(3)]                                        # Remove "Mastertel" column
names(x1)   <- c("name", "num", "mrtservice", "datetime", "inout", "turn", "turninout", "zero")

date_n_time <- strsplit(x1[ , 4], " ")                          # Split date_n_time column
x1$x.date   <- sapply(date_n_time, "[", 1)
x1$x.time   <- sapply(date_n_time, "[", 2)
x1          <- x1[-c(4)]                                        # Remove date_n_time column

x2          <- x1[c("name", "num", "mrtservice", "x.date", "x.time", "inout", "turn", "turninout", "zero")]

write.table(x2, file = file.new, quote = FALSE, sep = ";", row.names = FALSE, col.names = FALSE)
close(file.old)
close(file.new)