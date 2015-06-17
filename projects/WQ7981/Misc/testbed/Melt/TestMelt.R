setwd("/Users/Peter/Documents/MysticProject/testbed/Melt/TABLE 33/")
t.file <- "58.csv"
t.frame <- read.csv (file = t.file, header = TRUE, sep = ",", skip=4, na.strings = c(""), check.names = FALSE)
t1 <- melt(t.frame)
print(t1)