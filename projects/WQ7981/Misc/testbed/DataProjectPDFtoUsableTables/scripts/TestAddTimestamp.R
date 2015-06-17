
require(stringr)
TimeStampFromDateLoc <- function(f.date, f.loc, f.frame){
  for (row in 1:nrow(timestamps)){
    if ((f.date == f.frame[row,5])  && (f.loc == f.frame[row,2])) {
      return(f.frame[row,4])
    }
    
  }
  
}

j <- TimeStampFromDateLoc("2/26/81","UPL001",timestamps)



setwd("~/Documents/MysticProject/testbed/DataProjectPDFtoUsableTables/scripts")
Timestamps.File <- "/Users/Peter/Documents/MysticProject/testbed/DataProjectPDFtoUsableTables/scripts/DateTimestamps.csv"  
timestamps <- read.table (file = Timestamps.File, header = FALSE, sep = ",", skip=1, na.strings = c(""))


#test timestamp

require(lubridate)
tbl.date <- "1/1/1979"
new.date <- mdy(tbl.date)
print(new.date)
