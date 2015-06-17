require(stringr)
dateFromTimeStamp <- function(f.timestamp){
  
  return(str_trim(gsub('(.*)[].*[ ].*','\\1',f.timestamp)))

}


DateLocationTimeStamp.csv

Timestamps.File <- "/Users/Peter/Documents/MysticProject/testbed/DataProjectPDFtoUsableTables/scripts/DateLocationTimeStamp.csv"  
timestamps <- read.table (file = Timestamps.File, header = FALSE, sep = ",", skip=1, na.strings = c(""))


#timestamps$NewColumn <- apply(timestamps[,c('V4')], 1,  function(x) { (str_trim(gsub('(.*)[ ].*[ ].*','\\1',V4))) } )
l <- vector(mode="numeric", length=0)

for (rows in 1:nrow(timestamps)){
  
  date <- dateFromTimeStamp(toString(timestamps[rows,4]))
  l <- append(l, date) 
  print(l)
  
  
}


timestamps[,5] <- c(l)