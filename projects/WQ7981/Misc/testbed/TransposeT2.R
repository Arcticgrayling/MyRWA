# transpose table 2, Mystic River data project

setwd("/Users/Peter/Documents/MysticProject/testbed/R/transTest")
origFile <- "12.csv"
origData <- read.table (file = origFile, header = FALSE, sep = ",", skip=3)
results.col <- as.integer(1)
for (table.row in  seq(2, 37, 4)) {           # assign row
  for (table.col in 2:8){          # assign collum
    stationName <- toString(origData[table.row,1])
    surveyDate <- toString(origData[1,table.col])
    surveyTime <- toString(origData[table.row,table.col])
    surveyTemp <- toString(origData[table.row + 1,table.col])
    surveyOxy <- toString(origData[table.row + 2,table.col])
    surveySat <- toString(origData[table.row + 3,table.col])
      #write to vector
    l <- c(stationName, surveyDate, surveyTime, surveyTemp, surveyOxy, surveySat)
      #writ to data frame
    if (exists("results.frame") ) {
      results.frame[,results.col] <- c(l)
    } else { 
      results.frame <- data.frame(l)
    }
    results.col <- results.col + 1
  }    #end for col
} # end for row    


