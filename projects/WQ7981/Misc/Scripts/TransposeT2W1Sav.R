# transpose table 2, Mystic River data project
require(stringr)
require(lubridate)

#FUCNTION#
convert.time <- function(tbl.date, tbl.time = "000"){    #function to take date and time and return a timestamp
  # Format time so it can be used
  
  if (tbl.time == "NA"){
    tbl.time <- "0000"
  }
     
  st.min <- str_sub(tbl.time,-2,-1)  # gets min  - 1st and second digit from right
  st.hour <- str_sub(tbl.time, 1, -3)  #gets hours - 1st digit from Left to 3rd digit from right
  # can handle 1 or 2 digit hour values
  new.time <- paste(st.hour, st.min, sep=":")  # combine to create hour:min  "11:50"
  #new.time                                      # print result
  
  #Format Date so it can be used and is in the 1900's
  new.date <- as.Date(tbl.date, format = "%m/%d/%y") # rearanges date to ""1979-12-12"
  #with as.Date - /79 is 1979 not 2079
  #new.date                                 # print result
  
  #paste date and time together
  new.dt <- paste(new.date, new.time, sep=" ")  # gives format "1979-12-12 11:50"
  #new.dt                                         # print result
  
  final.date <- toString(ymd_hm(new.dt, tz = "EST"))       # gives "1979-12-12 11:50:00 EST" ; desired format
  #final.date
  return(final.date)
} # end convert time function

##MAIN SCRIPT##
setwd("/Users/Peter/Documents/MysticProject/testbed/R/transTest")
origFile <- "12.csv"
origData <- read.table (file = origFile, header = FALSE, sep = ",", skip=3, na.strings = c(""))
results.col <- as.integer(1)

for (table.row in  seq(2, 37, 4)) {           # assign row
  for (table.col in 2:8){          # assign collum
    stationName <- toString(origData[table.row,1])
    surveyDate <- toString(origData[1,table.col])
    surveyTime <- toString(origData[table.row,table.col])
    #surveyDate
    surveyTimestamp <- toString(convert.time(surveyDate, surveyTime))  # call function 
    #surveyTimestamp
    surveyTemp <- toString(origData[table.row + 1,table.col])
    surveyOxy <- toString(origData[table.row + 2,table.col])
    surveySat <- toString(origData[table.row + 3,table.col])
    Na <- "NA"  # filler value need to keep the same number of rows(columns)
      #write to vector
    l <- c(stationName, surveyTimestamp, surveyTemp, surveyOxy, surveySat, Na )
    #writ to data frame
    if (exists("results.frame") ) {
      results.frame[,results.col] <- c(l)
    } else { 
      results.frame <- data.frame(l)
    }
    results.col <- results.col + 1  # Move to next col
  }    #end for col
} # end for row    

df.save <- as.data.frame(t(results.frame))  # transpose frame

#add colnames
colnames(df.save) <- c("LocationID","Datetime","TEMP_WATER","DO", "DO_SAT", "drop")

df.save <- subset(df.save, select = -c(drop))  # remove the filler column (was a row) we needed to add above

write.table(df.save, file = "table02mod.csv",                # write the results out to a csv file
            append = FALSE, quote = TRUE, sep = ",", 
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, 
            qmethod = c("escape", "double"), fileEncoding = "")

