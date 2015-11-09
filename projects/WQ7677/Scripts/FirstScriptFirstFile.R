## Mystic River Watershed Association
##  WQ7677  - Mystic River Water Quality Data Report 1976 or 1977
## Project to convert data from to a usable format
##  Peter Olsen June, 2015
## This script focuses on transposing the first table into a usable format
## and save to a file that will be the bases for adding all the additional data

require(stringr)
require(lubridate)


rm(list=ls())   # Clean: remove all variables in Enviornment

reportDates <- c("3/21/1977", "5/2/1977", "7/5/1977", "9/6/1977")
reportStations <- c(17,18,19,20,21,22)  #list of stations from report


convert.time <- function(tbl.date, tbl.time){    #function to take date and time and return a timestamp
  # Format time so it can be used in MyRWA data base.  Handles time in "hhmm" or "hmm" format
  
  if (tbl.time == "NA"){
    tbl.time <- "0000"
  }
  
  st.min <- str_sub(tbl.time,-2,-1)  # gets min  - 1st and second digit from right
  st.hour <- str_sub(tbl.time, 1, -3)  #gets hours - 1st digit from Left to 3rd digit from right
  #  handles 1 or 2 digit hour values
  
  new.time <- paste(st.hour, st.min, sep=":")  # combine to create hour:min  "11:50"
  #new.time                                      # print result, for testing
  
  #Format Date so it can be used and is in the 1900's
  new.date <- as.Date(tbl.date, format = "%m/%d/19%y") # rearanges date to ""1979-12-12"
  #with as.Date - /79 is 1979 not 2079
  #new.date                                 # print result, for testing
  
  #paste date and time together
  new.dt <- paste(new.date, new.time, sep=" ")  # gives format "1979-12-12 11:50"
  #new.dt                                         # print result, for testing
  
  final.date <- toString(ymd_hm(new.dt, tz = "EST"))       # gives "1979-12-12 11:50:00 EST" ; desired format
  #final.date
  return(final.date)
} # end convert time function


StationNameChange <- function(st.name,df.stnames){  
  for (reportRow in 1:nrow(df.stnames)){
    if (df.stnames[reportRow,1] == st.name) {
      return( df.stnames[reportRow,2])
    }
  }
  stop(paste("missing", st.name))  ## if no match found stops the program
}



ExtractOneFile <- function (origFile,numRowSkip) { #paramaters: file to process and number of rows at top to skip
  origData <- read.table (file = origFile, header = FALSE, sep = ",", skip=numRowSkip, na.strings = c(""))
  print(origData)
  results.col <- as.integer(1)
  #print(results.col)
  for (table.row in  seq(2, nrow(origData), 3)) {           # assign row  (was 2,37, 4)
    for (table.col in 3:ncol(origData)){          # assign collum
      
      
      origStationName <- toString(origData[table.row,1])
      surveyDateI <-  table.col-2 #get column postion to use for date
      surveyTime <- toString(origData[table.row,table.col])
      surveyTemp <- str_trim(toString(origData[table.row + 1,table.col]))
      surveyOxy <- str_trim(toString(origData[table.row + 2,table.col]))
      
      
      
      if ((origStationName == "NA") || !(is.element(origStationName,reportStations))){
        next  # if the station name is null or not in the list of stations, assume you are at end and move on
      }
      
      surveyDate <- reportDates[[surveyDateI]] #get date from list above, based on column
      
      #checkStationName(origStationName,origFile)  # check station name
      stationName <- toString(StationNameChange(origStationName,df.LocationNameChange))  # change station name
      
     
      #Combine Date and Time into one value
      surveyTimeStamp <- toString(convert.time(surveyDate, surveyTime))  # call function from above
      
     
      #surveySat <- str_trim(toString(origData[table.row + 3,table.col]))
      #Na <- "NA"  # filler value need to keep the same number of rows(columns)
      #write to vector
      
      
      l <- c(stationName,  surveyDate,  surveyTimeStamp, surveyTemp, surveyOxy )
      print(l)
      #write to data frame
      if (exists("results.frame") ) {
        results.frame[,results.col] <- c(l)
      } else { 
        results.frame <- data.frame(l)
      }
      results.col <- results.col + 1  # Move to next col
    }    #end for col
  } # end for row   
  return(results.frame)
} # end function  ExtractOneFile




##MAIN SCRIPT##
#Establish file locations
wd <- "~/Documents/MyRWA/projects/WQ7677/"
scriptsLoc <- paste(wd,"scripts/", sep = "")
dataLoc <- paste(wd,"Data/", sep = "")
resultsLoc <- paste(wd,"Results/", sep = "")

##Establish list of station/Location names and replacements so they can changed, from file
LocationNameChange.File <- paste(dataLoc,"LocationChangeData.csv", sep = "")  
df.LocationNameChange <- read.csv (file = LocationNameChange.File)
#na.strings = c("")

## Establish list of Table names and relevant table info from file 
Table.NameInfo.File <- paste(dataLoc,"TableInfo.csv", sep ="") 
df.Table <- read.table (file = Table.NameInfo.File, header = FALSE, sep = ",", skip=1, na.strings = c(""))

setwd(wd)
currentFile <- "./Data/Table1.csv"
# call function to extact data from file
r.frame <- ExtractOneFile(currentFile,3)   #variables: file, rows to skip,

df.save <- as.data.frame(t(r.frame))  # transpose frame from columns of data to rows of data

#add column names
colnames(df.save) <- c("OrigStationName","Date","Surveytime","TEMP_WATER","DO" )

#df.save <- subset(df.save, select = -c(drop))  # remove the filler column (was a row) we needed to add above

print(df.save)
df.save[is.na(df.save)] <- ""
print(df.save)

setwd(resultsLoc)
write.table(df.save, file = "RESULTS.csv",                # write the results out to a csv file
            append = FALSE, quote = TRUE, sep = ",", 
            eol = "\n", na = "", dec = ".", row.names = FALSE, col.names = TRUE, 
            qmethod = c("escape", "double"), fileEncoding = "")
