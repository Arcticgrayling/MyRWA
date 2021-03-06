## Mystic River Watershed Association
##  WQ7981  - Mystic River Water Quality Data Report 1979, 1980, 1981
## Project to convert data from to a usable format
##  Peter Olsen April, 2015
## This script focuses on transposing table 2 into a usable format
##

require(stringr)
require(lubridate)

setwd("/Users/Peter/Documents/MysticProject/WQ7981/TABLE 2/")
rm(list=ls())   # Clean: remove all variables in Enviornment

##Establish list of station/Location names and replacements so they can changed
LocationNameChange.File <- "/Users/Peter/Documents/MysticProject/WQ7981/scripts/LocationChangeData.csv"  
df.LocationNameChange <- read.table (file = LocationNameChange.File, header = FALSE, sep = ",", skip=1, na.strings = c(""))


checkStationName <- function (st.name, origFile) {
  # first verify station name is O.K, if not stop program so correction can be made to data
  if (!(grepl("[A-Z]{2,3}[0-9]{2}[A]{0,1}$",st.name))) {
    #if (grepl("A",st.name)) {
    stop(paste("Bad Station Name: table:",origFile,"value:",st.name)) 
  }
}

## Function to replace station names (sampling sites) in report to useable existing station names ##
## needs station name from data and data frame with replacement station names paired with data station names
StationNameChange <- function(st.name,df.stnames){  

  for (reportRow in 1:nrow(df.stnames)){
    if (df.stnames[reportRow,1] == st.name) {
      return( df.stnames[reportRow,2])
    }
  }  
  stop(paste("missing", st.name))
}


#FUCNTION#
convert.time <- function(tbl.date, tbl.time){    #function to take date and time and return a timestamp
  # Format time so it can be used
  
  if (tbl.time == "NA"){
    tbl.time <- "0000"
  }
     
  st.min <- str_sub(tbl.time,-2,-1)  # gets min  - 1st and second digit from right
  st.hour <- str_sub(tbl.time, 1, -3)  #gets hours - 1st digit from Left to 3rd digit from right
  # can handle 1 or 2 digit hour values
  new.time <- paste(st.hour, st.min, sep=":")  # combine to create hour:min  "11:50"
  #new.time                                      # print result, for testing
  
  #Format Date so it can be used and is in the 1900's
  new.date <- as.Date(tbl.date, format = "%m/%d/%y") # rearanges date to ""1979-12-12"
  #with as.Date - /79 is 1979 not 2079
  #new.date                                 # print result, for testing
  
  #paste date and time together
  new.dt <- paste(new.date, new.time, sep=" ")  # gives format "1979-12-12 11:50"
  #new.dt                                         # print result, for testing
  
  final.date <- toString(ymd_hm(new.dt, tz = "EST"))       # gives "1979-12-12 11:50:00 EST" ; desired format
  #final.date
  return(final.date)
} # end convert time function

#Fuction to Extact data from one file from Table 2
ExtractOneFile <- function (origFile,numRowSkip) { #paramaters: file to process and number of rows at top to skip
  origData <- read.table (file = origFile, header = FALSE, sep = ",", skip=numRowSkip, na.strings = c(""))
  print(origData)
  results.col <- as.integer(1)
  #print(results.col)
  for (table.row in  seq(2, nrow(origData), 4)) {           # assign row  (was 2,37, 4)
    for (table.col in 2:8){          # assign collum
        #if (toString(origData[table.row,table.col]) == "NA") {   # skip lines with no data
        #  next
        #}
      
      origStationName <- toString(origData[table.row,1])
      checkStationName(origStationName,origFile)  # check station name
      stationName <- toString(StationNameChange(origStationName,df.LocationNameChange))  # change station name
      
      
      
      surveyDate <- toString(origData[1,table.col])
      surveyTime <- toString(origData[table.row,table.col])
      #Combine Date and Time into one value
      surveyTimestamp <- toString(convert.time(surveyDate, surveyTime))  # call function from above
      surveyTemp <- toString(origData[table.row + 1,table.col])
      surveyOxy <- toString(origData[table.row + 2,table.col])
      surveySat <- toString(origData[table.row + 3,table.col])
      #Na <- "NA"  # filler value need to keep the same number of rows(columns)
      #write to vector
      
      if ( (stationName == "NA")  ||
           (   (surveyTime == "NA") &&
               (surveyTemp == "NA") &&
               (surveyOxy == "NA") &&
               (surveySat == "NA") ) 
      )
      {  #if station Name is not one to include, or no useable data move to next line
        next
      }
      
      
      l <- c(origStationName, stationName, surveyTimestamp, surveyTemp, surveyOxy, surveySat )
      #print(l)
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
} # end function 

join_frames <- function (df1, df2) {  # Function to join two data frames
  #combine the data from the 2 files
  r.df <- union(df1, df2)
  names(r.df) <- union(names(df1), names(df2))
  r.df <- as.data.frame(r.df)
  return(r.df)
}

##MAIN SCRIPT##
# get 1st file for table 2

origFile <- "12.csv"
# call function to extact data from file
r.frame1 <- ExtractOneFile(origFile,3)   #variables: file, rows to skip,

# process 2nd file for table 2
origFile <- "13.csv"
# call function to extact data from file
r.frame2 <- ExtractOneFile(origFile,1)   #variables: file, rows to skip,

r.frame <- join_frames(r.frame1, r.frame2) #join first two frames

# process 3rd file for table 3
origFile <- "14.csv"
# call function to extact data from file
r.frame3 <- ExtractOneFile(origFile,1)   #variables: file, rows to skip,

r.frame <- join_frames(r.frame, r.frame3)  #join additional frames to main frame

# process 4th file for table 4
origFile <- "15.csv"
# call function to extact data from file
r.frame4 <- ExtractOneFile(origFile,1)   #variables: file, rows to skip,

r.frame <- join_frames(r.frame, r.frame4)  #join additional frames to main frame


df.save <- as.data.frame(t(r.frame))  # transpose frame from columns of data to rows of data

#add column names
colnames(df.save) <- c("OrigStationName","LocationID","Datetime","TEMP_WATER","DO", "DO_SAT" )

#df.save <- subset(df.save, select = -c(drop))  # remove the filler column (was a row) we needed to add above

write.table(df.save, file = "table02_Temp_DO_SAT_mod.csv",                # write the results out to a csv file
            append = FALSE, quote = TRUE, sep = ",", 
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, 
            qmethod = c("escape", "double"), fileEncoding = "")

