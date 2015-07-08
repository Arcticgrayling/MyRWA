## Mystic River Watershed Association
##  WQ7677  - Mystic River Water Quality Data Report 1977 conversion
## Project to convert data from Report to a usable format
##  Peter Olsen June, 2015
## This script transposes the remaining tables into a usable format
## and save to the results file that was created in "FirstScriptFirstFile.R". FristScriptFirstFile.R must be run first

require(stringr)
require(lubridate)


rm(list=ls())   # Clean: remove all variables in Enviornment, helpful for testing

reportDates <- c("3/21/1977", "5/2/1977", "7/5/1977", "9/6/1977")
reportStations <- c(17,18,19,20,21,22)  #list of stations from report

#########
findLine <- function (f.stationName, f.date, f.data ) {
  for (row in 1:nrow(RESULTS)){
    
    
    #print(paste("date:",f.date,"row",row,"RESULTS[row,1]",RESULTS[row,2]))
    if ((toString(f.stationName) == toString(RESULTS[row,1]))   
        && 
          ((toString(f.date)) == toString(RESULTS[row,2]))) {
      
        print("BINGO!!!!!")
      #print(paste("t2 station",df.t2[row,2],toString(df.t2[row,3])))
      print(paste("New table - date: ",toString(mdy(f.date)),"Station Name:",f.stationName,"data:",f.data,"row:",row))
      #df.t2[row,8] <- toString(f.data)
      print(paste("match found for row:",row))
      #stop()
      return(row)
    } 
    
  }# end for
} # end findLine function

#######
StationNameChange <- function(st.name,df.stnames){  
  print(paste("report name",st.name))
  for (reportRow in 1:nrow(df.stnames)){
    if (df.stnames[reportRow,1] == st.name) {
      print(paste("return name",df.stnames[reportRow,2]))
      return( df.stnames[reportRow,2])
    }
  }
  stop(paste("missing", st.name))  ## if no match found stops the program
}

########
changeZero <- function(table.number, t.date){  # from another project, some code won't be used
  if (! (table.number == 29)) {
    return(toString(df.Table[table.number,5]))  # this line should be only one used for this project
  } else {  #hanlde two dates and values for silver
    if (t.date == "5/13/1980") {
      return(toString(df.Table[table.number,6]))
    } else if (t.date == "10/28/1980"){
      return(toString(df.Table[table.number,7]))  
    }
  }
} # end of function changeZero()



#########Fuction to Extact data from one file from a Table
ProcessOneFile <- function (origFile,numRowSkip) {
  origData <- read.csv (file = origFile, header = FALSE, sep = ",", skip=numRowSkip, na.strings = c(""), stringsAsFactors = FALSE)
  print(origData)
  results.col <- as.integer(1)  # establish a varaible to keep track of columns in results frame 
  
  v1 <- vector(mode="character") # establish a vector to write data to and to add line to the results table
  
  for (table.row in  seq(2, nrow(origData), 1)) {           # assign row  (was 2,37, 4)
    for (table.col in 2:ncol(origData)){          # assign collum   ## 8 should be table width ncol(origData) #for (table.col in 2:8){ 
      
      OrigStationName <- toString(origData[table.row,1])
      surveyDateI <-  table.col-1 #get column postion to use for date
      surveyData <- toString(origData[table.row,table.col])
      
      if ((OrigStationName =="NA") ||  ( surveyData =="NA")){
        next   #skip lines with NA values
      }
     
      print(paste("row",table.row,"col",table.col,"date position",surveyDateI,"data",surveyData))
      #process data
      stationName <- toString(StationNameChange(OrigStationName, df.LocationNameChange))  #change location/station names
      surveyDate <- reportDates[[surveyDateI]] #get date from list above, based on column
      
      if ((surveyData == 0) || (surveyData == "0.00")){
        surveyData <- changeZero(t.num, surveyDate)
      }
      print(paste("info so far: station:", stationName, "Date:",surveyDate,"Data:",surveyData))
      #write data to a vector to be added to Results file
      
      line.num <- findLine(stationName, surveyDate, surveyData)
      print(paste("line.num",line.num))
      v1[line.num] <- surveyData  #add data to vector - to be added to table below
           
    }    #end for col
  } # end for row 
  print("end of loops")
  print(paste("v1 is:",v1))
  
  
  if (length(v1) == nrow(RESULTS)){
    print(v1)
    return(v1)
  } else{
    print(paste("v1",v1, "start:", length(v1),"end:",nrow(RESULTS)))
    stop ("vector not long enough")
    print(paste("start:", length(v1),"end:",nrow(RESULTS)))
    for (x in (length(v1)+1):nrow(RESULTS)){
    
      v1[x] <- ""
    
    }
  }
  return(v1)
  
} # end function 

#######MAIN SCRIPT#######
#Establish file locations
wd <- "~/Documents/MyRWA/projects/WQ7677/"
scriptsLoc <- paste(wd,"scripts/", sep = "")
dataLoc <- paste(wd,"Data/", sep = "")
resultsLoc <- paste(wd,"Results/", sep = "")

##Establish list of station/Location names and replacements so they can changed, from file
LocationNameChange.File <- paste(dataLoc,"LocationChangeData.csv", sep = "")  
df.LocationNameChange <- read.table (file = LocationNameChange.File, header = FALSE, sep = ",", skip=1, na.strings = c(""))
print(df.LocationNameChange)

## Establish list of Table names and relevant table info from file 
Table.NameInfo.File <- paste(dataLoc,"TableInfo.csv", sep ="") 
df.Table <- read.table (file = Table.NameInfo.File, header = FALSE, sep = ",", skip=1, na.strings = c(""))

## Set Location of RESULTS TABLE for writting to
Table.results <- paste(resultsLoc,"RESULTS.csv", sep ="")
df.results  <-  read.table (file = Table.results, header = TRUE, sep = ",",  na.strings = c(""))
RESULTS <- df.results

setwd(wd)

for (t.num in 2:21){
  #t.num <- 2
  print(paste("PROCESSING TABLE: ", t.num))
  p.file <- paste(dataLoc,"Table",t.num,".csv", sep ="")
  
  l <- ProcessOneFile(p.file,4)
  
  print(l)
  
  #SET Label - handle incomplete data in table
  if (is.na(df.Table[t.num,4])){
    print(paste(df.Table[t.num,4], df.Table[t.num,2]))
    label <- df.Table[t.num,2]  #lable of sample type eg. total Coliform etc.
  } else{
    label <- df.Table[t.num,4]
  }
  
  
  
  results.col <- t.num + 4  # establish column the results vector will go, using table numbers to increment
  RESULTS[,results.col] <- c(l)
  #colnames(df.save) <- c("OrigStationName","LocationID","SurveyDate","Datetime","TEMP_WATER","DO", )
  names(RESULTS)[t.num+4] <- toString(label)
}


write.table(RESULTS, file = "WQ7981FullReportReshaped.csv",                
            append = FALSE, quote = TRUE, sep = ",", 
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, 
            qmethod = c("escape", "double"), fileEncoding = "")




