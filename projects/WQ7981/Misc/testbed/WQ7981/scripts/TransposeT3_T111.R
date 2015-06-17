## Mystic River Watershed Association
##  WQ7981  - Mystic River Water Quality Data Report 1979, 1980, 1981
## Project to convert data from to a usable format
##  Peter Olsen April, 2015
## This script was used to transpose tables 3 - 33 of the Report
##

require(stringr)
require(lubridate)

#setwd("/Users/Peter/Documents/MysticProject/testbed/DataProjectPDFtoUsableTables/TABLE 3")
rm(list=ls())   # remove all variables in Enviornment

## Function ##
## Requires station(location) name from data and a data frame with replacement station names paired with 
## station names from report tables
StationNameChange <- function(st.name,df.stnames){  
  for (reportRow in 1:nrow(df.stnames)){
    if (df.stnames[reportRow,1] == st.name) {
      return( df.stnames[reportRow,2])
    }
  }
      stop(paste("missing", st.name))  ## if no match found stops the program
}

# Determine row to add data point
findLine <- function (f.stationName, f.date, f.data ) {
  
  
  for (row in 2:nrow(df.t2)){
    
    if ((f.stationName == df.t2[row,2]) && (toString(mdy(f.date)) 
                                           == toString(df.t2[row,3]))){
      #print("BINGO!!!!!")
      #print(paste("t2 station",df.t2[row,2],toString(df.t2[row,3])))
      #print(paste("New table - date: ",toString(mdy(f.date)),"Station Name:",f.stationName,"data:",f.data,"row:",row))
      #df.t2[row,8] <- toString(f.data)
      print(paste("match found for row:",row))
      return(row)
    } 
    
  }# end for
} # end findLine function

#Funtion to change Zero Values, use file with table info TableNamesInfo.csv to get values
changeZero <- function(table.number, t.date){
  if (! (table.number == 29)) {
   return(toString(df.Table[table.number,6]))
  } else {  #hanlde two dates and values for silver
      if (t.date == "5/13/1980") {
        return(toString(df.Table[table.number,6]))
      } else if (t.date == "10/28/1980"){
        return(toString(df.Table[table.number,7]))  
      }
    }
} # end of function changeZero()



#Fuction to Extact data from one file for a Table
ExtractOneFile <- function (origFile,t.num, vec1) {
  origData <- read.table (file = origFile, header = FALSE, sep = ",", skip=4, na.strings = c(""))
  print(origData)
  results.col <- as.integer(1)  # establish a varaible to keep track of columns in results frame 
  #vec1 <- vector(mode="character")  #, length=nrow(df.t2))
 
  for (table.row in 2:nrow(origData)) {           # assign row  (was 2,37, 4)
    for (table.col in 2:ncol(origData)){          # assign collum   ## 8 should be table width ncol(origData) #for (table.col in 2:8){ 
      
      stationName <- str_trim(toString(origData[table.row,1]))
      surveyDate <- str_trim(toString(origData[1,table.col]))
      surveyData <- str_trim(toString(origData[table.row,table.col]))
      
      #skip lines with no data
      if ((surveyData == "NA")|| (stationName == "NA")) {   # skip lines with no data
          next
        }
      
      # check if station name is O.K
      if (!(grepl("[A-Z]{2,3}[0-9]{2}[A]{0,1}$",stationName))) {
        #if (grepl("A",st.name)) {
        stop(paste("Bad Station Name: table:",origFile,"value:",stationName)) 
      }
      
      #change location/station names to a current one from csv file with table info
      stationNameC <- toString(StationNameChange(stationName, df.LocationNameChange))  #change location/station names
     
      if ((stationNameC =="NA") || (surveyDate == "NA") || ( surveyData =="NA")){
        next   #skip lines with NA values
      }
      
      #change data if it equals zero, use value for this table from csv file of table info
      
      if (surveyData == 0){
        surveyData <- changeZero(t.num, surveyDate)
      }
      
      
      #write to results table
      
      line.num <- findLine(stationNameC, surveyDate, surveyData)
      vec1[line.num] <- surveyData  #add data to vector - to be added to table below
     
    }    #end for col
  } # end for row   
  print(paste("vec1", vec1))
  return(vec1)  # return the updated vector
  
} # end function 

 
##FUNCTION TO PROCESS ONE TABLE##
ProcessOneTable <- function ( t.table.number ,
                              t.number.files,
                              t.label,
                              t.first.file ){
  
  new.dir <- paste(wd,"TABLE ",t.table.number, sep = "")
  print(paste(new.dir,"/",t.first.file,".csv",sep=""))
  veca <- vector(mode="character") 
  #vecb <- vector(mode="character")
 
  if (file.exists(paste(new.dir,"/",t.first.file,".csv",sep=""))){
    setwd(new.dir)
    
  } else {
    print("stop")
    return()
  }
  
  ## extract data from all the files in a table directory and combine them into one data frame
  for (files.num in 0:(t.number.files - 1)) {
    
    origFile <- paste(t.first.file + files.num, ".csv", sep = "")  # establish file name to process each file in dir
    
    if (file.exists(origFile)){
      print(paste("Processing",origFile," to ", new.dir))
      # call function to extact data from file
       veca <- ExtractOneFile(origFile, t.table.number, veca)   #variables: file, rows to skip, rows to process
    } else {
      next
    } 
    #lveca<- c(veca,vecb)
  }
  # handle short vectors that can't be added to RESULTS data frame
  if (length(veca) == nrow(RESULTS)){
    print(veca)
    return(veca)
  } else{
    #stop ("vector not long enough")
    print(paste("start:", length(veca),"end:",nrow(RESULTS)))
    for (x in (length(veca)+1):nrow(RESULTS)){
      
      veca[x] <- ""
     
    }
    return(veca)
  }
  

} #end function ProcessOneTable


##MAIN SCRIPT##

## SET Root Working Directory for project:
wd <- "~/Documents/MysticProject/testbed/WQ7981/"
scriptsLoc <- paste(wd,"scripts/", sep = "")

##Establish list of station/Location names and replacements so they can changed, from file
LocationNameChange.File <- paste(scriptsLoc,"LocationChangeData.csv", sep = "")  
df.LocationNameChange <- read.table (file = LocationNameChange.File, header = FALSE, sep = ",", skip=1, na.strings = c(""))

## Establish list of Table names and relevant table info from file 
Table.NameInfo.File <- paste(scriptsLoc,"TableNamesInfo.csv", sep ="") 
df.Table <- read.table (file = Table.NameInfo.File, header = FALSE, sep = ",", skip=1, na.strings = c(""))

## Set Location of Table 2 for writting
Table.2 <- paste(wd,"/TABLE 2/RESULTS.csv", sep ="")
df.t2  <-  read.table (file = Table.2, header = TRUE, sep = ",", skip=0, na.strings = c(""))
RESULTS <- df.t2
l <<- vector(mode="character", length=nrow(RESULTS))
#process the tables
for (t.num in 3:32) {   # process a range of tables
  
  table.number <- t.num

  #SET Lable - handle incomplete data in table
  if (is.na(df.Table[t.num,5])){
    print(paste(df.Table[t.num,5], df.Table[t.num,2]))
    lable <- df.Table[t.num,2]  #lable of sample type eg. total Coliform etc.
  } else{
    lable <- df.Table[t.num,5]
  }
  
  
  if (t.num == 11) {
    first.file <- 32
    number.files <- 2    
  } else{
  first.file <- df.Table[t.num,3] + 2
  number.files <- df.Table[t.num+1,3] - df.Table[t.num,3] 
  }
  
  
  #print(label)
  #print(first.file)
  #print(number.files)
  
  # get a vector of data values in date/location order so they will match up to results frame.
  l <- ProcessOneTable(table.number,
                  number.files ,
                  lable ,
                  first.file)
  print(l)
  
  #Add Collumn of new values to results frame
  results.col <- t.num + 5  # establish column results will go, using table numbers to increment
  RESULTS[,results.col] <- c(l)
  #colnames(df.save) <- c("OrigStationName","LocationID","SurveyDate","Datetime","TEMP_WATER","DO", "DO_SAT" )
  names(RESULTS)[t.num+5] <- toString(lable)
  
}
#write the results out to a file, from processing one or more files to a single csv file with linear usable data
setwd(wd)

RESULTS[is.na(RESULTS)] <- ""

write.table(RESULTS, file = "WQ7981FullReportReshaped.csv",                
            append = FALSE, quote = TRUE, sep = ",", 
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, 
           qmethod = c("escape", "double"), fileEncoding = "")

#df.save <- as.data.frame(t(r.frame))  # transpose frame from columns of data to rows of data

#add column names
#colnames(df.save) <- c("OriginalLocationID","LocationID","Datetime",toString(t.label))

#df.save <- subset(df.save, select = -c(drop))  # remove the filler column (was a row) we needed to add above

# create a results filename, use the direcory name/table name in the file name and label of sample type
#file.name <- paste("Table",t.table.number,"_", toString(t.label), "_mod.csv", sep = "")  # use the direcory name/table name in the file name 

# write the results out to a file, from processing one or more files to a single csv file with linear usable data
#write.table(df.save, file = file.name,                
#            append = FALSE, quote = TRUE, sep = ",", 
#            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, 
#            qmethod = c("escape", "double"), fileEncoding = "")
