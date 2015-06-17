# transpose tables 3 - 15, of the Mystic River data project
#Peter Olsen
# April, 2015
#
require(stringr)
require(lubridate)

#setwd("/Users/Peter/Documents/MysticProject/testbed/DataProjectPDFtoUsableTables/TABLE 3")
rm(list=ls())   # remove all variables in Enviornment

## Function ##
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

#Fuction to Extact data from one file from a Table
ExtractOneFile <- function (origFile,numRowSkip,numRows = 43) {
  origData <- read.table (file = origFile, header = FALSE, sep = ",", skip=numRowSkip, na.strings = c(""))
  #print(origData)
  results.col <- as.integer(1)
  #print(results.col)
  for (table.row in  seq(2, numRows, 1)) {           # assign row  (was 2,37, 4)
    for (table.col in 2:ncol(origData)){          # assign collum   ## 8 should be table width ncol(origData) #for (table.col in 2:8){ 
      
      stationName <- toString(origData[table.row,1])
      print(paste("get:",stationName,"numRowSkip Value:",numRowSkip))
      if ((toString(origData[table.row,table.col]) == "NA")|| (stationName == "NA")) {   # skip lines with no data
          next
        }
      
      stationName <- toString(origData[table.row,1])
      # check if station name is O.K
      if (!(grepl("[A-Z]{2,3}[0-9]{2}[A]{0,1}$",stationName))) {
        #if (grepl("A",st.name)) {
        stop(paste("Bad Station Name: table:",origFile,"value:",stationName)) 
      }
      print(stationName)
      stationNameC <- toString(StationNameChange(stationName, df.LocationNameChange))  #change location/station names
     
      surveyTimestamp <- toString(origData[1,table.col])
      surveyData <- toString(origData[table.row,table.col])
      
      if ((stationNameC =="NA") || (surveyTimestamp == "NA") || ( surveyData =="NA")){
        next   #skip lines with NA values
      }
     
      l <- c(stationName, stationNameC, surveyTimestamp, surveyData )  # create a vector of data
      print(l)
      #write vector to data frame results.frame if it exists, create one if it does not
      # this will add it as a column, which will be tansposed to a row later on
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


#t.info <- list( files = 2, dir = 'Table 3', file.one = 16, label = "Chem Oxy DD") # List of info for table to be processed
##FUNCTION TO PROCESS ONE TABLE##
ProcessOneTable <- function ( t.table.number ,
                              t.number.files,
                              t.label,
                              t.first.file ){
  
  new.dir <- paste("/Users/Peter/Documents/MysticProject/testbed/DataProjectPDFtoUsableTables/Table",table.number)
  setwd(new.dir)
  
  ## extract data from all the files in a table directory and combine them into one data frame
  for (files.num in 0:(t.number.files - 1)) {
    
    origFile <- paste(t.first.file + files.num, ".csv", sep = "")  # establish file name to process each file in dir
    
    if (file.exists(origFile)){
    print(paste("Processing",origFile))
    # call function to extact data from file
    r.frame1 <- ExtractOneFile(origFile,4, 55)   #variables: file, rows to skip, rows to process
    } else {
      next
    }
    
    ## determine if combined frame exists - should not for first file - create if not, join with existing frame if so
    if (exists("r.frame") ) {                  
      r.frame <- join_frames( r.frame, r.frame1)
    } else { 
      r.frame <- r.frame1
    }
  }
  
  
  df.save <- as.data.frame(t(r.frame))  # transpose frame from columns of data to rows of data
  
  #add column names
  colnames(df.save) <- c("OriginalLocationID","LocationID","Datetime",toString(t.label))
  
  #df.save <- subset(df.save, select = -c(drop))  # remove the filler column (was a row) we needed to add above
  
  file.name <- paste("Table",t.table.number, "mod.csv", sep = "")  # use the direcory name/table name in the file name 
  
  # write the results out to a file, from processing one or more files to a single csv file with linear usable data
  write.table(df.save, file = file.name,                
              append = FALSE, quote = TRUE, sep = ",", 
              eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, 
              qmethod = c("escape", "double"), fileEncoding = "")
}


##MAIN SCRIPT##

##Establish list of station/Location names and replacements so they can changed
LocationNameChange.File <- "/Users/Peter/Documents/MysticProject/testbed/DataProjectPDFtoUsableTables/scripts/LocationChangeData.csv"  
df.LocationNameChange <- read.table (file = LocationNameChange.File, header = FALSE, sep = ",", skip=1, na.strings = c(""))

##establish list of Table names and relevant info
Table.NameInfo.File <- "/Users/Peter/Documents/MysticProject/testbed/DataProjectPDFtoUsableTables/scripts/TableNamesInfo.csv"  

if (file.exists(Table.NameInfo.File)){
  df.Table <- read.table (file = Table.NameInfo.File, header = FALSE, sep = ",", skip=1, na.strings = c(""))
} else {
  next
}

#for (table.row in  seq(2, numRows, 1)) { 
for (t.num in 16:22) {
  
  table.number <- t.num
  label <- df.Table[t.num,2]
  first.file <- df.Table[t.num,3] + 2
  number.files <- df.Table[t.num+1,3] - df.Table[t.num,3] 
  
  
  print(label)
  print(first.file)
  print(number.files)
  
  ProcessOneTable(table.number,
                  number.files ,
                  label ,
                  first.file)
}





#table.number <- 3
#number.files <- 2
#label <- "Chem Oxy DD"
#first.file <- 16

#ProcessOneTable(table.number,
#                number.files ,
#                label ,
#                first.file)


