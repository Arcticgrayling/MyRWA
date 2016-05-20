### Process the data for WQ72
### Mystic River Watershed Association, Peter Olsen, 12/2015


rm(list=ls())   # Clean: remove all variables in Enviornment

##load libraries

library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)
library(lubridate)

##Set working directories
wd <- "~/Documents/MyRWA/projects/WQ70/"
##scriptsLoc <- paste(wd,"scripts/analyze/", sep = "")
##dataLoc <- paste(wd,"data/", sep = "")
setwd(wd)

raw <- read.csv("WQ70.csv", header=FALSE, stringsAsFactors = FALSE)


timeStamp <- function(date){
        
        time <- "12:00"
        new.date <- mdy(date)
        
        tStamp <- paste(new.date, time, sep=" ")
        
        final.date <- toString(ymd_hm(tStamp, tz = "EST"))       # gives "1979-12-12 11:50:00 EST" ; desired format
        #final.date
        return(final.date)
}

TempValue <- function(tempF){
        if (tempF == "" | tempF == " ")
                return("")
        t <- as.integer(str_sub(tempF, 0, 2)) # parse first two numbers
        c <- 5/9 * (t - 32)  # convert to Celcius
        c <- round(c,4)
        return(c)
}

phValue <- function(ph){
        if (ph == "" | ph == " ")
            return("")
        phv <- as.numeric(str_sub(ph, -3, -1))
        #print(phv)
        return(phv)      
}

## Create a data frame to save data in corrected formate
d<- data.frame()

## loop through rows to sort through data
    for (i in 1:nrow(raw)){
            if (grepl("Station",raw[i,1])){    #Find stations data
                #print(raw[i,1])
                n <- i
                
                while((raw[n+1,2] != "") & (n < nrow(raw)) ){
                        #print(n)
                        #print(raw[n+1,2])
                       datev <- raw[n+1,2]
                        #print(raw[n+1,3])
                       sample <- raw[n+1,3]
                        #print(raw[n+1,4])
                        #print(TempValue(raw[n+1,4]))
                        tempC <- TempValue(raw[n+1,4])
                        #print(raw[n+1,5])
                       ph <- phValue(raw[n+1,5])
                       
                       ## save to data frame
                       d[n,1] <- raw[i,1]
                       d[n,2] <- timeStamp(datev)
                       d[n,3] <- sample
                       d[n,4] <- tempC
                       d[n,5] <- ph
                       
                        n <- n + 1
                }
            }
    }

colnames(d) <- c("Station","Datetime", "Fcoli","Temp C", "Ph")
results <- d[complete.cases(d),]  ## remove rows without data

## create CSV file with data

write.table(results, file = "WQ70FullReportReshaped.csv",                
            append = FALSE, quote = TRUE, sep = ",", 
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, 
            qmethod = c("escape", "double"), fileEncoding = "")
