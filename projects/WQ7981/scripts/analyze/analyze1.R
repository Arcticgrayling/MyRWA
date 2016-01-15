###Analysis of data realted to report WQ7981
### Peter Olsen - 10-2015
###  Graphs etc. to compare data in report to other data for sites
###  refferenced in report
###  Using ideas from Exporatory Data Analysis Class - John Hopkins/Coursera
###


library(ggplot2)
library(xlsx)

rm(list=ls())   # Clean: remove all variables in Enviornment


wd <- "~/Documents/MyRWA/projects/WQ7981/"
scriptsLoc <- paste(wd,"scripts/analyze/", sep = "")
dataLoc <- paste(wd,"data/fullSiteDataFromDB/", sep = "")
resultsLoc <- paste(wd,"results/", sep = "")

setwd(wd)

## Load data, csv file from xlxs file given to me by Andy H. via email
## from extraction of MYrWA database

site.data <- paste(dataLoc,"siteData.csv", sep = "")  

site.df <- read.csv(site.data, stringsAsFactors=FALSE)

## ADD A YEAR COLUMN
require(lubridate)  # for year()
site.df <- mutate(site.df, year = year(strptime(site.df$Datetime, "%m/%d/%Y")))

#as.Date(site.df$Datetime)
#year(site.df$Datetime)

#as.POSIXlt(D)
#year(D)

#
#toString()
#D <- strptime(site.df$Datetime, "%m/%d/%Y")

# somewhat works  #D <- strptime(site.df$Datetime, "%m/%d/%y")
#site.df$Datetime <- D


#s <- subset(site.df, Datetime == "8/6/79" & LocationID == "ABR006")
##1
#s <- subset(site.df, LocationID == "ABR006" & CharacteristicID == "NH3")
#with(s, plot(Datetime, ResultValue))

##2
#s2 <- subset(site.df, LocationID == "ABR006" & CharacteristicID == "NH3")
#with(s2, plot(Datetime, ResultValue))
#qplot(Datetime, ResultValue, data = s2)

##3 Amonia improvement over time  ABR049
s3 <- subset(site.df, LocationID == "ABR049" & CharacteristicID == "NH3")

#t3 <- s3 %>%
#        group_by(Datetime) %>%
#        summarise_each(funs(mean))

g <- ggplot(s3, aes(Datetime, ResultValue))
g + geom_point() +
        geom_smooth(method = "lm", aes(group = 1)) 
        
##4    
s <- subset(site.df, LocationID == "ABR049" & Datetime < "")
s4<- subset(site.df, LocationID == "ABR049" & CharacteristicID == "SALINITY")

g <- ggplot(s4, aes(Datetime, ResultValue))
g + geom_point() +
        geom_smooth(method = "lm") 


## base Plot function ##  with(s3, plot(Datetime, ResultValue))
#qplot(Datetime, ResultValue, data = s3) ##simple ggplot2
#model <- lm(Datetime ~ ResultValue, s3)
#abline(model, lwd =2)


##misc
sm <- subset(site.df, LocationID == "ABR049" & CharacteristicID == "NH3")
with(sm, plot(Datetime, ResultValue))
# Location IDs: ABRN01 ABR070 HAB001  ABR049  ABR036

#hist(s$ResultValue)
#qplot(s, s$ResultValue, s$Datetime)