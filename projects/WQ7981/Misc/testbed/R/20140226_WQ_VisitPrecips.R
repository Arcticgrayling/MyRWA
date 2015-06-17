library(RODBC)
library(ggplot2)
library(reshape2)
library(plyr)
library(lubridate)
library(gridExtra)
library(scales)

rm(list=ls())
# odbcCloseAll()

getwd()

# Load Precip File

DB_PATH <- 
  "C:/Users/Patrick/Dropbox/MysticDB/Processed/Precip/LoganPrecip.xlsx"
ch<- odbcConnectExcel2007(DB_PATH)

# Check Table Name
sqlTables(ch)

# Fetch Tables
tblPrecip <- sqlFetch(ch, sqtable = "Processed precipitation",
                     na.strings = "NA", 
                     as.is = TRUE)                    
odbcCloseAll()

# parse dates
tblPrecip$Datetime <- ymd_hms(tblPrecip$Datetime)

# round to hour
tblPrecip$DateTime <- round_date(tblPrecip$Datetime, "hour")

head(tblPrecip)
summary(tblPrecip)
str(tblPrecip)

#Developing 48 hour precipitation summaries
# vPrecip <- tblPrecip$Precip
# vPrecip.48 <- filter(vPrecip,rep(1,48), side = 1)
# tail(vPrecip.48)
# summary(vPrecip)
# summary(vPrecip.48)
# plot(vPrecip.48)
# #bind vector back to dataframe
# tblPrecip$Precip.48 <- vPrecip.48
# head(tblPrecip, 50)

for (i in c(12, 24, 36, 48, 60, 72)){
  tblPrecip[, paste('Precip',i,sep='.')] <- filter(tblPrecip$Precip, rep(1, i), side=1)
}

# Load Water Quality Database

DB_PATH <- "C:/Users/Patrick/Dropbox/MysticDB/MysticDB_20140224.accdb"
ch<- odbcConnectAccess2007(DB_PATH)

tblVisit <- sqlFetch(ch, 'Visit', as.is=TRUE)
tblVisit$Datetime <- ymd_hms(tblVisit$Datetime) # parse date
tblVisit$DateHour <- round_date(tblVisit$Datetime,"hour")

head(tblVisit$DateHour)
tail(tblVisit$DateHour)
summary(tblVisit$DateHour)










  
  write.table(transform(tblPrecip, 
                        DateTime=as.character(DateTime),
                        Datetime=as.character(Datetime)), 
              "c:/Users/Patrick/VisitPrecip.txt", sep="\t")














tblPrecip$Precip.48 <- filter(tblPrecip$Precip, rep(1,48), side = 1)

# Load Water Quality Database

DB_PATH <- "C:/Users/Monitoring Director/Dropbox/MyRWA Database V.2/MysticDB_20130405.accdb"
ch<- odbcConnectAccess2007(DB_PATH)

#sqlTables(ch, tableType='TABLE')           #  Not sure of meaning of these comments         
#sqlColumns(ch, 'Result')
#sqlColumns(ch, 'Visit')
#fetch Results table

# Fetch Tables

tblResult <- sqlFetch(ch, 'Result')
tblResult$CharacteristicID <- factor(toupper(tblResult$CharacteristicID)) # change pH to PH
tblCharacteristic <- sqlFetch(ch, 'Characteristic')
tblVisit <- sqlFetch(ch, 'Visit', as.is=TRUE)
tblVisit$Datetime <- ymd_hms(tblVisit$Datetime) # parse date
tblLocation <- sqlFetch(ch, 'Location')
tblProject <- sqlFetch(ch, 'Project')
tblResult_Flag <- sqlFetch(ch, 'ResultFlag')

# join tables and check fields along the way
x <- tblResult
dim(x)
head(x)
summary(x)

# join Result Table to Charactericstic Table
x <- merge(x, 
           tblCharacteristic, 
           by.x='CharacteristicID', by.y='ID', all.x=T)
dim(x)
summary(x)
str(x)
# join Result AND Charactericstic to Visit Table
x <- merge(x, 
           tblVisit[, c('ID', 'SampleType', 'Datetime',
                        'LocationID', 'ProjectID')], 
           by.x='VisitID', by.y='ID', all.x=T)
dim(x)
summary(x)


# subset data
dim(x)
x <- subset(x, ProjectID =='BASE')
x <- subset(x, SampleTypeID =='S')
x <- subset(x, is.na(FlagID) | FlagID %in% c("", " "))
x <- subset(x, LocationID== "ABR006" | LocationID== "ABR028" | LocationID== "ABR049")
dim(x)

# round Datetime to nearest hour
x <- x[,c('CharacteristicID','ResultValue', 'Units','Datetime','LocationID')]
x$DateHour <- round_date(x$Datetime,"hour")
head(x$DateHour)
tail(x$DateHour)
summary(x$DateHour)


# join WQData to precipitation data
WQPrecip <- merge(x, 
                  tblPrecip[,c('DateTime','Precip.48')], 
                  by.x='DateHour', by.y='DateTime', all.x=T)
dim(WQPrecip)
head(WQPrecip)
tail(WQPrecip)
summary(WQPrecip)
str(WQPrecip$LocationID)


library(xlsReadWrite)
write.xls(transform(WQPrecip, 
                    DateHour=as.character(DateHour),
                    Datetime=as.character(Datetime)), 
          "c:/Users/Monitoring Director/WQPrecip.xls")

write.table(transform(WQPrecip, 
                      DateHour=as.character(DateHour),
                      Datetime=as.character(Datetime)), 
            "c:/Users/Monitoring Director/WQPrecip.txt", sep="\t")






# which row has highest Precip.48
WQPrecip[which.max(WQPrecip$Precip.48), ]


# Create binary bin based on 48 h precipitation
WQPrecip$Weather <- factor(ifelse(WQPrecip$Precip.48>=0.25, "Wet", "Dry"))

WQPrecip <- subset(WQPrecip, !is.na(Weather))

theme_set(theme_bw()) # drop gray background





WQPrecip$LocationID2<-factor(WQPrecip$LocationID, levels=c("MIC004", "CHR95S"))
x$LocationID2<-factor(x$LocationID, levels=c("MIC004", "CHR95S"))
WQPrecipMC <- subset(WQPrecip, !is.na(LocationID2))

ggplot(subset(subset(WQPrecip, CharacteristicID== "NO23"), LocationID2 == "MIC004"), 
       aes(DateHour, ResultValue, colour = Weather)) +
       geom_point()



ggplot(subset(WQPrecipMC, CharacteristicID== "NO23"), 
              aes(DateHour, ResultValue, colour = Weather)) + 
              geom_point(size = 4) +ylim(0,2)+ facet_grid(. ~ LocationID2)+ scale_y_continuous('NO23 (mg/l)') + 
              theme(axis.title.x = element_text(size = 18, colour = 'black', angle = 0), 
                   axis.title.y = element_text(size = 18, colour = 'black', angle = 90))+
              ggtitle("Nitrite + Nitrate levels at Mill Creek and Chelsea Creek (2008- 2012)")+ 
              theme(plot.title = element_text(lineheight=.8, size = 18))
                   
ggplot(subset(WQPrecipMC, CharacteristicID== "TP"), 
       aes(DateHour, ResultValue, colour = Weather)) + 
  geom_point(size = 4) + facet_grid(. ~ LocationID2)+ scale_y_continuous('TP (mg/l)',limits = c(0, 0.5))+
  geom_smooth(method="lm", alpha = 0.1)+
  theme(axis.title.x = element_text(size = 18, colour = 'black', angle = 0), 
       axis.title.y = element_text(size = 18, colour = 'black', angle = 90))+
  ggtitle("Total Phosphorus Levels at Mill Creek and Chelsea Creek (2008- 2012))")+ 
  theme(plot.title = element_text(lineheight=.8, size = 18))





d=data.frame(x1=c("1/1/2002"), x2=c("1/1/2013"), y1=c(235), y2=c(235), t=c('a'), r=c(1))
#geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="black", alpha=0.5)

P1 <- ggplot(subset(subset(WQPrecip, CharacteristicID== "ECOLI"), LocationID == "WIB001"),
       aes(DateHour, ResultValue, colour = Weather)) + 
  geom_point(size = 4) +   scale_y_log10('Cells/100ml')+
  geom_smooth(method="lm", alpha = 0.1)

P1 <- P1+ geom_rect(aes(NULL, NULL, xmin = as.POSIXct("2002-01-01 06:00:00"), 
                  xmax = as.POSIXct("2012-12-31 06:00:00"), 
              ymin = 0, ymax = 235), fill = "pink", alpha = 0.01)
P1

date <- c("2002-01-01","2011-09-20")
price <- c(235,1260)
tmp <- data.frame(date,price)

mytext <- data.frame(mydate = as.POSIXct(c('2012-01-10', '2012-01-20')),
                     col1 = c(120, 120), col2 = c(140,140), col3 = c(160,160))


P2 <- ggplot() +
      geom_rect(aes(NULL, NULL, xmin = as.POSIXct("2001-01-01 06:00:00"), 
                             #xmax = as.POSIXct("2013-12-31 06:00:00"), 
                             ymin = 0, ymax = 235), fill = "light green", alpha = 0.2) +
      geom_rect(aes(NULL, NULL, xmin = as.POSIXct("2001-01-01 06:00:00"), 
                xmax = as.POSIXct("2013-12-31 06:00:00"), 
                ymin = 235, ymax = 1260), fill = "dark orange" , alpha = 0.12) +
      geom_rect(aes(NULL, NULL, xmin = as.POSIXct("2001-01-01 06:00:00"), 
                xmax = as.POSIXct("2013-12-31 06:00:00"), 
                ymin = 1260, ymax = +Inf), fill = "pink" , alpha = 0.2) +
      scale_y_log10('Cells/100ml') +
      scale_x_datetime(limits = c(as.POSIXct("2002-01-01 06:00:00"), as.POSIXct("2012-12-31 06:00:00")))+
      geom_point(data=subset(subset(WQPrecip, CharacteristicID== "ECOLI"), LocationID == "MAR036"), 
             aes(DateHour, ResultValue, colour = Weather, size = 4))+ 
      geom_smooth(data=subset(subset(WQPrecip, CharacteristicID== "ECOLI"), LocationID == "MAR036"), 
             aes(DateHour, ResultValue, colour = Weather),method="lm", alpha =0)
P2 <- P2 + annotate("text", x=as.POSIXct("2002-09-01 06:00:00"), y=75, label="Safe \n for swimming", color = "dark green", size = 9)+
           annotate("text", x=as.POSIXct("2002-09-01 06:00:00"), y=600, label="Safe \n for boating", color = "dark orange", size = 9)+
           annotate("text", x=as.POSIXct("2002-09-01 06:00:00"), y=10000, label=" Not safe for \n recreation", color = "dark red", size = 9)
P2  
 


geom_text(data=mytext, x= mydate, y = col1, label="Something of note")

P2 <- P2 + geom_text(data = NULL, x = as.POSIXct("2001-01-01 06:00:00"), y = 200, label = "Safe for Swimming")+
P2

geom_smooth(method="lm", alpha = 0.1)
scale_x_datetime(xmin = as.POSIXct("2002-01-01 06:00:00"),xmax = as.POSIXct("2012-12-31 06:00:00") )++
  scale_y_log10('Cells/100ml')+
  geom_smooth(method="lm", alpha = 0.1)

g = ggplot(unrate.df) + geom_line(aes(x=date, y=UNRATE)) + theme_bw()
g = g + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2)


  theme(axis.title.x = element_text(size = 18, colour = 'black', angle = 0), 
        axis.title.y = element_text(size = 18, colour = 'black', angle = 90))+
  ggtitle("ECOLI Levels at WInn Brook in Belmont, MA")+ 
  theme(plot.title = element_text(lineheight=.8, size = 18))



  
  
              labs(title="Nitrite+Nitrate levels at Mill Creek and Chelsea River", size = 18)
opts(title = my_title,plot.title=theme_text(size=title.size))

opts(axis.title.y = theme_text(size = 18, colour = 'black', angle = 90))+

p + geom_point(size = 4)+ylim(0,5)+ facet_grid(. ~ LocationID2)+ scale_y_continuous('NO23 (mg/l)')



ggplot(subset(WQPrecip, aes(x= LocationID, y = ResultValue)), CharacteristicID == 'TP') + geom_point()
p <- ggplot(subset(subset(WQPrecip,CharacteristicID == "ENT"),LocationID2 == c("MIC004", "CHR95S")), aes(DateHour, ResultValue, colour = Weather))

p <- ggplot(subset(WQPrecip,CharacteristicID == "ENT"), aes(factor(year(Datetime)), ResultValue))
p + geom_boxplot(aes(fill = factor(LocationID)))+ scale_y_log10()+ plot + theme(axis.title.y = element_text(colour = 'red', angle = 45, size = 10, hjust = -0.2, vjust = 0.5, face = 'italic'))
  labs(title="Enterococcus")+ facet_grid(. ~ LocationID)
#p + facet_grid(. ~ LocationID)





lab <- log(c(0.01, 1.01, 10.01, 100.01, 1000.01, 10000.01, 100000.01)) 
names <- c(0,1,10,100,1000,10000,100000)
 
txt1 <- data.frame(x = 1/1/2013, y = 106, lab = "106")

ggplot(subset(WQPrecip, CharacteristicID=="ECOLI"), aes(LocationID, ResultValue, fill=Weather)) +
  geom_boxplot() +
  scale_y_log10()

ggplot(subset(WQPrecip, CharacteristicID=="NO23"), aes(LocationID2, ResultValue, fill=Weather)) +
  geom_boxplot() +
  scale_y_log10()

ggplot(subset(subset(WQPrecip, CharacteristicID=="NO23"),LocationID==c("MIC004", "CHR95S")),  aes(factor(year(DateHour)), ResultValue, fill=Weather)) +
  geom_point() + facet_grid(. ~ LocationID)+ scale_y_continuous('NO23 (mg/l)')
  scale_y_log10()




p <- ggplot(subset(subset(WQPrecip,CharacteristicID == "ENT"),LocationID == c("MIC004", "CHR95S")), aes(DateHour, ResultValue, colour = Weather))
p + geom_point(size = 4)+ scale_y_log10("Enterococcus #/100 ml",breaks=c(1,10,100,1000,10000,100000),labels=c(1,10,100,1000,10000,100000))+
  labs(title="")+ geom_hline(yint=350, colour = "red", size = 0.8)+ geom_hline(yint=104, colour = "orange", size = 0.8)+
  geom_smooth(method="lm", alpha = 0)+
  facet_grid(. ~ LocationID2)

p <- ggplot(subset(subset(WQPrecip,CharacteristicID == "NO23"),LocationID == c("MIC004", "CHR95S")), aes(DateHour, ResultValue, colour = Weather))
p + geom_point(size = 4)+ylim(0,5)+ facet_grid(. ~ LocationID2)+ scale_y_continuous('NO23 (mg/l)')

p <- ggplot(subset(subset(x,CharacteristicID == "NO23"),LocationID == c("MIC004", "CHR95S")), aes(DateHour, ResultValue))
p + geom_point(size = 4)+ylim(0,5)+ facet_grid(. ~ LocationID2)+ylim(0,2)+scale_y_continuous('Nitrate (NO23-N) (mg/l)')

p <- ggplot(subset(subset(x,CharacteristicID == "TP"),LocationID == c("MIC004", "CHR95S")), aes(DateHour, ResultValue))
p + geom_point(size = 4)+ylim(0,5)+ facet_grid(. ~ LocationID2)+ ylim(0,2) +scale_y_continuous('Total Phosphorus(mg/l)')


+ geom_smooth(method="lm") + geom_hline(yint=104) + geom_hline(yint=350)
#p + facet_grid(. ~ LocationID)




ggplot(subset(WQPrecip, CharacteristicID=="ECOLI"), aes(LocationID, ResultValue, fill=Weather)) +
  geom_boxplot() +
  scale_y_log10()

p <- ggplot(subset(subset(subset(WQPrecip,LocationID== "WIB001"),CharacteristicID == "ECOLI"),Weather=="Dry"), aes(factor(year(Datetime)), ResultValue))+ 
  geom_point()


ggplot(subset(WQPrecip, CharacteristicID=="ENT"), aes(LocationID, ResultValue, fill=Weather)) +
  geom_boxplot() +
  scale_y_log10()

ggplot(subset(WQPrecip, CharacteristicID=="ENT"), aes(LocationID, ResultValue, fill=Weather)) +
  geom_boxplot() +
  scale_y_log10()

p <- ggplot(subset(subset(WQPrecip,LocationID== "WIB001"),CharacteristicID == "ECOLI"), aes(factor(year(DateHour)), ResultValue))+ 
  geom_point()+ 
  labs(title="ECOLI concentrations at Winn Brook")+ylim(10,5000)+
  geom_abline(intercept = 235, slope = 0, aes(color= "blue"))+
  geom_abline(intercept = 1260, slope = 0, aes(color= "blue"))
p

p <- ggplot(subset(WQPrecip,CharacteristicID == "ECOLI"), aes(factor(year(DateHour)), ResultValue))
p + geom_bar(aes(fill = factor(LocationID)))+ labs(title="ECOLI concentrations across Upper Mystic Baseline Sites")+ facet_grid(. ~ LocationID) + 
  geom_abline(intercept = 235, slope = 0, aes(color= "blue"))+
  geom_abline(intercept = 1260, slope = 0, aes(color= "blue"))
p
#p + facet_grid(. ~ LocationID)


ggplot(subset(WQPrecip, CharacteristicID=='ECOLI'), 
       aes(ResultValue, color=LocationID)) +
         geom_density() + 
         facet_wrap(~LocationID, scales = 'free') + xlim(0,10000)



ggplot(WQPrecip, aes(LocationID, ResultValue, fill=Weather)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~CharacteristicID, scales="free")



p.box <- function(characteristic, logY) {
  p <- ggplot(subset(WQPrecip, CharacteristicID==characteristic), aes(factor(year(Datetime)), ResultValue, fill=Weather)) +
    geom_boxplot() +
    facet_wrap(~LocationID) +
    theme_bw()
  if (logY) p <- p + scale_y_log10()
  p
}
p.box("ECOLI", TRUE)
p.box("TP", TRUE)

manipulate(p.box(characteristic, logY),
           characteristic = picker("ECOLI", "TP", "SPCOND"),
           logY = checkbox(TRUE))



ggplot(subset(WQPrecip, CharacteristicID=="ECOLI"), aes(Datetime, ResultValue, color=Weather)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_y_log10() +
  geom_hline(yint=235) +
  facet_wrap(~LocationID)

ggplot(subset(WQPrecip, CharacteristicID=="ECOLI"), aes(Precip.48, ResultValue, color=Weather)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  scale_y_log10() +
  facet_wrap(~LocationID)


table(year(WQPrecip$Datetime), WQPrecip$CharacteristicID)

ggplot(tblPrecip, aes(factor(year(DateTime)), Precip)) + 
  stat_summary(fun.y=sum, geom="bar")

WQPrecip$ltStandard <- as.numeric(WQPrecip$ResultValue < 235)

ggplot(subset(WQPrecip, CharacteristicID=="ECOLI"), aes(factor(year(Datetime)), ltStandard)) + 
  stat_summary(fun.y=function(x) { sum(x)/length(x) }, geom="bar") +
  facet_wrap(~LocationID)

ggplot(subset(WQPrecip, CharacteristicID=="ECOLI"), aes(factor(year(Datetime)), ltStandard, fill=Weather)) + 
  stat_summary(fun.y=function(x) { sum(x)/length(x) }, geom="bar") +
  facet_wrap(~LocationID)



#-----------------------------------
x <- 1:10 # same as [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
y <- filter(x, rep(1, 3)) # rep(1, 3) is the same as c(1, 1, 1)
this returns a ts object y = [NA  6  9 12 15 18 21 24 27 NA]
so each value of y is the rolling sum of three values in x, with the sum 'centered'
y[i] = 1*x[i-1] + 1*x[i] + 1*x[i+1]
the first and last values of y are NA because x[-1] and x[11] don't exist

to get the sum of the previous three values for position i, use the side argument
y <- filter(x, rep(1, 3), side=1)
return y = [NA  NA 6 9 12 15 18 21 24 27]
where y[i] = 1*x[i-2] + 1*x[i-1] + 1*x[i]

you can also compute the mean by change the filter from [1, 1, 1] to [1/3, 1/3, 1/3]
y <- filter(x, rep(1, 3)/3, side=1)
return y = [NA  NA 2  3  4  5  6  7  8  9]
where y[i] = (1/3)*x[i-2] + (1/3)*x[i-1] + (1/3)*x[i] = ( x[i-2] + x[i-1] + x[i] ) / 3