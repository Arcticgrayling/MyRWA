library(RODBC)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(ggmap)
library(wq)


setwd("C:/Users/Volunteer Intern/Dropbox/MysticDB")
source("./Rcode/Sandbox/Jeff/load_wq.R")
wq <- load_wq(exclude.qaqc.samples=FALSE)
source('./Rcode/Sandbox/Jeff/load_precip.R')
precip <- load_precip()
wq2 <- append_weather(wq,precip)

## 1.Preparing data for the "Sample data" sheet
#Select regular baseline (no field duplicates) data with DEP format required fields
BASEDATA <- wq2 %>% 
  select(WaterBodyID, LocationID, LocationDescription, Latitude, Longitude, 
         VisitID, Datetime, CharacteristicName, Units, ResultValue, 
         ProjectID, SampleTypeID) %>%
  filter(ProjectID == "BASE", SampleTypeID == "S")
BASEDATA <- BASEDATA %>% mutate(Watershed = "Boston Harbor: Mystic")
BASEDATA <- BASEDATA %>% mutate(FlowCondition = "Flowing")

#Extracting Date only from Datetime
BASEDATA$SampleDate <- as.Date(BASEDATA$Datetime)

#Extracting time only from Datetime
BASEDATA$SampleTime <- format(as.POSIXct(BASEDATA$Datetime, format="%Y-%m-%d %H:%M:%S"), format="%H:%M")

#Changing columns order to fit DEP template
BASEDATA <- BASEDATA[c("Watershed", "WaterBodyID", "LocationID", "LocationDescription",
                       "Latitude", "Longitude", "VisitID", "SampleDate", "SampleTime",
                       "FlowCondition", "CharacteristicName", "Units", "ResultValue")]

#Changing columns names to fit DEP template
colnames(BASEDATA) <- c("Watershed", "WaterBody", "StationID", "StationDescription",
                        "Latitude_WGS84", "Longitude_WGS84", "SampleID", "SampleDate",
                        "SampleTime", "FlowCondition", "Analyte","Units", "Result")

#Export BASEDATA as a text file
write.table(BASEDATA, "C:/Users/Volunteer Intern/Desktop/Basedata.txt", 
            sep="\t", row.names = FALSE)




## 2.Preparing data for the "field qc data" sheet - Field duplicates (and not Field blanks)
#Select FD data and expand results as required by DEP for this sheet
FD <- wq2 %>% 
  select(VisitID, Datetime, CharacteristicID, ResultValue, LocationID, 
         ProjectID, SampleTypeID) %>%
  filter(ProjectID == "BASE", SampleTypeID == "FD") %>%
  spread(CharacteristicID, ResultValue)

#Add a date only column
FD$SampleDate <- as.Date(FD$Datetime)

#Add a time only column
FD$SampleTime <- format(as.POSIXct(FD$Datetime, format="%Y-%m-%d %H:%M:%S"), format="%H:%M")

#Concatenate Date and Location which makes a unique couple for each FD
#Extract this new column as a Value
FD <- FD %>% mutate(Filt = paste(FD$LocationID, FD$SampleDate,sep="/"))
Filter <- FD[,"Filt"]

#Drop useless columns
drops <- c("Datetime", "LocationID", "ProjectID")
FD <- FD[,!(names(FD) %in% drops)]

#Select regular data and expand results as required by DEP for this sheet
#Add a date only column
#Add a time only column
S <- wq2 %>% 
  select(VisitID, Datetime, CharacteristicID, ResultValue, LocationID,
         ProjectID, SampleTypeID) %>%
  filter(ProjectID == "BASE", SampleTypeID == "S") %>%
  spread(CharacteristicID, ResultValue)
S$SampleDate <- as.Date(S$Datetime)
S$SampleTime <- format(as.POSIXct(S$Datetime, format="%Y-%m-%d %H:%M:%S"), format="%H:%M")

#Concatenate Date and Location as for FD data to be able to match each FD 
#with its matching regular sample
S <- S %>% mutate(Filt = paste(S$LocationID, S$SampleDate,sep="/"))

#Drop useless columns
drops <- c("Datetime", "LocationID", "ProjectID")
S <- S[,!(names(S) %in% drops)]

#Extract regular samples with a matching FD
SmatchFD <- subset(S, Filt %in% Filter)

#Make sure events are in the same order in FD and SmatchFD
FD <- FD[ order(FD[,"Filt"]), ]
SmatchFD <- SmatchFD[ order(SmatchFD[,"Filt"]), ]

#Add corresponding FD VisitID to regular samples and regular sample VisitID to FD
FD_VisitID <- FD$VisitID
SmatchFD <- SmatchFD %>% mutate(QCMatch = FD_VisitID)

SmatchFD_VisitID <- SmatchFD$VisitID
FD <- FD %>% mutate(QCMatch = SmatchFD_VisitID)

#Calculate RPD in a dataframe similar to FD and SmatchFD
c1 <- c("VisitID", "SampleTypeID")
c2 <- c("DO", "DO_SAT", "ECOLI", "ENT", "FCOLI", "NH3", "NO2", "NO23", "NO3", "PH",
        "SALINITY", "SPCOND", "TEMP_WATER", "TN", "TP", "TSS", "TURB")
c3 <- c("SampleDate", "SampleTime", "Filt", "QCMatch")
RPD <- data.frame(FD[,c1], (abs(FD[,c2]-SmatchFD[,c2])*2*100/(FD[,c2]+SmatchFD[,c2])), FD[,c3])

#Fill VisitID, SampleTypeID, and QCMatch with blanks to match DEP template
RPD$VisitID <- rep(c("RPD"),nrow(RPD))
RPD$SampleTypeID <- rep(c(""),nrow(RPD))
RPD$QCMatch <- rep(c(""),nrow(RPD))

#Merge dataframe of regular samples with matching FD, dataframe of FD, and dataframe of RPD
FDandMatchS <- rbind(SmatchFD, FD)
FD_Final <- rbind(FDandMatchS, RPD)

#Order rows to have S/FD/RPD in that order for each event
FD_Final <- FD_Final[ order(FD_Final[,"SampleDate"], FD_Final[,"Filt"]), ]

#Add Flow condition to meet DEP template requirements
FD_Final <- FD_Final %>% mutate(FlowCondition = "Flowing")

#Changing columns order to fit DEP template
FD_Final <- FD_Final[c("VisitID", "SampleTypeID", "QCMatch", "SampleDate",
                       "SampleTime", "FlowCondition", "DO", "DO_SAT", "ECOLI", "ENT", "FCOLI",
                       "NH3", "NO2", "NO23", "NO3", "PH", "SALINITY", "SPCOND", "TEMP_WATER",
                       "TN", "TP", "TSS", "TURB")]

#Changing columns names to fit DEP template
colnames(FD_Final) <- c("SampleID", "SampleType", "QCMatch", "SampleDate", "SampleTime", 
                        "FlowCondition", "Dissolved Oxygen", "Dissolved Oxygen, % saturation",
                        "Escherichia coli", "Enterococcus", "Fecal Coliform", "Ammonia", 
                        "Nitrite", "Nitrate + Nitrite", "Nitrate", "pH", "Salinity", 
                        "Specific conductance", "Water Temperature", "Total Nitrogen", 
                        "Total Phosphorus", "Total suspended solids", "Turbidity")

#Export FDandMatchS as a text file
write.table(FD_Final, "C:/Users/Volunteer Intern/Desktop/FDdata.txt", sep="\t", row.names = FALSE)