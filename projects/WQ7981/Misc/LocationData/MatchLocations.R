# compare two csv files with geo locations, determines which location in one table is closest to each point in second table

setwd("/Users/Peter/Documents/MysticProject/LocationData")

# first list of locations 
ReportStations <- "ReportLocations.csv"
Report.frame <- read.csv (file = ReportStations, header = TRUE, sep = ",", skip=0, na.strings = c(""))

# Second list of locations
ExistStations <- "Locations_select_20150414.csv"
Exist.frame <- read.csv (file = ExistStations, header = TRUE, sep = ",", skip=0, na.strings = c(""))


# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)  from: http://www.r-bloggers.com/great-circle-distance-calculations-in-r/
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

r.name <- Report.frame[1,1]
r.lat <- Report.frame[1,3]
r.long <- Report.frame[1,4]

e.name <- Exist.frame[1,1]
e.lat <- Exist.frame[1,5]
e.long <- Exist.frame[1,6]


x.x <- gcd.hf(r.lat,r.long, e.lat,e.long)
rm(working.list)
results.col <- as.integer(1) # setup result table- column numbers

#for (reportRow in 1:15){
for (reportRow in 1:nrow(Report.frame)){
    for (existRow in 1:nrow(Exist.frame)){
    #for (existRow in 1:nrow(Exist.frame)){  
       distancer <- gcd.hf(Report.frame[reportRow,3], Report.frame[reportRow,4], 
                               Exist.frame[existRow,5], Exist.frame[existRow,6]) 
       distance <- format(round(distancer, 2), nsmall = 2)
       station.r <- Report.frame[reportRow,1]
       station.e <- Exist.frame[existRow,1]
        
      # if (! (distance == "NA")) {
      print(distance)
      #print(working.list$distance)
      if (!(exists("working.list"))){
        working.list <- list(reportStation=station.r, existingStation=station.e, distance=distance)
        print("workinglist")
      } else if (distance < working.list$distance){
        working.list$existingStation <- station.e 
        working.list$distance <- distance
      }
      
           #print(working.list$distance)      
    }
    #print(working.list$distance)
    #print(working.list$reportStation)
    #print(working.list$existingStation)
    l <- c( toString(working.list$reportStation), 
            toString(working.list$existingStation),
            toString(working.list$distance))
    #write to data frame
    if (exists("results.frame") ) {
      results.frame[,results.col] <- c(l)
    } else { 
      results.frame <- data.frame(l)
    }
    results.col <- results.col + 1  # Move to next col
    
    rm(working.list)
}

df.save <- as.data.frame(t(results.frame))  # transpose frame from columns of data to rows of data

#add column names
colnames(df.save) <- c("Report Station","Existing Station","Distance between")

write.table(df.save, file = "MatchStations.csv",                # write the results out to a csv file
            append = FALSE, quote = TRUE, sep = ",", 
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, 
            qmethod = c("escape", "double"), fileEncoding = "")






