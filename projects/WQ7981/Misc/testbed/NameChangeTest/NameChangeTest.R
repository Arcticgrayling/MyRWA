StationNameChange <- function(st.name,df.stnames){
  #new.dir <- paste("/Users/Peter/Documents/MysticProject/testbed/)
  #setwd(new.dir)
  
  t <- "16"
  line <- "2"
  if (!(grepl("[A-Z]{2,3}[0-9]{2}[A]{0,1}$",st.name))){
  #if (grepl("A",st.name)) {
  stop(paste("Bad Station Name: table:",t,"line:",line)) 
 
  }
       
 
  
  for (reportRow in 1:nrow(df.stnames)){
    if (df.stnames[reportRow,1] == st.name) {
     return( df.stnames[reportRow,2])
    }
  }
}

# establish file name to process each file in dir
StationNames.File <- "/Users/Peter/Documents/MysticProject/testbed/DataProjectPDFtoUsableTables/scripts/LocationChangeData.csv"  
#Names.File <- ()
df.names <- read.table (file = StationNames.File, header = FALSE, sep = ",", skip=1, na.strings = c(""))

x <- StationNameChange("MY16A",df.names)
print(x)