
##Establish list of table names and numbers and pages, we can 
Table.NameInfo.File <- "/Users/Peter/Documents/MysticProject/testbed/DataProjectPDFtoUsableTables/scripts/TableNamesInfo.csv"  

if (file.exists(Table.NameInfo.File)){
df.Table <- read.table (file = Table.NameInfo.File, header = FALSE, sep = ",", skip=1, na.strings = c(""))
} else {
  next
}

#for (table.row in  seq(2, numRows, 1)) { 
for (t.num in 3:11) {
  
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
  ProcessOneTable(table.number,
                number.files ,
                label ,
                first.file)
