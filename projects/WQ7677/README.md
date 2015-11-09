## README
## Mystic River watershed association
## WQ7677  - Water quality report 76-77  

##The project:
  The data is from a report labeled  
###DWPC - MAPC 208 Monitoring Program  
### Aberjona River Basin"  
  
   
  
##Data:
Is located in the /Data directory.

The RAW data given is from a PDF file "1976-1977 WQ Aberjona River Basin.pdf" 
containing the report with a series of tables.
After the data is OCR'd/extracted, Tables were created - Table1.csv through Table21.csv.
These were cut and pasted from 1976-1977 WQ Aberjona River BasinPdftoExcel.xlsx
They were edited to correct errors from the OCR process.

The data also has been modified to correct the value Zero in some instances and the
the column names for some of the data. This is reflected in the file /Data/Tableinfo.csv

##Script:
Located in the /Scripts directory
FirstScriptFirstFile.R needs to be run first.  This file modifys Locations and creates 
timestamps from data and time info in the first file.  It produces a results file that it
used by the next script to create a completed, single, fairly tidy data set.

Tables2to21.R - this script adds data from the rest of the tables to the data from the 
first script.


##Results:
Located in the /Results Directory. 
WQ7677FullReportReshaped.csv is the file that is a complete, fairly tidy data set that is
in a format that can be used by MyRWA to add to their data base.

##Code Book:
Is located in the files codebook.md
It contains information on the variables and names in the data set.  

##Process:  
  Convert tables in pdf report to csv, used pdftoexcel.com utility to OCR and convert    
   whole report to excel format.   
  - Cut and paste tables from report to separate excel files, saved to CSV format  
  - Edit content to line up correctly for script, checked data against pdf and edited    
  as necessary to correct for errors in OCR process.
  - Run first script to establish timestamp and locations to use as a key along with  
  temp and oxy data. 
  - Run second script to add all additional data to a results file, using timestamp and  
  location to make the additions to the table in the correct row.
  -  The scripts also change column labels for some of the data
  - Zero values are changed to less than x numbers in some instances to more accurately
indicate that the substance wasn't detected because if fell below a detectable 
threshold and not that it did not exist.
  
##Misc:
##Notes:
There is a zero value for 5 day BioChemical Oxygen Demand that has been left at Zero.
