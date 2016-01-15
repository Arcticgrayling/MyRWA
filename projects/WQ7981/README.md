#WATER QUALITY REPORT EXTRACTION PROJECT WQ7981  
##PETER OLSEN, 2015


### The Project
This project extracts water quality data on the Mystic River watershed from a Report
labeled:   
"The Mystic River, Water Quality Data - 1979, 1980 and 1981,  
Massachusetts department of environmental quality engineering  
Division of Water Pollution Control  
Thomas C. McMahon, Director   

This was a typed report saved as pdf files, one file per page.

There are 33 tables within the report that contain data.

The goal is to extract the data from this report and format it so that it can be imported  
into the Mystic River Watershed Associations Data base.

The data is processed to modify location names to use location names in current use in  
the database,  excludes  
data from sites that are no longer available (sites that have been filled in)  
and modifies data reporting units to reflect actual reporting standards.


###Data:
The data was given to me as a set of pdf files, one file for each page of the report.
The original raw data can be found here:  data/Mystic-River-Water-Quality-Data-1979-1981/
PDF Pages

The data needs to be extracted from the pdf files.
Adobe acrobat was used to extract all the pdf to excel formated files


###Results:
WQ7981FullReportReshapedNoBlankRows.csv
Is in a format that could be imported into the MyRWA database.  This format also meets  
tidy data standards.

###Scripts:


AcrobatPDFtoExcel.txt describes how to create a script in Acrobats GUI
 to convert a group of PDF files to Excel format
ConvertPDFtoExcel.sequ  is an export of the Acrobat script used to convert all the files
to .xlsx format.

This script converted the pdf files to .xlsx formated files.  These files are located
at /data/1_Excel_pages

The long file names were a problem and were converted to short names
using apple script
data/2_ExcelShortNames


scripts/SortTableFiles.bas  is a VBA script that separated the data tables 
from other reprot pages.  The files for each table where put into the 
data/3_ExcelShortNamesSortedByTableCSVFormat/Tables/Table(X) folders for each Table.
The script also converted each file to a csv format to be better used by R.


R scripts:
TransposeTable2.R  - needs to be run first to create rows with timestamps from table 2
TransposeT3_T111.R  -  run second to add data from other tables to the results table


###Process
The process followed for this project is a little convoluted.
Scripts were used in Adobe Acrobat, Applescript, Excel macros/vba, and R.

The pdfs are converted to .xlsx format with Adobe acrobat.
The file names are shortened using Apple script.
The .xlsx files are sorted and converted to .csv format using VBA.
R is used to extract the data from the relevant files and put in a tidy data file.
WQ7981FullReportReshapedNoBlankRows.csv

This data was imported into MyRWAs database.


