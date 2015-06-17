Attribute VB_Name = "Module1"
Sub transpose()


'
' transpose "TABLE 2" files into a usable linear format
'
'
     Dim file As Variant, WS As Workbook, SWS As Workbook
     Dim FromSheet As Worksheet, ToSheet As Worksheet
     Dim Filename As String, SurveyDate As String, SaveFile As String, SaveLine As Integer
     
     Dim X As Integer, Y As Integer
     
     'directory table files are ing
     Dim Dirname As String: Dirname = "Macintosh HD:Users:Peter:Documents:MysticProject:testbed:"
    
      'loop through table files
  ' file = Dir(Dirname)
   'While (file <> "")
    file = "12.xlsx"  ' 1st file for dev purposes
    SaveFile = "SaveData.xlsx"
   'If InStr(file, ".xls") > 0 Then
   
       Filename = CStr(file)
       Set WS = Workbooks.Open(Dirname & Filename)  'Open Table file
       Set FromSheet = WS.Sheets("Table 1")
       Set SWS = Workbooks.Open(Dirname & SaveFile) 'Open Table to Save data
       Set ToSheet = SWS.Sheets("Sheet1")
       
       'TableName = WS.Sheets("Table 1").Range("A1").Value                            'Get value of A1 Where tables are labeled
        TableName = FromSheet.Range("A1").Value
        SaveLine = 2
       For X = 1 To 9
            Line = (4 * X) + 1
              For Y = 2 To 8
                Col = Y + 1
                
                StationName = FromSheet.Range("A" & Line).Value
                SurveyDate = FromSheet.Cells(4, Y).Value     'Cells(6,1) equals A6
                SurveyTime = FromSheet.Cells(Line, Y).Value
                STemp = FromSheet.Cells(Line + 1, Y).Value
                SOxy = FromSheet.Cells(Line + 2, Y).Value
                SSat = FromSheet.Cells(Line + 3, Y).Value
                
                'MsgBox (" Station Name " & StationName & " Date is " & SurveyDate & " Time: " & SurveyTime & " Temp: " & STemp)
                
                ToSheet.Cells(SaveLine, 1) = StationName
                ToSheet.Cells(SaveLine, 2) = SurveyDate
                ToSheet.Cells(SaveLine, 3) = SurveyTime
                ToSheet.Cells(SaveLine, 4) = STemp
                ToSheet.Cells(SaveLine, 5) = SOxy
                ToSheet.Cells(SaveLine, 6) = SSat
                SaveLine = SaveLine + 1
             Next Y
       Next X
       
       
End Sub
