Attribute VB_Name = "Module2"
Sub SortFiles()
Attribute SortFiles.VB_ProcData.VB_Invoke_Func = " \n14"
'
' sortFiles Macro to Sort excel files with tables from the excel files with out tables
' based on label in cell A1
'
     Dim file As Variant, WS As Workbook, SaveDir As String
     Dim Filename As String
     Dim TableName As String, srcFile As String
     'main directory
     Dim Dirname As String: Dirname = "Macintosh HD:Users:Peter:Documents:MysticProject:ExcelFilesSorted:"
     ' directory to move files that are not part of tables
     Dim NotATable As String: NotATable = "Macintosh HD:Users:Peter:Documents:MysticProject:ExcelFilesSorted:NotATable:"
     'directory to move tables
     Dim TableDir As String: TableDir = "Macintosh HD:Users:Peter:Documents:MysticProject:ExcelFilesSorted:Tables:"
   
   file = Dir(Dirname)
   While (file <> "")
   
     If InStr(file, ".xls") > 0 Then
       Filename = CStr(file)
       Set WS = Workbooks.Open(Dirname & Filename)  'Open file
       TableName = Range("A1").Value                            'Get value of A1 Where tables are labeled
       WS.Close SaveChanges:=False                              ' close file for now
       
        If TableName Like "TABLE ##*" Or TableName Like "TABLE #*" Then     'Determine if File is a table
                 TableName = RTrim(Left(TableName, 8))  '  trim to just 8 char TABLE ##  and trim tailing space if just one number
                 SaveDir = TableDir & TableName                 'determine directory to save table
                 
                 If FileOrFolderExistsOnMac(2, SaveDir) = False Then  ' see 3rd party function below, create directory for tables if they don't exist
                    MkDir SaveDir
                 End If
                
                srcFile = Dirname & Filename
                FileCopy srcFile, SaveDir & ":" & Filename
                
         
         Else  ' file is not a table
               ' move to Not a table directory
             FileCopy Dirname & Filename, NotATable & Filename
             
        
        End If
           
      End If
     file = Dir
     
  Wend
  ' Option to remove files from main directory since they have moved,
  ' done at end to not interupt file <> loop
  DeleteFilesInFolder (Dirname)
End Sub

Function FileOrFolderExistsOnMac(FileOrFolder As Long, FileOrFolderstr As String) As Boolean
'By Ron de Bruin
'30-July-2012
'Function to test whether a file or folder exist on a Mac.
'Uses AppleScript to avoid the problem with long file names
    Dim ScriptToCheckFileFolder As String
    ScriptToCheckFileFolder = "tell application " & Chr(34) & "Finder" & Chr(34) & Chr(13)
    If FileOrFolder = 1 Then
        ScriptToCheckFileFolder = ScriptToCheckFileFolder & "exists file " & _
                                  Chr(34) & FileOrFolderstr & Chr(34) & Chr(13)
    Else
        ScriptToCheckFileFolder = ScriptToCheckFileFolder & "exists folder " & _
                                  Chr(34) & FileOrFolderstr & Chr(34) & Chr(13)
    End If
    ScriptToCheckFileFolder = ScriptToCheckFileFolder & "end tell" & Chr(13)
    FileOrFolderExistsOnMac = MacScript(ScriptToCheckFileFolder)
End Function




Function DeleteFilesInFolder(FolderWithFiles As String)
    ' Modified from Ron DeBriuns work http://www.rondebruin.nl/mac/mac012.htm
    'Dim FolderWithFiles As String: FolderWithFiles = dirToDelete
    Dim scriptToRun As String

    'FolderWithFiles = MacScript("return (path to documents folder) as string") & "Test:"
    
    ' Or enter the full path to the folder.
    ' FolderWithFiles = "Macintosh HD:Users:YourUserName:Desktop:Test:"

    If Right(FolderWithFiles, 1) <> ":" Then
        FolderWithFiles = FolderWithFiles & ":"
    End If
    
    scriptToRun = scriptToRun & "tell application " & Chr(34) & _
                  "Finder" & Chr(34) & Chr(13)
    scriptToRun = scriptToRun & _
               "do shell script ""rm "" & quoted form of posix path of " & _
               Chr(34) & FolderWithFiles & """ & " & Chr(34) & "*" & Chr(34) & Chr(13)
    scriptToRun = scriptToRun & "end tell"

    On Error Resume Next
    MacScript (scriptToRun)
    On Error GoTo 0
End Function
