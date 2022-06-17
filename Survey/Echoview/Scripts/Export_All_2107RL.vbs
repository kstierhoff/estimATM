' This script is used to export:
' 	- Integration results (.csv files) for all frequencies from both CPS and krill
'	- Echogram images from the noise filtered 38 and 120 kHz echograms
'
' The CPS and krill EV files must be placed in separate directories so that the exported results come from the correct type (CPS or kill). 
' The exported images will also be made for both CPS and Krill EV files

''''''''''' USER INPUT(S)

' Define CPS and Krill EV folder names. Do NOT include \ at end
Dim FolderNameCPS: FolderNameCPS = "C:\SURVEY\2107RL\PROCESSED\EV\Files_to_Export\CPS"						' Folder where the CPS EV file to be processed is located
Dim FolderNameKrill: FolderNameKrill = "C:\SURVEY\2107RL\PROCESSED\EV\Files_to_Export\Krill"				' Folder where the Krill EV file to be processed is located
Dim CSVFolderName: CSVFolderName = "C:\SURVEY\2107RL\PROCESSED\EV\CSV\LASKER"								' Folder where the resulting CSV file will be stored
Dim ImagesFolderName: ImagesFolderName = "C:\SURVEY\2107RL\PROCESSED\EV\Exported_Images"					' Folder where the resulting echogram images will be stored

''''''''''' MAIN PROGRAM

' Define file system object, used for accessing files and directories
Dim fso: Set fso = CreateObject("Scripting.FileSystemObject")	

' Define Echoview object, used for communicating with Echoview
Dim EvApp: Set EvApp = CreateObject("EchoviewCom.EvApplication")	

' Get list of CPS and Krill EV files
Dim FilesCPS: Set FilesCPS = fso.GetFolder(FolderNameCPS).Files
Dim FilesKrill: Set FilesKrill = fso.GetFolder(FolderNameKrill).Files

' Loop through each CPS file, calling CPS-specific function
For Each File In FilesCPS
	ExportFromFileCPS FolderNameCPS & "\" & File.Name
Next

' Loop through each Krill file, calling Krill-specific function
For Each File In FilesKrill
	ExportFromFileKrill FolderNameKrill & "\" & File.Name
Next


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' A subroutine to export data from a CPS EV file
Sub ExportFromFileCPS(FileName)

	' If filename doesn't end in .EV or is a backup file, then don't process
	If (UCase(Right(FileName, 3)) <> ".EV" Or UCase(Right(FileName, 12)) = " (BACKUP).EV") Then
			Exit Sub
	End If
	
	' Open EV file
	Dim EvFile: Set EvFile = EvApp.OpenFile(FileName)
	
	' If unable to open EV file, alert user and exit function
	If EvFile Is Nothing Then
			MsgBox "Failed to open EV file '" & FileName & "'"
			Exit Sub
	End If
	
	' Call subroutine to export final CPS variables for each frequency
	ExportVariable EvFile, "Final 38 kHz CPS"
	' ExportVariable EvFile, "Final 70 kHz CPS"
	' ExportVariable EvFile, "Final 120 kHz CPS"
	' ExportVariable EvFile, "Final 200 kHz CPS"
	
	' Export screengrabs
	ExportEchogramToImage EvFile, "38 Remove Passive Pings"
	ExportEchogramToImage EvFile, "120 Remove Passive Pings"
	ExportEchogramToImage EvFile, "38 kHz CPS for Image Export"

	' Close the file
	EvApp.CloseFile(EvFile)
End Sub


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' A subroutine to export data from a CPS EV file
Sub ExportFromFileKrill(FileName)

	' If filename doesn't end in .EV or is a backup file, then don't process
	If (UCase(Right(FileName, 3)) <> ".EV" Or UCase(Right(FileName, 12)) = " (BACKUP).EV") Then
			Exit Sub
	End If
	
	' Open EV file
	Dim EvFile: Set EvFile = EvApp.OpenFile(FileName)
	
	' If unable to open EV file, alert user and exit function
	If EvFile Is Nothing Then
			MsgBox "Failed to open EV file '" & FileName & "'"
			Exit Sub
	End If
	
	' Call subroutine to export final CPS variables for each frequency
	' ExportVariable EvFile, "Final_38kHz_Krill"
	' ExportVariable EvFile, "Final_70kHz_Krill"
	ExportVariable EvFile, "Juan Krill Final 120"
	' ExportVariable EvFile, "Final_200kHz_Krill"
	
	' Export screengrabs
	' ExportEchogramToImage EvFile, "38kHz noise filtered"
	' ExportEchogramToImage EvFile, "120kHz noise filtered"

	' Close the file
	EvApp.CloseFile(EvFile)
End Sub


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Subroutine to export integration results (CSV file)
Sub ExportVariable(EvFile, VarName)

	' Get the Echoview object for that variable from the defined EV file
	Dim Var: Set Var = EvFile.Variables.FindByName(VarName)
	
	' If it couldn't be found, prompt user and exit
	If Var Is Nothing Then
			MsgBox "Failed to find variable '" & VarName & "' in EV file '" & EvFile.FileName & "'"
			Exit Sub
	End If
	
	' Get acoustic variable
	Dim VarAc: Set VarAc = Var.AsVariableAcoustic
	
	' If detected that it's not acoustic, alert user and exit
	If VarAc Is Nothing Then
			MsgBox "Variable '" & VarName & "' is not acoustic in EV file '" & EvFile.FileName & "'"
			Exit Sub
	End If
	
	' Create filename for CSV file
	Dim File: Set File = fso.GetFile(EvFile.FileName)
	OutFileName = CSVFolderName & "\" & Left(File.Name, Len(File.Name) - 3) & "-" & (VarName) & ".csv"
	
	' Try to export results to the created output file. If it fails, alert user and exit
	If Not VarAc.ExportIntegrationByCells(OutFileName, Nothing) Then
			MsgBox "Failed to export '" & VarName & "' by cells from EV file '" & EvFile.FileName & "'"
	End If
End Sub


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Subroutine to export echogram image file
Sub ExportEchogramToImage(EvFile, VarName)

	' Get Echoview object for acoustic variable
	Set objEvVariable = EvFile.Variables.FindByName(VarName).AsVariableAcoustic()

	' Create filename for PNG file
	Dim File: Set File = fso.GetFile(EvFile.FileName)
	OutFileName = ImagesFolderName & "\" & Left(File.Name, Len(File.Name) - 3) & "-" & (VarName) & ".png"
	
	' Export image
	objEvVariable.ExportEchogramToImage OutFileName, 1080, -1, 80000
End Sub