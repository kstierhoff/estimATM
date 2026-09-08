:: Execute batch file to sync data from GCP
call "%~dp0sync-gcp-2606RL.bat"

:: Define the location of the R and Quarto executables
SET R_PATH="C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe" 
SET Q_PATH="C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe"

:: Define Rstudio Connect server
set "CONNECT_SERVER=https://test-connect.fisheries.noaa.gov/"

:: Define Rstudio Connect product IDs
set "trackSurvey_id=87109f5f-f740-48b5-bb6a-b37b835105f4"
set "plotSurvey_id=79f7f077-af34-4094-864b-ed54eb2ae5fc"
set "plotTrawls_id=ef078863-7a8d-4f94-b950-9a73b98e09b8"
set "checkTrawls_id=421bb82c-b5d7-4dd6-a2f5-c93dad060edb"
set "plotCTD_id=08f14d90-7f13-4469-b049-b0ab61d39361"

:: Define the path to the text file containing the Rstudio Connect API key
set "KEY_FILE=C:\KLS\CODE\rsconnect-api-key.txt"

:: Check if the file exists
if not exist "%KEY_FILE%" (
  echo Error: %KEY_FILE% not found.
  exit /b 1
)

:: Read the first line of the file into a variable
set /p CONNECT_API_KEY=<"%KEY_FILE%"

:: Trim any accidental trailing spaces
set "CONNECT_API_KEY=%CONNECT_API_KEY: =%"

:: To install pandoc on Windows, run CMD.exe then winget install JohnMacFarlane.Pandoc

:: Path to where the RMD and QMD files are located
SET WORK_DIR="C:\KLS\CODE\Github\estimATM\2606RL\Doc"

CD /d %WORK_DIR%

:: Name of plotSurvey Rmd
SET RMD_FILE="plotSurvey.Rmd"
%R_PATH% -e "rmarkdown::render('%RMD_FILE%', output_format='html_document')"
:: Open resulting file
start "" "%~dp0..\..\Doc\plotSurvey.html"

:: Name of checkTrawls Rmd
SET RMD_FILE="checkTrawls.Rmd"
%R_PATH% -e "rmarkdown::render('%RMD_FILE%', output_format='html_document')"
:: Open resulting file
start "" "%~dp0..\..\Doc\checkTrawls.html"

:: Name of plotTrawls Rmd
SET RMD_FILE="plotTrawls.Rmd"
%R_PATH% -e "rmarkdown::render('%RMD_FILE%', output_format='html_document')"
:: Open resulting file
start "" "%~dp0..\..\Doc\plotTrawls.html"

echo --------------------------------------------------
  echo Publishing trackSurvey Quarto document to Connect 
echo --------------------------------------------------

:: Name of trackSurvey Qmd
SET QMD_FILE="trackSurvey.qmd"
:: Publish file to Rstudio Connect
%Q_PATH% publish connect "%QMD_FILE%" --id "%trackSurvey_id%" --no-prompt --no-browser
:: Open resulting file
start "" "%~dp0..\..\Doc\trackSurvey.html"

:: Keeps window open to show results
pause
