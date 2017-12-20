set CUR_PATH="C:\Users\mohit.bansal61089\My Projects\EP_Audit_Sheet"

:: Refresh Excel & Mail file
echo "Refreshing file"
cd %CUR_PATH%
cscript Refresh_Macro.vbs
::pause

:: Rename excel file
echo "Copying file & Timestamping"
cd %CUR_PATH%
PowerShell ./TimeStamping.ps1
::pause

:: Copying files to Repository
::SET YOUR SOURCE PATH
cd %CUR_PATH%
cd "Archives"
set SOURCE=%cd%
::SET PATH WHERE YOU WANT TO COPY
set DESTINATION="\\in-vx45.prod.homecredit.in\HCIN\CRM\EP_AUDIT_SHEET"

:: /E - copy subdirectories including empty folders
:: /XO - Exclude older files

robocopy "%SOURCE%" %DESTINATION% /E /XO /XN

::pause