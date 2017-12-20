:: MAKE SURE FILES ARE IN APPROPRIATE FOLDERS BEFORE RUNNING SCRIPT

:: BACKUP CLEARLINK

::SET YOUR SOURCE PATH
set SOURCE="C:\Users\AB37179\Documents\My Projects\Clearlink Sales Dashboard\Archives\2016"

::SET PATH WHERE YOU WANT TO COPY
set DESTINATION="\\eldnp1515dm3\mixdeng06\MRI\Reporting\Business\Team SMB Deliveries\SB-Clearlink Sales Dashboard\2016"

:: /E - copy subdirectories including empty folders
:: /XO - Exclude older files

robocopy %SOURCE% %DESTINATION% /E /XO /XN

PAUSE

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:: BACKUP PROMO USAGE TOOL

set SOURCE="C:\Users\AB37179\Documents\My Projects\Promo Usage Tool\Archives\2016"

set DESTINATION="\\eldnp1515dm3\mixdeng06\MRI\Reporting\Business\Team SMB Deliveries\SB-Promo Usage Tool\2016"

robocopy %SOURCE% %DESTINATION% /E /XO /XN

PAUSE

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:: BACKUP FIBER SALES COMPENSATION REPORT

set SOURCE="C:\Users\AB37179\Documents\My Projects\GPON Sales Compensation\Archives\2016"

set DESTINATION="\\eldnp1515dm3\mixdeng06\MRI\Reporting\Business\Team SMB Deliveries\SB-Fiber Sales Compensation Report\2016"

robocopy %SOURCE% %DESTINATION% /E /XO /XN

PAUSE
