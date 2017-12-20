:: Run command to create the text file

echo "Running Query"
sqlplus -s ab37179/Mohit1603@SBMSDB_PROD @"C:\Users\AB37179\Documents\My Projects\Agent Promo Usage Backup\Scripts\All_Steps_Batch_2.sql" 

::pause

:: Rename text file

echo "Copying file"

cd "C:\Users\AB37179\Documents\My Projects\Agent Promo Usage Backup\"
PowerShell ./Agent_Copy_TimeStamp.ps1

::pause

:: Mail the text file 

echo "Sending mail"

cd "C:\Users\AB37179\Documents\My Projects\Agent Promo Usage Backup\"
PowerShell ./mailer_batch.ps1

::pause