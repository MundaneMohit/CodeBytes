# Go to basefile path
$Path =  "C:\Users\mohit.bansal61089\My Projects\EP_Audit_Sheet"
$Dump = $Path + "\Dump"
$Archive = $Path + "\Archives"
# Store in file
cd $Path
$File = Get-ChildItem '.\EP_AUDIT.xlsx' # Changefilename here

# Check if file was created today
if(((Get-Date) - $File.LastWriteTime).totalHours -lt 1 )
{
    Write-Output "FILE HAS BEEN UPDATED. Copying..."
    
    # Retrieve Filename
    $Filename = $File.BaseName

    # Add Time Stamp
    $FileNameTime = $Filename + "_" + $File.LastWriteTime.adddays(-1).ToString('ddMMyyyy') + 	$File.Extension

    # Go to output folder
    cd $Dump

    # Create copy
    cp $File $FileNameTime
    Set-ItemProperty $FileNameTime -Name IsReadOnly -Value $true
}
else
{
    Write-Output "FILE NOT UPDATED. Aborting..."
}


#######################################################################################################

### MOVE FILES TO LOCAL ARCHIVE ###

$files = get-childitem $Dump *.xlsx  # Change extension here

foreach ($file in $files) 

{
    $Directory = $Archive + "\" + $file.CreationTime.Date.ToString('yyyy') + "\" + $file.CreationTime.Date.ToString('MMM')
    if (!(Test-Path $Directory))
    {
        New-Item $Directory -type directory
    }
    Move-Item -force $file.fullname $Directory 
}