Param (
	[string]$Path = "C:\Users\AB37179\Documents\My Projects\Agent Promo Usage\",
	[string]$SMTPServer = "mailgate.uswc.uswest.com",
	[string]$From = "mohit.bansal1@centurylink.com",
	#[string]$To = "garg.gaurav@centurylink.com",
    #[string]$Cc = "garg.gaurav@centurylink.com",
    [string]$Bcc = "mohit.bansal1@centurylink.com; saanchi.maheshwari@centurylink.com",
    [string]$To = "Janet.Brodsky@centurylink.com ; James.Miroslaw@CenturyLink.com ; Archana.Kulkarni@channelmanagement.com ; kayla.jones@channelmanagement.com ; Scott.Sinclair@channelmanagement.com",
	[string]$Cc = "mohit.bansal1@centurylink.com ; Mayuri.Srivastava@CenturyLink.com ; Stephanie.J.Bryan@centurylink.com",
	[string]$Subject = "Agent Promo Usage File"
	)

$SMTPMessage = @{
To = $To
Cc = $Cc
Bcc = $Bcc
From = $From
Subject = $Subject
Smtpserver = $SMTPServer
}

$File = Get-ChildItem $Path | Where { $_.LastWriteTime -ge [datetime]::Now.AddMinutes(-120) }
$File = Get-ChildItem $Path |  Where { $_.LastWriteTime.toshortdatestring() -eq (Get-Date -Format d)}
Write-Output $File.fullname
If ($File)
{   $Att = new-object Net.Mail.Attachment($File.FullName)
	$SMTPBody = "`nHi All,`n`nPlease find attached the Promo Usage File for today.`n`nRegards,`nMohit Bansal"
	Send-MailMessage @SMTPMessage -Body $SMTPBody -Attachments $File.FullName
}
Else
{
  Write-Output "NO FILE FOUND"
  }
