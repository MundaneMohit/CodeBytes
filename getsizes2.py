import win32com.client as com

folderPath = r"C:\Users\AB37179\Downloads"
fso = com.Dispatch("Scripting.FileSystemObject")
folder = fso.GetFolder(folderPath)
MB=1024*1024.0
print  "%.2f MB"%(folder.Size/MB)
