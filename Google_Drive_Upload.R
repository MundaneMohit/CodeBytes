#################################################
###### CODE TO UPLOAD DATA TO GOOGLE DRIVE ######
# Author        :  MundaneMohit                 #
# Created On    :  21-Dec-17                    #
# Last Modified :  21-Dec-17                    #
# Description   : Use this code to upload or    #
#                 update files on google drive  #
#################################################
#################################################

options(download.file.method="libcurl")

#install.packages("httpuv")
#install.packages("rlang")
#install.packages("googledrive")
#install.packages("tidyverse")


library("googledrive")

# LOCAL FILE
myFile = 'D:/Investment Portfolio.xlsx'

# FIRST TIME 
#drive_upload(myFile)

# UPDATE FILE ON DRIVE
drive_update(file = "Investment Portfolio.xlsx",media = myFile)
