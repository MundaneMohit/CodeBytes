#install.packages("data.table")

library(data.table)
############# Always Change the diretory when Changing the Source Folder ########
setwd("C:/Users/AB37179/Documents/My Projects/Retention and Active HSI/Promo vs Non Promo/Raw Data")
Raw_Data<- read.csv(file="Promo-Non.csv")
Raw_Data <- Raw_Data[,c(1:11)]
Raw_Data<-Raw_Data[which(Raw_Data$IN_DATE!=""),]
#### Getting Month Using In Date ############
Raw_Data$Month<-ifelse(substring(Raw_Data$IN_DATE,1,2)=='1/','Jan',
                       ifelse(substring(Raw_Data$IN_DATE,1,2)=='2/','Feb',
                              ifelse(substring(Raw_Data$IN_DATE,1,2)=='3/','Mar',
                                     ifelse(substring(Raw_Data$IN_DATE,1,2)=='4/','Apr',
                                            ifelse(substring(Raw_Data$IN_DATE,1,2)=='5/','May',
                                                   ifelse(substring(Raw_Data$IN_DATE,1,2)=='6/','Jun',
                                                          ifelse(substring(Raw_Data$IN_DATE,1,2)=='7/','Jul',
                                                                 ifelse(substring(Raw_Data$IN_DATE,1,2)=='8/','Aug',
                                                                        ifelse(substring(Raw_Data$IN_DATE,1,2)=='9/','Sep',
                                                                               ifelse(substring(Raw_Data$IN_DATE,1,2)=='10','Oct',
                                                                                      ifelse(substring(Raw_Data$IN_DATE,1,2)=='11','Nov',
                                                                                             ifelse(substring(Raw_Data$IN_DATE,1,2)=='12','Dec',"NA"))))))))))))





########## Month end ##############
######### Getting Credit Class Map ##############
Raw_Data$Credit_Class<-ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='A','Low Risk',
                              ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='B','Low Risk',
                                     ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='C','Med Risk',
                                            ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='D','Med-High Risk',
                                                   ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='E','High Risk',
                                                          ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='F','High Risk',
                                                                 ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='H','High Risk',
                                                                        ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='P','High Risk',
                                                                               ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='R','Med-High Risk',
                                                                                      ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='U','Unknown',
                                                                                             ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='Z','Med-High Risk',
                                                                                                    ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='0','Low Risk',
                                                                                                           ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='1','Low Risk',
                                                                                                                  ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='2','Low Risk',
                                                                                                                         ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='3','Med Risk',
                                                                                                                                ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='4','Med-High Risk',
                                                                                                                                       ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='5','High Risk',
                                                                                                                                              ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='6','High Risk',
                                                                                                                                                     ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='7','High Risk',
                                                                                                                                                            ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='8','High Risk',
                                                                                                                                                                   ifelse(Raw_Data$CRNT_CREDIT_CLS_CD=='9','High Risk','Unknown')))))))))))))))))))))
######### Credit Class Ends ###########
########### getting the State ####
Raw_Data$State<-ifelse(Raw_Data$ADDR_ST_CD=='AL', 'Alabama',
                       ifelse(Raw_Data$ADDR_ST_CD=='AK', 'Alaska',
                              ifelse(Raw_Data$ADDR_ST_CD=='AZ', 'Arizona',
                                     ifelse(Raw_Data$ADDR_ST_CD=='AR', 'Arkansas',
                                            ifelse(Raw_Data$ADDR_ST_CD=='CA', 'California',
                                                   ifelse(Raw_Data$ADDR_ST_CD=='CO', 'Colorado',
                                                          ifelse(Raw_Data$ADDR_ST_CD=='CT', 'Connecticut',
                                                                 ifelse(Raw_Data$ADDR_ST_CD=='DE', 'Delaware',
                                                                        ifelse(Raw_Data$ADDR_ST_CD=='FL', 'Florida',
                                                                               ifelse(Raw_Data$ADDR_ST_CD=='GA', 'Georgia',
                                                                                      ifelse(Raw_Data$ADDR_ST_CD=='HI', 'Hawaii',
                                                                                             ifelse(Raw_Data$ADDR_ST_CD=='ID', 'Idaho',
                                                                                                    ifelse(Raw_Data$ADDR_ST_CD=='IL', 'Illinois',
                                                                                                           ifelse(Raw_Data$ADDR_ST_CD=='IN', 'Indiana',
                                                                                                                  ifelse(Raw_Data$ADDR_ST_CD=='IA', 'Iowa',
                                                                                                                         ifelse(Raw_Data$ADDR_ST_CD=='KS', 'Kansas',
                                                                                                                                ifelse(Raw_Data$ADDR_ST_CD=='KY', 'Kentucky',
                                                                                                                                       ifelse(Raw_Data$ADDR_ST_CD=='LA', 'Louisiana',
                                                                                                                                              ifelse(Raw_Data$ADDR_ST_CD=='ME', 'Maine',
                                                                                                                                                     ifelse(Raw_Data$ADDR_ST_CD=='MD', 'Maryland',
                                                                                                                                                            ifelse(Raw_Data$ADDR_ST_CD=='MA', 'Massachusetts',
                                                                                                                                                                   ifelse(Raw_Data$ADDR_ST_CD=='MI', 'Michigan',
                                                                                                                                                                          ifelse(Raw_Data$ADDR_ST_CD=='MN', 'Minnesota',
                                                                                                                                                                                 ifelse(Raw_Data$ADDR_ST_CD=='MS', 'Mississippi',
                                                                                                                                                                                        ifelse(Raw_Data$ADDR_ST_CD=='MO', 'Missouri',
                                                                                                                                                                                               ifelse(Raw_Data$ADDR_ST_CD=='MT', 'Montana',
                                                                                                                                                                                                      ifelse(Raw_Data$ADDR_ST_CD=='NE', 'Nebraska',
                                                                                                                                                                                                             ifelse(Raw_Data$ADDR_ST_CD=='NV', 'Nevada',
                                                                                                                                                                                                                    ifelse(Raw_Data$ADDR_ST_CD=='NH', 'New Hampshire',
                                                                                                                                                                                                                           ifelse(Raw_Data$ADDR_ST_CD=='NJ', 'New Jersey',
                                                                                                                                                                                                                                  ifelse(Raw_Data$ADDR_ST_CD=='NM', 'New Mexico',
                                                                                                                                                                                                                                         ifelse(Raw_Data$ADDR_ST_CD=='NY', 'New York',
                                                                                                                                                                                                                                                ifelse(Raw_Data$ADDR_ST_CD=='NC', 'North Carolina',
                                                                                                                                                                                                                                                       ifelse(Raw_Data$ADDR_ST_CD=='ND', 'North Dakota',
                                                                                                                                                                                                                                                              ifelse(Raw_Data$ADDR_ST_CD=='OH', 'Ohio',
                                                                                                                                                                                                                                                                     ifelse(Raw_Data$ADDR_ST_CD=='OK', 'Oklahoma',
                                                                                                                                                                                                                                                                            ifelse(Raw_Data$ADDR_ST_CD=='OR', 'Oregon',
                                                                                                                                                                                                                                                                                   ifelse(Raw_Data$ADDR_ST_CD=='PA', 'Pennsylvania',
                                                                                                                                                                                                                                                                                          ifelse(Raw_Data$ADDR_ST_CD=='RI', 'Rhode Island',
                                                                                                                                                                                                                                                                                                 ifelse(Raw_Data$ADDR_ST_CD=='SC', 'South Carolina',
                                                                                                                                                                                                                                                                                                        ifelse(Raw_Data$ADDR_ST_CD=='SD', 'South Dakota',
                                                                                                                                                                                                                                                                                                               ifelse(Raw_Data$ADDR_ST_CD=='TN', 'Tennessee',
                                                                                                                                                                                                                                                                                                                      ifelse(Raw_Data$ADDR_ST_CD=='TX', 'Texas',
                                                                                                                                                                                                                                                                                                                             ifelse(Raw_Data$ADDR_ST_CD=='UT', 'Utah',
                                                                                                                                                                                                                                                                                                                                    ifelse(Raw_Data$ADDR_ST_CD=='VT', 'Vermont',
                                                                                                                                                                                                                                                                                                                                           ifelse(Raw_Data$ADDR_ST_CD=='VA', 'Virginia',
                                                                                                                                                                                                                                                                                                                                                  ifelse(Raw_Data$ADDR_ST_CD=='WA', 'Washington',
                                                                                                                                                                                                                                                                                                                                                         ifelse(Raw_Data$ADDR_ST_CD=='WV', 'West Virginia',
                                                                                                                                                                                                                                                                                                                                                                ifelse(Raw_Data$ADDR_ST_CD=='WI', 'Wisconsin',
                                                                                                                                                                                                                                                                                                                                                                       ifelse(Raw_Data$ADDR_ST_CD=='WY', 'Wyoming','Unknown'))))))))))))))))))))))))))))))))))))))))))))))))))
########## State Ends##########
#### FLAG STARTS #########         
Raw_Data$FLAG<-ifelse(Raw_Data$DISCOUNT_CODE=="","Non Promo","Promo")
###### FLAG ENDS###########

#### Creating the count Datasets#####

Count_Data<-Raw_Data[,c(1,12,15)]
Count_Data<-unique(Count_Data)
Count_Data<- Count_Data[which(Count_Data$CUST_ACCT_ID!='NA'),]
Count_Data<-data.table(Count_Data)
Count<-Count_Data[,list
                  (
                    Non_Promo=sum(ifelse(FLAG=='Non Promo',1,0)),
                    Prom=sum(ifelse(FLAG=='Promo',1,0))
                  ),
                  by=list(Month)]
Count$Total<-Count$Non_Promo + Count$Prom

#####################

Sum_Promo<-sum(Count$Prom)
Sum_Non_Promo<-sum(Count$Non_Promo)
Total<- Sum_Promo + Sum_Non_Promo
Promo_perc<- round((Sum_Promo/Total)*100,2)
Non_Promo_perc<- round((Sum_Non_Promo/Total)*100,2)

Promo_Name<- c("Non_Promo", "Promo")
Promo_Perc<-c( Non_Promo_perc, Promo_perc)
Promo_Perc_Table<- cbind(Promo_Name,Promo_Perc)
Promo_Perc_Table<-data.frame(Promo_Perc_Table)

############# Always Change the diretory when Changing the destination Folder ########
setwd("C:/Users/AB37179/Documents/My Projects/Retention and Active HSI/Promo vs Non Promo/Tables")
write.csv(file="Count.csv",Count,row.names=FALSE)
write.csv(file="Promo_Perc_Table.csv",Promo_Perc_Table,row.names=FALSE)

####################### Count Data base ends#############
####################### State Database Begins############

State_Data<-Raw_Data[,c(1,12,14,15)]
State_Data<-unique(State_Data)
State_Data<- State_Data[which(State_Data$CUST_ACCT_ID!='NA'),]
State_Data<-data.table(State_Data)

State<-State_Data[,list
                  (
                    Non_Promo=sum(ifelse(FLAG=='Non Promo',1,0)),
                    Promo=sum(ifelse(FLAG=='Promo',1,0))
                  ),
                  by=list(State)]
State$Total<-State$Non_Promo+State$Promo
State<-State[order(Total,decreasing = TRUE),]

write.csv(file="State.csv",State,row.names=FALSE)

############ State Data Ends ############
############ Credit Class Starts #########

Credit_Data<-Raw_Data[,c(1,12,13,15)]
Credit_Data<-unique(Credit_Data)
Credit_Data<- Credit_Data[which(Credit_Data$CUST_ACCT_ID!='NA'),]
Credit_Data<-data.table(Credit_Data)

Credit<-Credit_Data[,list
                    (
                      Non_Promo=sum(ifelse(FLAG=='Non Promo',1,0)),
                      Prom=sum(ifelse(FLAG=='Promo',1,0))
                    ),
                    by=list(Credit_Class,Month)]

Credit<-Credit[order(Credit$Credit_Class),]
Credit$Total<-Credit$Non_Promo+Credit$Prom
write.csv(file="Credit.csv",Credit,row.names=FALSE)

############ Credit end ###########################
########## Promo Top 20 ########################

Promo_Top_Data<-Raw_Data[,c(1,5,6,15)]
Promo_Data<-unique(Promo_Top_Data)
Promo_Data<- Promo_Data[which(Promo_Data$CUST_ACCT_ID!='NA'),]
Promo_Data$Unit<-1
Promo_Data<-data.table(Promo_Data)

Promo_Top<-Promo_Data[,list
                      (
                        Total=sum(Unit)),
                      by=list(
                        DISCOUNT_DESC)] 

Promo_Top<-Promo_Top[order(Total,decreasing = TRUE),]
Promo_Top<-Promo_Top[which(Promo_Top$DISCOUNT_DESC!="")]
write.csv(file="Promo_Top.csv",Promo_Top,row.names=FALSE)

############# Promo Top 20 Ends #########################





