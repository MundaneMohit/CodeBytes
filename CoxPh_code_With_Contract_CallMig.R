#### Download Packages ####
# install.packages("survival")
# install.packages("pec")
#install.packages("smbinning")


# Added contract info for LQ & LCTL

# TRAINING & TESTING:- Jan 2015 EIS + Feb 2015 to Mar 2016 Ins & Outs
# VALIDATION :- Apr 2016 EIS + May 2016 Ins & Outs


#### Clear memory ####
gc()
rm(list = ls())
gc()
search()
length(x)
for(i in 2:length(x))
{  detach(2)}

detach(2)
search()


x<- search()
class(x)
as.array(x)
x

gc()

#### LOAD LIBRARIES ####
library("smbinning")
library(survival)
library(pec)
library(rpart)
library(stringr)
# VISUALIZE
library("partykit")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)

#### LOAD DATASETS ####
setwd("C:/Users/AB37179/Documents")

# Read data
HSI_RAW <- read.csv("C:/Users/AB37179/Documents/My Projects/HSI Predictive Churn Analysis/Survival Analysis/Raw Data/HSI_EIS_Raw_TrainCallMig.csv")

#HSI.final_merge <- read.csv("C:/Users/AB37179/Documents/My Projects/HSI Predictive Churn Analysis/Survival Analysis/ak.csv")

# Load Validation data also
HSI_VAL <- read.csv("C:/Users/AB37179/Documents/My Projects/HSI Predictive Churn Analysis/Survival Analysis/Raw Data/HSI_EIS_Raw_Val_June.csv")
colnames(HSI_RAW)
##############################################################################################
##################################### DATA PREPROCESSING #####################################
##############################################################################################

#### Data Preprocessor ####
DataCleanser <- function(Raw_Data)
{
  head(Raw_Data)
  colnames(Raw_Data)
  
  # Check class for variables
  #print(sapply(Raw_Data, class))
  #print(class(Raw_Data$CUSTOMER_TENURE))
  
  # Exclude Negative Tenure
  Raw_Data <- Raw_Data[which(Raw_Data$DUR_DAYS>0),]
  
  # Fix Datatype issues
  Raw_Data$CUSTOMER_TENURE <- as.numeric(Raw_Data$CUSTOMER_TENURE)
  Raw_Data$PRIMSIC_COMBINED <- as.factor(Raw_Data$PRIMSIC_COMBINED)
  Raw_Data$REV_MISS <- as.factor(Raw_Data$REV_MISS)
  Raw_Data$REV_NEG <- as.factor(Raw_Data$REV_NEG)
  Raw_Data$REV_ZERO <- as.factor(Raw_Data$REV_ZERO)
  Raw_Data$HAS_CONTRACT <- as.factor(Raw_Data$HAS_CONTRACT)
  Raw_Data$HAZARD <- as.factor(Raw_Data$HAZARD)
  Raw_Data$UPG_WNG <- as.character(Raw_Data$UPG_WNG)
  
  # Remove NA Values. replace with 0
  Raw_Data <- Raw_Data[which(!is.na(HSI_RAW$CUSTOMER_TENURE)),] 
  Raw_Data[which(is.na(Raw_Data$MONTHS_FROM_START)),"MONTHS_FROM_START"] <- 9999
  Raw_Data[which(is.na(Raw_Data$MONTHS_TO_END)),"MONTHS_TO_END"] <- 9999
  Raw_Data[which(is.na(Raw_Data$MONTHS_FROM_END)),"MONTHS_FROM_END"] <- 9999
  Raw_Data[which(Raw_Data$UPG_WNG == ""),"UPG_WNG"] <- 'N'
  Raw_Data[which(is.na(Raw_Data$CALL_COUNT)),"CALL_COUNT"] <- 0
  Raw_Data[which(is.na(Raw_Data$CALL_DURATION)),"CALL_DURATION"] <- 0
  Raw_Data[which(is.na(Raw_Data$TIME_WITH_AGENT)),"TIME_WITH_AGENT"] <- -1
  Raw_Data[which(is.na(Raw_Data$ANSHOLDTIME)),"ANSHOLDTIME"] <- -1
  Raw_Data[which(is.na(Raw_Data$QUEUETIME)),"QUEUETIME"] <- -1
  Raw_Data$UPG_WNG <- as.factor(Raw_Data$UPG_WNG)
  
  return(Raw_Data)
}

# Use function for Train & Validation sets
HSI_RAW <- DataCleanser(HSI_RAW)
HSI_RAW <- DataCleanser(HSI_RAW)
HSI_VAL <- DataCleanser(HSI_VAL)
HSI_VAL <- DataCleanser(HSI_VAL)

##############################################################################################
################################# FEATURE ENGINEERING ########################################
##############################################################################################

#### Add multiple levels for PRIMSIC. Replace missing with neutral value (Eg. "NNNN")####
# Convert to Character
HSI_RAW$PRIMSIC_COMBINED <- as.character(HSI_RAW$PRIMSIC_COMBINED)
# Padding left
HSI_RAW$PRIMSIC_COMBINED <- str_pad(HSI_RAW$PRIMSIC_COMBINED,4,c("left"), pad = 0)
# Replace NAs with neutral value
HSI_RAW[which(is.na(HSI_RAW$PRIMSIC_COMBINED)), "PRIMSIC_COMBINED"] <- "NNNN"
# Add multiple levels for PRIMSIC
HSI_RAW$PRIMSIC_1 <- substr(HSI_RAW$PRIMSIC_COMBINED,0,2)
HSI_RAW$PRIMSIC_2 <- substr(HSI_RAW$PRIMSIC_COMBINED,2,3)
HSI_RAW$PRIMSIC_3 <- substr(HSI_RAW$PRIMSIC_COMBINED,3,4)

# Data type conversion
HSI_RAW$PRIMSIC_COMBINED <- as.factor(HSI_RAW$PRIMSIC_COMBINED)

##### Binning Customer Tenure #####
class(HSI_RAW$HAZARD)
#numeric
HSI_RAW$HAZARD <- as.integer(as.character((HSI_RAW$HAZARD)))
#integer
#Cut based on correlation to HAZARD
cutpoints <- smbinning(HSI_RAW, y= "HAZARD", x= "CUSTOMER_TENURE")$cuts
cutpoints
#7  14  26  53  66  89 132 266 374
# Add new Binned variable
max(HSI_RAW$CUSTOMER_TENURE)
levels(cut(HSI_RAW$CUSTOMER_TENURE, c(0,cutpoints,max(HSI_RAW$CUSTOMER_TENURE))))
HSI_RAW$CUSTEN_10BINS <- cut(HSI_RAW$CUSTOMER_TENURE, c(0,cutpoints,max(HSI_RAW$CUSTOMER_TENURE)), lab = c("<8", "8-14", "15-26", "27-53", "54-66", "67-89", "90-132", "133-266", "267-374", ">374"))
levels(HSI_RAW$CUSTEN_10BINS)

#### Similarly for Product Tenure#### 
## Binning Product Tenure
cutpoints <- smbinning(HSI_RAW, y= "HAZARD", x= "PRODUCT_TENURE")$cuts
cutpoints
# 5 14 32 36 43 47 52 78
max(HSI_RAW$PRODUCT_TENURE)
levels(cut(HSI_RAW$PRODUCT_TENURE, c(0,cutpoints,max(HSI_RAW$PRODUCT_TENURE))))
HSI_RAW$PRODTEN_BINS <- cut(HSI_RAW$PRODUCT_TENURE, c(0,cutpoints,max(HSI_RAW$PRODUCT_TENURE)), lab = c("<6", "6-14", "15-32", "33-36", "37-43", "44-47", "48-52", "53-78", ">78"))
levels(HSI_RAW$PRODTEN_BINS)

#### Binning Contract Variables ####
# MONTHS_FROM_END
cutpoints <- smbinning(HSI_RAW, y= "HAZARD", x= "MONTHS_FROM_END")$cuts
cutpoints
# -22   -2 1397
max(HSI_RAW$MONTHS_FROM_END[which(HSI_RAW$MONTHS_FROM_END!=9999)])
min(HSI_RAW$MONTHS_FROM_END)
levels(cut(HSI_RAW$MONTHS_FROM_END, c(min(HSI_RAW$MONTHS_FROM_END)-1, cutpoints,max(HSI_RAW$MONTHS_FROM_END))))
HSI_RAW$MTHFEND_BINS <- cut(HSI_RAW$MONTHS_FROM_END, c(min(HSI_RAW$MONTHS_FROM_END)-1,cutpoints,max(HSI_RAW$MONTHS_FROM_END)), lab = c("<-22", "-21 to -2", "-1 to 1397", "NO_CONTRACT"))
levels(HSI_RAW$MTHFEND_BINS)

# MONTHS_TO_END
cutpoints <- smbinning(HSI_RAW, y= "HAZARD", x= "MONTHS_TO_END")$cuts
cutpoints
# 1 21 33
max(HSI_RAW$MONTHS_TO_END[which(HSI_RAW$MONTHS_TO_END!=9999)])
min(HSI_RAW$MONTHS_TO_END)
levels(cut(HSI_RAW$MONTHS_TO_END, c(min(HSI_RAW$MONTHS_TO_END)-1, cutpoints,342, max(HSI_RAW$MONTHS_TO_END))))
HSI_RAW$MTHTOEND_BINS <- cut(HSI_RAW$MONTHS_TO_END, c(min(HSI_RAW$MONTHS_TO_END)-1,cutpoints,342, max(HSI_RAW$MONTHS_TO_END)), lab = c("< 1", "2 to 21", "22 to 33", "34 to 342", "NO_CONTRACT"))
levels(HSI_RAW$MTHTOEND_BINS)

# MONTHS_FROM_START
cutpoints <- smbinning(HSI_RAW, y= "HAZARD", x= "MONTHS_FROM_START")$cuts
cutpoints
# 6  20  41 159
max(HSI_RAW$MONTHS_FROM_START[which(HSI_RAW$MONTHS_FROM_START!=9999)])
min(HSI_RAW$MONTHS_FROM_START)
levels(cut(HSI_RAW$MONTHS_FROM_START, c(min(HSI_RAW$MONTHS_FROM_START)-1, cutpoints, max(HSI_RAW$MONTHS_FROM_START))))
HSI_RAW$MTHFSTRT<- cut(HSI_RAW$MONTHS_FROM_START, c(min(HSI_RAW$MONTHS_FROM_START)-1,cutpoints, max(HSI_RAW$MONTHS_FROM_START)), lab = c("<6", "7 to 20", "21 to 41", "42 to 159", "NO_CONTRACT"))
levels(HSI_RAW$MTHFSTRT)

#### Adding new Contract variable ####
HSI_RAW$CONTRACT_FLAG <- ifelse(HSI_RAW$MONTHS_FROM_END == 9999, "NO_CONTRACT",
                                ifelse(HSI_RAW$MONTHS_FROM_END >= 0, "IN_CONTRACT",
                                       ifelse(HSI_RAW$MONTHS_FROM_END < 0, "POST_CONTRACT","ERR")))
HSI_RAW$CONTRACT_FLAG <- as.factor(HSI_RAW$CONTRACT_FLAG)

#### RETAIN OLD CONTRACT VARIABLES AS WELL ####
HSI_RAW$IN_CONTRACT <- HSI_RAW$MONTHS_FROM_END < 0
HSI_RAW$POST_CONTRACT <- HSI_RAW$MONTHS_TO_END < 0                          
HSI_RAW$NO_CONTRACT <- HSI_RAW$MONTHS_FROM_END == 9999   

detach(HSI_RAW)

#### BINNING CALL VARIABLES ####
# Bin CALL_COUNT
cutpoints <- smbinning(HSI_RAW, y= "HAZARD", x= "CALL_COUNT")$cuts
cutpoints
# 0 2
# Add new Binned variable
max(HSI_RAW$CALL_COUNT)
levels(cut(HSI_RAW$CALL_COUNT, c(-1,cutpoints,max(HSI_RAW$CALL_COUNT))))
HSI_RAW$CALL_COUNT_BINNED <- cut(HSI_RAW$CALL_COUNT, c(-1,cutpoints,max(HSI_RAW$CUSTOMER_TENURE)), lab = c("0", "1-2", ">2"))
levels(HSI_RAW$CALL_COUNT_BINNED)

# Bin CALL_DURATION
cutpoints <- smbinning(HSI_RAW, y= "HAZARD", x= "CALL_DURATION")$cuts
cutpoints
#  7  665 1495
# Add new Binned variable
max(HSI_RAW$CALL_DURATION)
levels(cut(HSI_RAW$CALL_DURATION, c(-1, 0,cutpoints,max(HSI_RAW$CALL_DURATION))))
HSI_RAW$CALL_DUR_BINNED <- cut(HSI_RAW$CALL_DURATION, c(-1,0,cutpoints,max(HSI_RAW$CALL_DURATION)), lab = c("0", "1-7", "8-665", "666-1495", ">1495"))
levels(HSI_RAW$CALL_DUR_BINNED)

# Bin TIME_WITH_AGENT
cutpoints <- smbinning(HSI_RAW, y= "HAZARD", x= "TIME_WITH_AGENT")$cuts
cutpoints
# -1 73
# Add new Binned variable
max(HSI_RAW$TIME_WITH_AGENT)
min(HSI_RAW$TIME_WITH_AGENT)
levels(cut(HSI_RAW$TIME_WITH_AGENT, c(-2,-1,0,73,max(HSI_RAW$TIME_WITH_AGENT))))
HSI_RAW$AGENTTIME_BINNED <- cut(HSI_RAW$TIME_WITH_AGENT, c(-2,-1,0,73,max(HSI_RAW$TIME_WITH_AGENT)), lab = c("<0", "0", "1-73", ">73"))
levels(HSI_RAW$AGENTTIME_BINNED)

# Bin ANSHOLDTIME
cutpoints <- smbinning(HSI_RAW, y= "HAZARD", x= "ANSHOLDTIME")$cuts
cutpoints
#  -1  5
# Add new Binned variable
max(HSI_RAW$ANSHOLDTIME)
levels(cut(HSI_RAW$ANSHOLDTIME, c(-2,-1,0,5,max(HSI_RAW$ANSHOLDTIME))))
HSI_RAW$ANSHOLD_BINNED <- cut(HSI_RAW$ANSHOLDTIME, c(-2,-1,0,5,max(HSI_RAW$ANSHOLDTIME)), lab = c("<0", "0", "1-5", ">5"))
levels(HSI_RAW$ANSHOLD_BINNED)

# Bin QUEUETIME
cutpoints <- smbinning(HSI_RAW, y= "HAZARD", x= "QUEUETIME")$cuts
cutpoints
#  -1 36
# Add new Binned variable
max(HSI_RAW$QUEUETIME)
levels(cut(HSI_RAW$QUEUETIME, c(-2, -1, 0,36,max(HSI_RAW$QUEUETIME))))
HSI_RAW$QUEUE_BINNED <- cut(HSI_RAW$QUEUETIME, c(-2, -1, 0,36,max(HSI_RAW$QUEUETIME)), lab = c("<0", "0", "1-36", ">36"))
levels(HSI_RAW$QUEUE_BINNED)

###########################################################################################
############################## ALTERNATE DATASETS #########################################
###########################################################################################

#### Qwest Only
HSI_LQ <- HSI_RAW[which(HSI_RAW$LEGACY == 'Q'),]

#### CTL Only
HSI_LCTL <- HSI_RAW[which(HSI_RAW$LEGACY == 'CTL'),]

#### Voluntary Churners only
HSI_VOLUN <- HSI_RAW[which(HSI_RAW$REASON_TYPE != 'Involuntary'),]

##########################################################################################
############################# DATA SAMPLING & PREPERATION ################################
##########################################################################################

#### Function for data Sampling ####
CreateDataSample <- 
  function(myData, sampleSize, seed) {
    set.seed(seed)
    myData.Sample <- myData[sample(nrow(myData), sampleSize),]
    return(myData.Sample)
  }          

# Change dataset name inside the function for which you want to do the analysis
# Set sample size
# Set seed
HSI_RAW.Sample <- CreateDataSample(HSI_RAW, 150000, 111)
HSI_RAW.Sample <- CreateDataSample(HSI_VOLUN, 150000, 111)

#HSI_RAW.Sample <- CreateDataSample(HSI_VOLUN, 150000, 111)

#### Divide into train & test (60:40 ratio) ####
set.seed(123)

# SET SAMPLING RATIO FOR TRAIN & TEST DATA
ratio <- floor(0.60*nrow(HSI_RAW.Sample))
samp_index <- sample(nrow(HSI_RAW.Sample), ratio)

# Create Train & Test sets
HSI_RAW.Sample.Train <- HSI_RAW.Sample[samp_index, ]
HSI_RAW.Sample.Test  <- HSI_RAW.Sample[-samp_index,]

#### Check ratios for train & test ####
table(HSI_RAW.Sample.Train$HAZARD)
table(HSI_RAW.Sample.Test$HAZARD)

##############################################################################################
################################# MODEL BUILDING #############################################
##############################################################################################
colnames(HSI_RAW.Sample.Test)
sapply(HSI_RAW.Sample.Train, class)

##### i) BEST MODEL WITH BINS & TENURE (MODEL 7) #####
HSI.CoxModel <- coxph(Surv(DUR,HAZARD)~ PRODUCT_TENURE+CUSTOMER_TENURE+LEGACY+REGION+
                        STATE+CREDIT_RISK+INDUSTRY_COMBINED
                      +MKT_CLUSTER+SPEED
                      +SPEED_BUCKET+REVENUE_TREND+REV_MISS+REV_ZERO+REV_NEG
                      +HAS_CONTRACT+MONTHS_TO_END+MONTHS_FROM_START+MONTHS_FROM_END
                      +CUSTEN_10BINS+PRODTEN_BINS
                      +IN_CONTRACT+POST_CONTRACT+NO_CONTRACT,  
                      #CONTRACT_FLAG,  
                      data= HSI_RAW.Sample.Train)

####### ii) NEW variables callmig ######
sapply(HSI_RAW.Sample.Train, class)
levels(HSI_RAW.Sample.Train$UPG_WNG)
+CALL_COUNT+CALL_DURATION+TIME_WITH_AGENT+ANSHOLDTIME+QUEUETIME

HSI.CoxModel <- coxph(Surv(DUR,HAZARD)~ PRODUCT_TENURE+CUSTOMER_TENURE+LEGACY+REGION+
                      STATE+CREDIT_RISK+INDUSTRY_COMBINED
                      +MKT_CLUSTER+SPEED+SPEED_BUCKET
                      +REVENUE_TREND+REV_MISS+REV_ZERO+REV_NEG
                      +HAS_CONTRACT+MONTHS_TO_END+MONTHS_FROM_START+MONTHS_FROM_END
                      +CUSTEN_10BINS+PRODTEN_BINS
                      +IN_CONTRACT+POST_CONTRACT+NO_CONTRACT
                      +UPG_WNG
                      +CALL_COUNT+CALL_DURATION+TIME_WITH_AGENT+ANSHOLDTIME+QUEUETIME,  
                      #CONTRACT_FLAG,  
                      ,data= HSI_RAW.Sample.Train)

##### iii) LOG TRANSFORMATION ####
HSI.CoxModel <- coxph(Surv(DUR,HAZARD)~ log(PRODUCT_TENURE)+log(CUSTOMER_TENURE)+LEGACY+REGION+
                        STATE+CREDIT_RISK+INDUSTRY_COMBINED
                      +MKT_CLUSTER+SPEED+SPEED_BUCKET
                      +REVENUE_TREND+REV_MISS+REV_ZERO+REV_NEG
                      +HAS_CONTRACT+MONTHS_TO_END+MONTHS_FROM_START+MONTHS_FROM_END
                      +CUSTEN_10BINS+PRODTEN_BINS
                      +IN_CONTRACT+POST_CONTRACT+NO_CONTRACT
                      +UPG_WNG
                      +CALL_COUNT+CALL_DURATION+TIME_WITH_AGENT+ANSHOLDTIME+QUEUETIME,  
                      #CONTRACT_FLAG,  
                      ,data= HSI_RAW.Sample.Train)

### SIMILAR RESULTS

#### iv) BINNED CALL VARIABLES #####
HSI.CoxModel <- coxph(Surv(DUR,HAZARD)~ log(PRODUCT_TENURE)+log(CUSTOMER_TENURE)+LEGACY+REGION+
                        STATE+CREDIT_RISK+INDUSTRY_COMBINED
                      +MKT_CLUSTER+SPEED+SPEED_BUCKET
                      +REVENUE_TREND+REV_MISS+REV_ZERO+REV_NEG
                      +HAS_CONTRACT+MONTHS_TO_END+MONTHS_FROM_START+MONTHS_FROM_END
                      +CUSTEN_10BINS+PRODTEN_BINS
                      +IN_CONTRACT+POST_CONTRACT+NO_CONTRACT
                      +UPG_WNG
                      +CALL_COUNT+CALL_DURATION+TIME_WITH_AGENT+ANSHOLDTIME+QUEUETIME+ 
                      +CALL_COUNT_BINNED+CALL_DUR_BINNED+AGENTTIME_BINNED+ANSHOLD_BINNED+QUEUE_BINNED
                      #CONTRACT_FLAG,  
                      ,data= HSI_RAW.Sample.Train)

colnames(HSI_RAW)


####### DEBUGGING WARNINGS ##############################################################

HSI.CoxModel <- coxph(Surv(DUR,HAZARD)~ PRODUCT_TENURE+CUSTOMER_TENURE+
                        LEGACY+REGION+CREDIT_RISK+INDUSTRY_COMBINED
                      +REVENUE_TREND+REV_MISS+REV_ZERO+REV_NEG
                      +MONTHS_FROM_START
                      +MONTHS_FROM_END
                      ,data= HSI_Data.Sample.Train)


# Adding these triggers warning
HAS_CONTRACT
MKT_CLUSTER
STATE
SPEED
SPEED_BUCKET
MONTHS_TO_ENDS

# Warnings?
xtabs(~HAZARD+STATE+REGION,data=HSI_Data.Sample.Train)

##############################################################################################
############################## PREDICTION ####################################################
##############################################################################################

#### CHECK COEFFICIENTS ####
HSI.CoxModel$xlevels
HSI.CoxModel$coefficients
HSI.CoxModelDays$coefficients

# Visualize Model
#HSI.Surv_Tree <- rpart(Surv(DUR_DAYS,HAZARD)~ LEGACY+REGION+STATE+CREDIT_RISK+INDUSTRY_COMBINED+MKT_CLUSTER+SPEED+SPEED_BUCKET+PRODUCT_TENURE+CUSTOMER_TENURE+REVENUE_TREND+REV_MISS+REV_ZERO+REV_NEG+HAS_CONTRACT+MONTHS_FROM_START+MONTHS_TO_END+MONTHS_FROM_END, data=HSI_Data.Sample.Train)
#fancyRpartPlot(HSI.Surv_Tree)

#### Run mode on sample test set (DUR)####
HSI.Prediction <- predictSurvProb(HSI.CoxModel,HSI_RAW.Sample.Test,  c(1,2,3,4,5,6,12))

#### Run mode on sample test set (DURDAYS) ####
#HSI.Prediction <- predictSurvProb(HSI.CoxModelDays,HSI_RAW.Sample.Test,  c(30,60,90,120,150,180,360))

#### MODIFY COLUMN NAMES ####
HSI.Prediction<-data.frame(HSI.Prediction)
colnames(HSI.Prediction) <- c("30", "60", "90", "120", "150", "180", "360")

# Join prediction with test set
HSI.final_merge<- merge(HSI_RAW.Sample.Test, y = HSI.Prediction, by="row.names")

colnames(HSI.final_merge)

##### Sort by Survival Probability in month 3 #####
HSI.final_merge <-  HSI.final_merge[order(HSI.final_merge$"90"),]

summary(HSI.final_merge$"90")

#### Add Quantiles ####
Quantiles <- 10
start<-1
partition <-nrow(HSI.final_merge)/Quantiles
pos <- partition
for ( i in 1:Quantiles)
{
  for(z in start:pos)
  {
    HSI.final_merge$QUANTILE[z]<-i
  }
  start=start+partition
  pos=pos+partition
}

HSI.final_merge$QUANTILE <- as.numeric(HSI.final_merge$QUANTILE)

##############################################################################################
############################# VALIDATION & ACCURACY ##########################################
##############################################################################################
HSI.final_merge$SNO <- c(1:nrow(HSI.final_merge))
attach(HSI.final_merge)

table(QUANTILE)
#### CHECK QUANTILE WISE ACCURACY  & PRECISION ####
MAT <-  table(HSI.final_merge[which(HAZARD == 1 & DUR <= 3),]$QUANTILE,HSI.final_merge[which(HAZARD == 1 & DUR <= 3),]$DUR)
MAT <- data.frame(Q = MAT[,0], M1 = MAT[,1], M2 = MAT[,2], M3 = MAT[,3])
MAT$TOTAL <- MAT$M1 + MAT$M2 + MAT$M3
MAT$CHURN.BY.BASE <-  MAT$TOTAL/60
MAT$PERC.OF.TOTAL <- MAT$TOTAL*100/(colSums(MAT)[4])
MAT

#### CHECK ROWWISE ACCURACY & PRECISION ####
size <- 6000
MAT2 <- table(HSI.final_merge[which(SNO <= size),]$SNO,HSI.final_merge[which(SNO <= size),]$DUR)
head(MAT2)
MAT2 <-   data.frame(ROW = c(1:size), CHURN = (MAT2[,1]+MAT2[,2]+MAT2[,3]))
head(MAT2)
MAT2$CUMULATIVE.CHURN <- cumsum(MAT2$CHURN)
MAT2$PERC <- MAT2$CUMULATIVE.CHURN*100 / MAT2$ROW
head(MAT2)
attach(MAT2)
range <- 6000
plot(ROW[1:range], PERC[1:range], type = "l")

table(HSI_RAW$UPG_WNG)

##############################################################################################
##################################### EXPORT #################################################
##############################################################################################

write.csv(HSI.final_merge,"C:/Users/AB37179/Documents/My Projects/HSI Predictive Churn Analysis/Survival Analysis/Model Results/15_BESTMOD_LOG_CALLMIGBINS_VOL.csv", row.names = FALSE)

##############################################################################################
################################EXPERIMENTAL WORK#############################################
##############################################################################################
############################################################

# Try segmenting the top 3 deciles to predict DURATION
colnames(HSI.final_merge)

# Take top 3 Deciles

HSI.final_merge.top3 <- HSI.final_merge[which(HSI.final_merge$DECILE == 1 | HSI.final_merge$DECILE==2 | HSI.final_merge$DECILE==3),]

# 1,2,3
colnames(HSI.final_merge.top3)
levels(HSI.final_merge.top3$DECILE)

table(HSI.final_merge.top3$DECILE)

#Build model based on complete HSI.final_merge data
##Tree_Model <- rpart(DUR~LEGACY+CUST_TEN+PROD_TEN+REGION+STATE+MKT_CLUSTER+CREDIT_RISK+INDUSTRY+SPEED+SPEED_BUCKET+CUSTOMER_AGE+REV_TREND+REV_MISS+REV_ZERO, data = HSI.final_merge, method = "class")

# Predict on Top 3 Deciles
HSI.Surv_Tree.Prediction <- predict(HSI.Surv_Tree, HSI.final_merge.top3)

#View(HSI.Surv_Tree.Prediction)

# VALIDATE RESULTS
HSI.Tree_Merge <- merge(HSI.final_merge.top3, HSI.Surv_Tree.Prediction, by="row.names")

HSI.final_merge.top3$TREE_DECILE <- HSI.Surv_Tree.Prediction



plot(HSI.Tree_Merge$DUR, HSI.Tree_Merge$y )
table(HSI.Tree_Merge$DUR, round(HSI.Tree_Merge$y))


# RESEARCH

install.packages("OIsurv")

data(tongue)
