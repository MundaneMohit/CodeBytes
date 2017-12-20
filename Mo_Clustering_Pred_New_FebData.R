####### PORT BASED CLUSTERING & PREDICTION FOR PHN4-PCOR-20 ROUTER #######
#################################
# CREATION_DATE: 04-07-2016     #
# AUTHOR: MOHIT BANSAL          #
#################################

# Install required libraries

#install.packages("TSclust")
#install.packages("fpc")
#install.packages("TSdist")
#install.packages("dtw")
#install.packages("amap")

?hcluster

# Import Libraries

library(dtw)
library(stats)
library("TSclust")
library(TSdist)
library(fpc)
library(amap)

### Raw Data
DATA_PCOR <- read.csv("C:/Users/AB37179/Documents/My Projects/FCS Error Prediction/Mohit Work Clustering/PHN4_PCOR_FCS_04202016.csv")

# Checking number of rows
nrow(DATA_PCOR)
#2618

###########check for data frame######
is.data.frame(DATA_PCOR)
#True

##########################Clustering all Ports##############################
# Since we have a minimum of 34 days for each port #
n2<- 34

# Unique ports
PORTS <- unique(DATA_PCOR$PORT)
PORTS <-data.frame(PORTS)
nrow(PORTS)
# 77 Unique Ports

# COPY DATA FOR CLUSTERING
CLUST_DATA <- DATA_PCOR

# Remove all columns except time-series [or additional metrics if added]
colnames(CLUST_DATA)
# Get data ready for clustering
CLUST_DATA <- CLUST_DATA[,c(-1,-2,-3, -28,-29,-30, -31,-32,-33, -34,-35,-36,-37, -38,-39,-40)]

colnames(CLUST_DATA)
#############################   ENDS ###########################
############# Hierarchial Clustering Using euclidean ####################

# ADD LABELS TO IDENTIFY DATA LATER
observedLabels <- rep(1:77, each=n2)

# CREATE DISTANCE MATRIX BEFORE CLUSTERING. 
# ?dist to see different distance calculation methods
DIST_MATRIX_EUC <- dist(CLUST_DATA,method="euclidean")

# FIND OPTIMAL NUMBER OF CLUSTERS
#pamk.best <- pamk(DIST_MATRIX_EUC)
#pamk.best$nc

# CLUSTERING OF THE DISTANCE MATRIX
# ?hclust to see different clustering methods
HCLUST_EUC_AVG <- hclust(DIST_MATRIX_EUC, method="average")

# VISUALIZE HEIRARCHICAL DENDOGRAM
#plot(HCLUST_EUC_AVG,col = "dark red",lwd = 0.20,main="Hierarchial Clustering",cex = .02)

# Draws rectangles around the branches of a dendrogram 
# highlighting the corresponding clusters.
#rect.hclust(HCLUST_EUC_AVG,k=6)

# Cuts a tree resulting from hclust into several groups 
# By specifying the desired number(s) of groups.
EUC <- cutree(HCLUST_EUC_AVG,k=6)

# SEE WHICH CLUSTERS PORTS FALL IN
table(observedLabels,EUC)
EUC <- data.frame(EUC)

# Map clustering results to original data
Final_Clusters<-cbind(DATA_PCOR,EUC)
########## Hierarchial Clustering Using Euclidean Ends########
############# Hierarchial Clustering Using DTW ####################

# CREATE DISTANCE MATRIX BEFORE CLUSTERING. 
# ?dist to see different distance calculation methods
DIST_MATRIX_DTW <- dist(CLUST_DATA,method="DTW")

# FIND OPTIMAL NUMBER OF CLUSTERS
#pamk.best <- pamk(DIST_MATRIX_DTW)
#pamk.best$nc   # 2

# CLUSTERING OF THE DISTANCE MATRIX
# ?hclust to see different clustering methods
HCLUST_DTW_AVG <- hclust(DIST_MATRIX_DTW,method="average")

# VISUALIZE HEIRARCHICAL DENDOGRAM
plot(HCLUST_DTW_AVG,main="DTW Hierarchial clustering")
#plot(HCLUST_DTW_AVG,labels=observedLabels,main="DTW Hierarchial clustering")

# Draws rectangles around the branches of a dendrogram 
# highlighting the corresponding clusters.
rect.hclust(HCLUST_DTW_AVG,k=6)

# Cuts a tree resulting from hclust into several groups 
# By specifying the desired number(s) of groups.
DTW <- cutree(HCLUST_DTW_AVG,k=6)

# SEE WHICH CLUSTERS PORTS FALL IN
table(observedLabels,DTW)
DTW <- data.frame(DTW)

# Map clustering results to original data
Final_Clusters<-cbind(Final_Clusters, DTW)

########## Hierarchial Clustering Using DTW Ends########

############# Hierarchial Clustering Using CORRELATION ####################

# hcluster command can be used to create distance matrix
# and do clustering at the same time
HCLUST_CORR_WARD <- hcluster(CLUST_DATA, method="correlation", link = "ward")
                             
rect.hclust(HCLUST_CORR_WARD,k=6)

# VISUALIZE HEIRARCHICAL DENDOGRAM
plot(HCLUST_CORR_WARD,main="DTW Hierarchial clustering")

# Draws rectangles around the branches of a dendrogram 
# highlighting the corresponding clusters.
rect.hclust(HCLUST_CORR_WARD,k=6)

# Cuts a tree resulting from hclust into several groups 
# By specifying the desired number(s) of groups.
CORRWARD <- cutree(HCLUST_CORR_WARD,k=6)

# SEE WHICH CLUSTERS PORTS FALL IN
table(observedLabels,CORRWARD)
CORRWARD <- data.frame(CORRWARD)

# Map clustering results to original data
Final_Clusters<-cbind(Final_Clusters, CORRWARD)

######################  Write the file ####################

Final_Clusters<-write.csv(Final_Clusters,file="Final_Clusters_FebData_04222016.csv",row.names=F)

########################## Code Ends #############################

# PREDICTIVE MODELLING ON THE DATA INCLUDING CLUSTERS #

colnames(Final_Clusters)
library(rpart)

# SET TRAIN-TEST Ratio
ratio <- floor(0.70*nrow(Final_Clusters))
samp_index <- sample(nrow(Final_Clusters), ratio)

train_data <- Final_Clusters[samp_index, ]
test_data  <- Final_Clusters[-samp_index,]

# MODEL 1 on FCS_ROUTER
FCSR_Tree <- rpart(FCS_ROUTER~PORT+CAPACITY+SUBSLOT+PORT.1+PPORT+FLAG.FOR.BLANK+FLAG.FOR.0+SD+VARIANCE+SLOPE+AVG_UTILIZATION+UTIL_CLASS+EUC+CORRWARD, data =train_data, method = "class")
FCSR_Predict <- predict(FCSR_Tree, test_data, type = "class")
Final_Pred <-cbind(test_data,FCSR_Predict)

# Check Results
table(OBSERVED = Final_Pred$FCS_ROUTER, PREDICTED = Final_Pred$FCSR_Predict)

f#######################################################################
# MODEL 2 on FCS

FCS_Tree <- rpart(FCS~CAPACITY+SUBSLOT+PORT.1+PPORT+FLAG.FOR.BLANK+FLAG.FOR.0+SD+VARIANCE+SLOPE+AVG_UTILIZATION+UTIL_CLASS+EUC+CORRWARD, data =train_data, method = "class")
FCS_Predict <- predict(FCS_Tree, test_data, type = "class")
Final_Pred <-cbind(Final_Pred,FCS_Predict)

# Check Results
table(OBSERVED = Final_Pred$FCS, PREDICTED = Final_Pred$FCS_Predict)

#######################################################################
# MODEL 3 on NEXT DAY FCS
NEXT_DAY_FCS_Tree <- rpart(NEXT_DAY_FCS~PORT+CAPACITY+SUBSLOT+PORT.1+PPORT+FLAG.FOR.BLANK+FLAG.FOR.0+SD+VARIANCE+SLOPE+AVG_UTILIZATION+UTIL_CLASS+EUC+CORRWARD, data =train_data, method = "class")
NEXT_DAY_FCS_Predict <- predict(NEXT_DAY_FCS_Tree, test_data, type = "class")
Final_Pred <-cbind(Final_Pred,NEXT_DAY_FCS_Predict)

# Check Results
table(OBSERVED = Final_Pred$NEXT_DAY_FCS, PREDICTED = Final_Pred$NEXT_DAY_FCS_Predict)

#######################################################################
### VISUALIZATION ###

library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)


fancyRpartPlot(FCSR_Tree)
fancyRpartPlot(FCS_Tree)
fancyRpartPlot(NEXT_DAY_FCS_Tree)


