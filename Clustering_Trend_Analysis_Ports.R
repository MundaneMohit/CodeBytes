#install.packages("TSclust")
#install.packages("fpc")
#install.packages("TSdist")
#install.packages("dtw")
library(dtw)
library(stats)
library("TSclust")
library(TSdist)
library(fpc)

### Raw Data
mydata_pcor2<-read.csv("C:/Users/AB37179/Documents/My Projects/FCS Error Prediction/Mohit Work Clustering/Final_PCOR_R_17032016.csv")

nrow(mydata_pcor2)
#1475
mydata_pcor2_col<-colnames(mydata_pcor2)
mydata_pcor2_col

###########REMOVE ROWS WITH UNEVEN COUNT for all Ports ######

mydata_pcor2$PORT.NO

mydata_pcor_2_24_27_28 <- mydata_pcor2[ which(mydata_pcor2$PORT.NO !='24'),]
nrow(mydata_pcor_2_24_27_28)
#1394
mydata_pcor_2_24_27_28 <- mydata_pcor_2_24_27_28[ which(mydata_pcor_2_24_27_28$PORT.NO !='27'),]
nrow(mydata_pcor_2_24_27_28)
#1312
mydata_pcor_2_24_27_28 <- mydata_pcor_2_24_27_28[ which(mydata_pcor_2_24_27_28$PORT.NO !='28'),]
nrow(mydata_pcor_2_24_27_28)
#1230
### removing Flag=1 ###########

#mydata_pcor_2_24_27_28_Flag0<- mydata_pcor_2_24_27_28
mydata_pcor_2_24_27_28_Flag0<- mydata_pcor_2_24_27_28[ which(mydata_pcor_2_24_27_28$FLAG_FOR_ZERO !='1'),]

#########REMOVE UNREQUIRED COLUMNS##########

mydata_pcor_2_24_27_28_Flag0_t<-mydata_pcor_2_24_27_28_Flag0
#mydata_pcor_2_24_27_28_Flag0_t<-mydata_pcor_2_24_27_28_Flag0[,c(-27,-1,-2)]

nrow(mydata_pcor_2_24_27_28_Flag0_t)
#1192


#####convert to data frame
mydata_pcor_2_24_27_28_Flag0_t<-data.frame(mydata_pcor_2_24_27_28_Flag0_t)

###########check for data frame######
is.data.frame(mydata_pcor_2_24_27_28_Flag0_t)
#True
nrow(mydata_pcor_2_24_27_28_Flag0_t)
#1192
##########################taking sample##############################
n2<-64
s2 <- sample(1:64, n2)
D<-NULL

# Unique ports
C<-unique(mydata_pcor_2_24_27_28_Flag0_t$PORT.NO)
C<-data.frame(C)

C
#?c

head(mydata_pcor_2_24_27_28_Flag0_t)
class(mydata_pcor_2_24_27_28_Flag0_t$PORT.NO)


Sample<-mydata_pcor_2_24_27_28_Flag0_t[which(mydata_pcor_2_24_27_28_Flag0_t$PORT.NO==C[13,1]),]

Sample

B<-Sample[s2,]
B
D<-B

#Sampling of all ports
for (i in 2:nrow(C))
{
  Sample<-mydata_pcor_2_24_27_28_Flag0_t[which(mydata_pcor_2_24_27_28_Flag0_t$PORT.NO==C[i,1]),]
  B<-Sample[s2,]
  D<-rbind(D,B)
}
DT<-D[,c(-28,-1,-2, -3)]
D
DT
#############################   ENDS ###########################
############# Hierarchial Clustering Using euclidean ####################
observedLabels <- c(rep(1,n2), rep(2,n2), rep(3,n2), rep(4,n2), rep(5,n2), rep(6,n2),rep(7,n2),rep(8,n2),rep(9,n2),rep(10,n2),rep(11,n2),rep(12,n2),rep(13,n2),rep(14,n2),rep(15,n2))
distMatrix <- dist(DT,method="euclidean")
DT
head(DT)
hc <- hclust(distMatrix,method="average")
plot(hc,col = "dark red",lwd = 0.20,main="Hierarchial Clustering",cex = .02)
#plot(hc,labels=observedLabels,col = "dark red",lwd = 0.20,main="Hierarchial Clustering",cex = .02)
pamk.best <- pamk(distMatrix)
pamk.best$nc
'head(distMatrix)'
'data.frame(distMatrix)'
rect.hclust(hc,k=6)
memb<-cutree(hc,k=6)
table(observedLabels,memb)
menm<-data.frame(memb)
########## Hierarchial Clustering Using Euclidean Ends########
############# Hierarchial Clustering Using DTW ####################
distMatrix1 <- dist(DT,method="DTW")
head(distMatrix1)
hc1 <- hclust(distMatrix1,method="average")
plot(hc1,main="DTW Hierarchial clustering")
#plot(hc1,labels=observedLabels,main="DTW Hierarchial clustering")
rect.hclust(hc1,k=6)
memb1<-cutree(hc1,k=6)
table(observedLabels,memb1)
menm1<-data.frame(memb1)
########## Hierarchial Clustering Using DTW Ends########
Final_Clusters<-cbind(D,memb,memb1)
Final_Clusters$FLAG<-ifelse(Final_Clusters$memb==Final_Clusters$memb1,'0','1')
#Final_Clusters<-Final_Clusters[,c(-2,-27)]
#Final_Clusters_dif<-Final_Clusters[which(Final_Clusters$FLAG==1),]

######################  Write the file ####################
A<-getwd()
A
Final_Clusters<-write.csv(Final_Clusters,file="Final_Clusters_Perc_Util2.csv",row.names=F)
########################## Code Ends #############################
