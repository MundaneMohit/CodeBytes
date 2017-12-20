#install packages and include libraries to perform recursive partioning

install.packages("rpart")
install.packages("ROSE") # For OVUN
install.packages("DMwR") # For SMOTE

library(DMwR)
library(ROSE)
library(rpart)
library(tree)

# include the raw csv file which contains the data datewise,portwise indicating an occurence of FCS Error
FCS_DATA <-read.csv("C:/Users/AB37179/Documents/My Projects/FCS Error Prediction/FCS Error Project/2059_Error_All.csv")

# check for the rows of the file
dim(FCS_DATA)
nrow(FCS_DATA)
#156640

#see the column names as we need to discard column indicating FCS error with no-2059
colnames(FCS_DATA)
names(FCS_DATA)
"CHASSIS.MAJOR.tmnxEqCardPChipError.2059"
"CHASSIS.MINOR.tmnxEqCardPChipError.2059"

# code to remove irrelevant columns
FCS_DATA<-FCS_DATA[,-59] # minor error
FCS_DATA<-FCS_DATA[,-41] # major error
FCS_DATA<-FCS_DATA[,-1] #logdate
FCS_DATA <- FCS_DATA[,-3] # router

# Treatment of null values
FCS_DATA[is.na(FCS_DATA)]<-0

# Check names to see if they're removed
names(FCS_DATA)

# check for levels & ratio of FCS Error that is the prediction variable 
class(FCS_DATA$'FCS_24HRS')
levels(FCS_DATA$'FCS_24HRS')
FCS_DATA$FCS_24HRS <- as.factor(FCS_DATA$FCS_24HRS)
table(FCS_DATA$'FCS_24HRS')
#N          Y 
#155985    655

# convert the data set to a data frame
FCS_DATA<-as.data.frame(FCS_DATA)

####################DIVIDE FIRST SMOTE LATER#######################

ratio <- floor(0.50*nrow(FCS_DATA))
set.seed(100)
samp_index <- sample(nrow(FCS_DATA), ratio)

FCS_DATA.train <- FCS_DATA[samp_index, ]
FCS_DATA.test  <- FCS_DATA[-samp_index,]


table(FCS_DATA.train$'FCS_24HRS')
table(FCS_DATA.test$FCS_24HRS)

####################
#### OPTION 2 ######
####################
table(FCS_DATA$'FCS_24HRS')

# Set ratio of test set
X <- nrow(FCS_DATA[which(FCS_DATA$FCS_24HRS == 'Y'),])  # No. of minority observations
R_maj <- 70           # Set %age of majority class
R_min <- 30          # Set %age of minority class
R_ovun <- R_maj/R_min  # Ratio between majority & minority

# Either set sample size to get perc.over
tot_samp <- 30000                         # Set total sample size
X_ov <-  tot_samp * R_min / (R_maj + R_min) # Count of minority observations based on sample size
perc.over  <- ((X_ov/X)) * 100              # Input 1 - Oversampling

# Or set oversampling ratio & get perc.over
times_over <- 10
X_ov <- X * times_over
perc.over <- (times_over - 1) * 100           # Input 1 - Oversampling

perc.under <- (R_ovun * X_ov)/(X_ov - X) *100 # Input 2 - Undersampling


# Create the training data set by oversampling and undersampling using SMOTE
set.seed(123)

FCS_DATA.balanced <- SMOTE(FCS_24HRS ~., data=FCS_DATA.train, perc.over = perc.over , perc.under = perc.under)

# Check Ratio
table(FCS_DATA.balanced$'FCS_24HRS')
#N     Y 
#11977  5082
###################################################################################


# perform rpart function by creating the model on the training data
FCS_ERROR_TREE <- rpart(FCS_24HRS~.,data=FCS_DATA.balanced)

# Visualize tree
library("partykit")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library("party")

fancyRpartPlot(FCS_ERROR_TREE)

# Predict on test set
FCS_PRED <- predict(FCS_ERROR_TREE , FCS_DATA.test, type = "class")

# Confusion matrix
table(OBSERVED = FCS_DATA.test$FCS_24HRS, PREDICTED = FCS_PRED)

# plot the tree model created
plot(FCS_ERROR_TREE)
text(FCS_ERROR_TREE,pretty=0)

# plot the tree with cp factor, size of the tree and relative error
plotcp(tree_model)


