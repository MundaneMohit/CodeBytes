#install packages and include libraries to perform recursive partioning

install.packages("rpart")
install.packages("ROSE") # For OVUN
install.packages("DMwR") # For SMOTE

library(DMwR)
library(ROSE)
library(rpart)
library(tree)

# include the raw csv file which contains the data datewise,portwise indicating an occurence of FCS Error
FCS_TEST<-read.csv("C:/Users/AB37179/Documents/My Projects/FCS Error Prediction/FCS Error Project/2059_Error_All.csv")

# check for the rows of the file
dim(FCS_TEST)
nrow(FCS_TEST)
#156640

#see the column names as we need to discard column indicating FCS error with no-2059
colnames(FCS_TEST)
names(FCS_TEST)
"CHASSIS.MAJOR.tmnxEqCardPChipError.2059"
"CHASSIS.MINOR.tmnxEqCardPChipError.2059"

# code to remove irrelevant columns
FCS_TEST<-FCS_TEST[,-59] # minor error
FCS_TEST<-FCS_TEST[,-41] # major error
FCS_TEST<-FCS_TEST[,-1] #logdate
FCS_TEST <- FCS_TEST[,-3] # router

# Treatment of null values
FCS_TEST[is.na(FCS_TEST)]<-0

# Check names to see if they're removed
names(FCS_TEST)

# check for levels & ratio of FCS Error that is the prediction variable 
class(FCS_TEST$'FCS_24HRS')
levels(FCS_TEST$'FCS_24HRS')
FCS_TEST$FCS_24HRS <- as.factor(FCS_TEST$FCS_24HRS)
table(FCS_TEST$'FCS_24HRS')
#N          Y 
#155985    655

# convert the data set to a data frame
FCS_TEST<-as.data.frame(FCS_TEST)


##################### TWO APPROACHES ###########################

####################
#### OPTION 1 ######
####################
# create the training data set by oversampling and undersampling using ROSE
set.seed(786)

FCS_TEST.balanced <- ovun.sample(FCS_24HRS ~., data=FCS_TEST, 
                                  p=0.3, N = 30000, seed=1, 
                                  method="both")$data

# Check Ratio
table(FCS_TEST.balanced$'FCS_24HRS')
#N     Y 
#21044  8956 

####################
#### OPTION 2 ######
####################
table(FCS_TEST$'FCS_24HRS')

# Set ratio of test set
X <- nrow(FCS_TEST[which(FCS_TEST$FCS_24HRS == 'Y'),])  # No. of minority observations
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

FCS_TEST.balanced <- SMOTE(FCS_24HRS ~., data=FCS_TEST, perc.over = perc.over , perc.under = perc.under)

# Check Ratio
table(FCS_TEST.balanced$'FCS_24HRS')
#N     Y 
#20246 10473 

###################################################################################

# Divide into train & test
ratio <- floor(0.70*nrow(FCS_TEST.balanced))
set.seed(911)
samp_index <- sample(nrow(FCS_TEST.balanced), ratio)

FCS_TEST.train <- FCS_TEST.balanced[samp_index, ]
FCS_TEST.test  <- FCS_TEST.balanced[-samp_index, ]

# Check ratio
table(FCS_TEST.train$'FCS_24HRS')
table(FCS_TEST.test$FCS_24HRS)

# to see first 5 rows of the training set created
#head(training_set)

# create the testing set by selecting remaining rows of the training set
#testing=-training_set
#training_data=FCS_TEST[training_set,]
#testing_data=FCS_TEST[testing,]


# perform rpart function by creating the model on the training data
FCS_ERROR_TREE <- rpart(FCS_24HRS~.,data=FCS_TEST.train)

# Visualize tree
library("party")
fancyRpartPlot(FCS_ERROR_TREE)

# Predict on test set
FCS_PRED <- predict(FCS_ERROR_TREE, FCS_TEST.test, type = "class")

# Confusion matrix
table(OBSERVED = FCS_TEST.test$FCS_24HRS, PREDICTED = FCS_PRED)

# plot the tree model created
plot(FCS_ERROR_TREE)
text(FCS_ERROR_TREE,pretty=0)

# plot the tree with cp factor, size of the tree and relative error
plotcp(tree_model)



