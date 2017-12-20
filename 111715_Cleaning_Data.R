library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)

setwd("Kaggle/")

# Get the data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#Add Survived attribute to Test data
test$Survived <- NA

train_new <- train
test_new <- test

#Combine the Data
combi <- rbind(train, test)

##################################
##### Feature Engineering ########
#################################

#Factor to char
combi$Name <- as.character(combi$Name)

#Add Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

#Combine infrequent titles
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'

#Char to factor
combi$Title <- factor(combi$Title)

# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model. 
# This time you give method="anova" since you are predicting a continuous variable.
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title,
                       data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(predicted_age, combi[is.na(combi$Age),])


# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, we give them the value S.
# We code all embarkment codes as factors.
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#Add Family Size(Larger familes might have trouble surviving)
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Added Child indicator
combi$Child <- ifelse(combi$Age <= 16, 1, 0)

# Added Family Indicator
combi$HasFamily <- ifelse(combi$FamilySize > 1, 1, 0)

#Added Surname
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

#Add family ID to identify Families(Specific families might have trouble surviving)
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
table(combi$FamilyID)

#Families with less than 2 members are small
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

summary(combi$FamilyID)

#Family ID on basis of ticket
combi$FamilyID2 <- paste(as.character(combi$FamilySize), combi$Surname, combi$Ticket, sep="")
#table(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(combi$FamilyID2))
View(famIDs)
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID2[combi$FamilyID2 %in% famIDs$Var1] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
table(combi$FamilyID2)f
#View(combi[order(combi$FamilyID2),])


#################################################################################################
#Run rpart on this data

#Split into training & test
train_1 <- combi[1:891,]
test_1 <- combi[892:1309,]

#Prediction tree
prediction_tree_1 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + 
                                 Parch + Fare + Embarked + 
                                 Title + FamilySize + Child + HasFamily + FamilyID2 ,
                               data=train_1, method="class")

fancyRpartPlot(prediction_tree_1)

#Prediction on test set
my_prediction_1 <- predict(prediction_tree_1, test_1, "class")

#Solution for Kaggle
my_solution_rpart_famID <- data.frame(PassengerId = test_1$PassengerId, 
                                Survived = my_prediction_1)
#Write to csv
write.csv(my_solution_rpart_famID, file = "my_solution_rpart_famID.csv", row.names = FALSE)
#################################################################################################
#Run randomforest on this data

prediction_tree_2 <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + 
                             Parch + Fare + Embarked + 
                             Title + FamilySize + Child + HasFamily + FamilyID2 ,
                           data=train_1, method="class")

##########Won't run as levels in familyID too high##############################################

################################################################################################
# Try cforest (Conditional inference trees)
install.packages('party')
library(party)

# Set seed for consistent result
set.seed(786)

# Also set number of trees & number of random attributes to select 
prediction_tree_3 <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + 
                                    Parch + Fare + Embarked + 
                                    Title + FamilySize + Child + HasFamily + FamilyID2 ,
                                  data=train_1, controls=cforest_unbiased(ntree=2000, mtry=3))


my_prediction_3 <- predict(prediction_tree_3, test_1, OOB=TRUE, type = "response")

#Solution for Kaggle
my_solution_cforest_famID <- data.frame(PassengerId = test_1$PassengerId, 
                                      Survived = my_prediction_3)
#Write to csv
write.csv(my_solution_cforest_famID, file = "my_solution_cforest_famID.csv", row.names = FALSE)

#################################################################################################
# Remove data with fare 0 & predict using familyId & cforest 

#Fare is 0
combi[which(train$Fare!=0),]

summary(combidata$Fare)
summary(combi)

#Split into training & test
train_new <- combi[1:891,]
test_new <- combi[892:1309,]

# Remove records where fare is 0
train_new <- combi[which(train$Fare!=0),]


# Set seed for consistent result
set.seed(786)


# Also set number of trees & number of random attributes to select 
prediction_tree_4 <- cforest(as.factor(Survived) ~ Pclass + Sex + Age 
                             data=train_new, controls=cforest_unbiased(ntree=2000, mtry=3))


my_prediction_4 <- predict(prediction_tree_4, test_new, OOB=TRUE, type = "response")

#Solution for Kaggle
my_solution_cforest_famID_clean <- data.frame(PassengerId = test_1$PassengerId, 
                                        Survived = my_prediction_4)
#Write to csv
write.csv(my_solution_cforest_famID_clean, file = "my_solution_cforest_famID_clean.csv", row.names = FALSE)


##########################################################################################

max(combi$Fare)


train_new
