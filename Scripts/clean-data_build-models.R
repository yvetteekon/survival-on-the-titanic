# Initialize libraries
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(Amelia)
library(randomForest)
library(party)
library(ROSE)

# Load data
train <- read.csv("train.csv", header=TRUE, sep=",", stringsAsFactors = T,
                  na.strings = c(""," ", NA))
test <- read.csv("test.csv", header=TRUE, sep=",", stringsAsFactors = T,
                 na.strings = c(""," ", NA))

# Initialize a column of zeros for variable Survived in test
match(names(train), names(test))
test$Survived <- rep(0, 418)

# Model 1: Decision Tree - CART Model.
model_dtree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                       Embarked, data=train, method="class")

# Plot decision tree
fancyRpartPlot(model_dtree) # Sex is the most predictive variable

# Prediction
test$Survived <- predict(model_dtree, newdata = test, type = "class")

# First submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "decisionTree.csv", row.names = FALSE)

# Feature Engineering
# Variable Title
combi <- rbind(train, test)

# Encode variable Name to character variable
str(combi$Name)
combi$Name <- as.character(combi$Name)

# Extract title without space and strings for each row
combi$Title <- sapply(combi$Name, FUN=function(x)
{strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
data.frame(table(combi$Title))

# Group similar titles into single category
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in%
              c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# Encode variable Title to factor variable
combi$Title <- factor(combi$Title)

# Variable Family Size
combi$FamilySize <- combi$SibSp + combi$Parch + 1
table(combi$FamilySize)

# Create frequency table for Variable Family Size
newtable <- data.frame(table(combi$FamilySize))
names(newtable) <- c("Family Size", "Number of Passengers")

# Variable Family ID size
# Extract surnames
combi$Surname <- sapply(combi$Name, FUN=function(x)
{strsplit(x, split='[,.]')[[1]][1]})
data.frame(table(combi$Surname))

# Append variable Family Size to Surname to create variable FamilyID
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
(data.frame(combi$FamilyID))

# Group family size of 2 and less. Small family size will have less trouble
# sticking together in the chaos
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(combi$FamilyID))

# Subset family of size 2 or less still not grouped and assign to "Small"
head(famIDs)
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

# Encode variable FamilyID to factor variable
combi$FamilyID <- factor(combi$FamilyID)

# Split data back into train and test data sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Model 2 : Decision Tree (CART Model) with Engineered Features
model_dtree1 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                        Embarked + Title + FamilySize + FamilyID,
                      data=train, method="class")

# Plot decision tree
fancyRpartPlot(model_dtree1)

# Prediction
test$Survived <- predict(model_dtree1, test, type = "class")

# Second Submission
submit <- data.frame(PassengerID = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "withEngineeredFeatures.csv", row.names = FALSE)

# Clean data
# Check for variables with missing values
colnames(combi)[colSums(is.na(combi)) > 0] # colSums gives the sum of each column

# Visualize variables with missing values
missmap(train, main = "Missing values vs observed")

# Prediction model to impute missing age values
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked
                + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")

# Impute missing age values with predicted age values
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

# Impute missing values for variable Embarked with mode
which(is.na(combi$Embarked))
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

# Impute missing values for variable Fare with mean
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# Reduce Factor Levels of variable FamilyID
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
data.frame(table(combi$FamilyID2))

# Split data into train and test
train <- combi[1:891,]
test <- combi[892:1309,]

# Improve Model Predictive Accuracy using Ensemble Methods
# Model 3 : Random Forests
set.seed(415)
model_rforest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp +
                                Parch + Fare + Embarked + Title + FamilySize +
                                FamilyID2, data=train, importance=TRUE, ntree=2000)

# Plot and measures
print(model_rforest)
importance(model_rforest)
varImpPlot(model_rforest)

# Prediction
test$Survived <- predict(model_rforest, newdata = test)

# Third Submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "randomForestModel.csv", row.names = FALSE)

# Model 4 : Conditional Inference Tree Model.
set.seed(415)
model_cforest <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp +
                           Parch + Fare + Embarked + Title + FamilySize
                         + FamilyID, data = train,
                         controls=cforest_unbiased(ntree=2000, mtry=3))

# Prediction:
test$Survived <- predict(model_cforest, test, OOB=TRUE, type = "response")

# Fourth Submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "conditionalInferenceModel.csv", row.names = FALSE)
# Model with highest accuracy rate of 0.81

# Check class distribution of variable Survived
round(prop.table(table(train$Survived)) * 100) # imbalanced data

# Modify imbalanced data
# SMOTE
train_rose <- ROSE(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                     Embarked, data = train, seed = 1)$data
table(train_rose$Survived)

# Model 5 : Logistic Regression
nlevels(train$Title)
model_logReg <- glm(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch +
                      Embarked, data = train, family = "binomial")

# Prediction
pred_prob <- predict(model_logReg, newdata = test, type = "response")
test$Survived <- ifelse(pred_prob > 0.5, 1, 0)

# Fifth Submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "logisticModel.csv", row.names = FALSE)

