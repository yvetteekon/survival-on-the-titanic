### Load the train data
train <- read.csv("train.csv", header=TRUE, sep=",")
test <- read.csv("test.csv", header=TRUE, sep=",")
data <- read.csv("data.csv", header= TRUE, sep=",")
str(train) ######## rename the levels in Pclass
summary(train)
View(train)
names(train)
names(test)