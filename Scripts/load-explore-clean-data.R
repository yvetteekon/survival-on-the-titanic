# Load data
train <- read.csv("train.csv", header=TRUE, sep=",", stringsAsFactors = T)
test <- read.csv("test.csv", header=TRUE, sep=",", stringsAsFactors = T)

# Look at data
str(train)
str(test)
summary(train)

# Check class of variables
data.frame(sort(sapply(train, class)))
data.frame(sort(sapply(test, class)))

# Initialize a zero column for "Survived" in test data
test$Survived <- 0

# Reorder column "Survived" in test data
refcols <- c("PassengerId", "Survived")
test <- test[, c(refcols, setdiff(names(test), refcols))]

# Change variables Survived and Pclass into factors
grep("Survived", colnames(train))
grep("Pclass", colnames(train))
grep("Survived", colnames(test))
grep("Pclass", colnames(test))

for (i in c(2:3)){
  train[, i] <- as.factor(train[, i])
  test[, i] <- as.factor(test[, i])
}


