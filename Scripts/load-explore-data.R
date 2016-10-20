# Initialize libraries
library(ggplot2)
library(gridExtra)
library(cowplot)

# Load data
train <- read.csv("train.csv", header=TRUE, sep=",", stringsAsFactors = T)
test <- read.csv("test.csv", header=TRUE, sep=",", stringsAsFactors = T)

# Look at data structure
str(train)
str(test)
summary(train)

# Check class of variables
data.frame(sort(sapply(train, class)))
data.frame(sort(sapply(test, class)))

# Initialize a column of zeros for Survived in testa
test$Survived <- 0

# Reorder column "Survived" in test data
refcols <- c("PassengerId", "Survived")
test <- test[, c(refcols, setdiff(names(test), refcols))]

# Encode variables Survived and Pclass into factors
grep("Survived", colnames(train))
grep("Pclass", colnames(train))
grep("Survived", colnames(test))
grep("Pclass", colnames(test))

for (i in c(2:3)){
  train[, i] <- as.factor(train[, i])
  test[, i] <- as.factor(test[, i])
}

# Encode variable Name to character variable
grep("Name", colnames(train))
grep("Name", colnames(test))

for (i in c(4)){
  train[, i] <- as.character(train[, i])
  test[, i] <- as.character(test[, i])
}

# Check variable Survived classes distribution
unique(train$Survived)
round(prop.table(table(train$Survived)) * 100) # severity of imbalanced classes

# Split train data into categorical and numerical variables
cat_train <- train[ ,c(2,3,5,12)]
num_train <- train[ ,c(6,7,8,10)]

# Univariate Visual Analysis
# Histogram and density plot function
hist <- function(a){
  ggplot(data = num_train, aes(x = a, y=..density..)) +
    geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +
    geom_density()
}

# Histogram and density plots
p1 <- hist(num_train$Age)
p2 <- hist(num_train$SibSp)
p3 <- hist(num_train$Parch)
p4 <- hist(num_train$Fare)
ph <- plot_grid (p1, p2, p3, p4, labels = c("Age",
                                           "Number of Siblings/Spouses Aboard",
                           "Number of Parents/ Children Aboard", "Fare"))
title <- ggdraw() + draw_label("Univariate Visual Analysis: Numerical Variables",
                               fontface = "bold")
plot_grid(title, ph, ncol =1, rel_heights = c(0.1, 1) )

# Bar plot function
bar <- function(b){
  ggplot(data = cat_train, aes(x = b)) +
    geom_bar(fill = "steelblue", color = "green", width = 0.8, stat = "count") +
    guides(fill = FALSE)
}

# Bar plots
p3 <- bar(cat_train$Survived) # more people died
p4 <- bar(cat_train$Pclass) # more of the passengers were in the lower class
p5 <- bar(cat_train$Sex) # more passengers were male
p6 <- bar(cat_train$Embarked) # most of the passengers embarked at Southampton
pb <- plot_grid (p3, p4, p5, p6,
                labels = c("Survived", "Passenger Class", "Gender", "Embarked"))
title <- ggdraw() + draw_label("Univariate Visual Analysis: Categorical Variables",
                               fontface = "bold")
plot_grid(title, pb, ncol = 1, rel_heights = c(0.1, 1))

# Bivariate Visual Analysis
# Correlation of numerical variables
library(corrgram)
corrgram (num_train, order=TRUE, lower.panel=panel.shade,
          upper.panel=panel.pie, text.panel=panel.txt,
          main="Correlation of Numerical Predictors")

# Stacked bar function
stacked_bar <- function(i){
  ggplot(cat_train, aes(x = i, fill = Survived)) +
    geom_bar(color="black", width = 0.8, stat = "count")
}

# Stacked bar plots
p7 <- stacked_bar(cat_train$Pclass)
p8 <- stacked_bar(cat_train$Sex)
p9 <- stacked_bar(cat_train$Embarked)
ps <- plot_grid(p7, p8, p9,
                labels = c("Passenger Class", "Gender", "Embarked"))
title <- ggdraw() + draw_label("Bivariate Visual Analysis: Categorical Variables",
                               fontface = "bold")
plot_grid(title, ps, ncol = 1, rel_heights = c(0.1, 1))

# Multivariate Visual Analysis
# Scatterplot
ggplot(train, aes(x = Age, y = Fare, color = Survived)) + geom_point() +
  xlab("Age") + ylab("Fare")

# Remove unwanted files
rm(num_train, cat_train)






