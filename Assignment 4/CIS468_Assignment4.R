library(caret)
library(pROC)
library(ggplot2)
library(WVPlots) 
######################################################################
#Load and partition data
######################################################################

#Load data & get summary 
diabetes <- read.csv(file="diabetes.csv")
summary(diabetes)
View(diabetes)


#Get a listing of variables
names(diabetes)

library(ggplot2)                                    	 
ggplot(diabetes, aes(x=age)) + 
  geom_histogram(binwidth=10, fill="gray") 



diabetes <- diabetes[,c(-1)]


#Visualize pairs plot of the diabetes data colored by class
pairs(diabetes[,-1],col=diabetes$class)

#We will use the origin field as the class variable. Let's convert it to a factor.
diabetes$class <- as.factor(diabetes$class)

#Partition the data into training and testing using the hold out method
#First, we need to set the random seed for repeatability
set.seed(4567)
#Create an index variable to perform a 70/30 split 
trainIndex <- createDataPartition(diabetes$class, p=.7, list=FALSE, times = 1)
diabetes_train <- diabetes[trainIndex,]
diabetes_test <- diabetes[-trainIndex,]

#Check the proportion of each origin in training and testing partitions
prop.table(table(diabetes_train$class)) * 100
prop.table(table(diabetes_test$class)) * 100

#Inspect the descriptive statistics for each variable in the training and testing partition
summary(diabetes_train)
summary(diabetes_test)


######################################################################
#Bagged Classifier using treebag method
######################################################################
trControl <- trainControl(method = 'cv')

bagFit <- train(class ~ ., data = diabetes_train, method = 'treebag', preProcess = c("center","scale"), trControl = trControl)

bagPredClass <- predict(bagFit,diabetes_test)

#Now evaluate the classifier using the confusionMatrix() function
confusionMatrix(bagPredClass, diabetes_test$class, mode="everything")



######################################################################
#Random Forest Using rf
######################################################################
trControl <- trainControl(method = 'cv')

rfFit <- train(class ~ ., data = diabetes_train, method = 'rf', preProcess = c("center","scale"), trControl = trControl)

rfPredClass <- predict(rfFit,diabetes_test)

#Now evaluate the classifier using the confusionMatrix() function
confusionMatrix(rfPredClass, diabetes_test$class, mode="everything")

######################################################################
#Boosted Classifier using XG-boost method
######################################################################
trControl <- trainControl(method = 'cv')

boostFit <- train(class ~ ., data = diabetes_train, method = 'xgbTree', preProcess = c("center","scale"), trControl = trControl)

boostPredClass <- predict(boostFit,diabetes_test)

#Now evaluate the classifier using the confusionMatrix() function
confusionMatrix(boostPredClass, diabetes_test$class, mode="everything")

rm(list = ls())

