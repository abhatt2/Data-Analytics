#Import required libraries
#This demo will use the Caret package for classification
library(caret)
library(pROC)
library(ggplot2)
library(WVPlots) 

#Load data & get summary 
diabetes <- read.csv(file="diabetes.csv")
summary(diabetes)

#Get a listing of variables
names(diabetes)


library(ggplot2)                                    	 
ggplot(diabetes, aes(x=age)) + 
  geom_histogram(binwidth=10, fill="gray") 

diabetes <- diabetes[,c(-1)]

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



###################################################################
#Train K-Nearest Neighbor Classifier with repeated 10-fold cross validation
###################################################################
ctrl_cv <- trainControl(method="repeatedcv", repeats = 3)
knnFit_cv <- train(class ~ ., data = diabetes_train, method = "knn", trControl = ctrl_cv, preProcess = "scale")
knnFit_cv


#Visualize the result of the cross-validation
plot(knnFit_cv)

#Now we're ready to predict our test data in order to evaluate the performance of the model
knnPredict_cv <- predict(knnFit_cv, newdata = diabetes_test)
knnPredict_cv

#Now evaluate the classifier using the confusionMatrix() function
confusionMatrix(knnPredict_cv, diabetes_test$class, mode="everything")

###########################################################################################
#Train Decision Tree using the CART algorithm and cross validation
###########################################################################################
ctrl <- trainControl(method="cv")
treeFit <- train(class ~ ., data = diabetes_train, method = "rpart", trControl = ctrl)
treeFit
summary(treeFit$finalModel)

#Check the attribute importance of the tree. This will show you the attribute 
varImp(treeFit,scale=FALSE)

#Plot a simple representation of the decision tree
plot(treeFit$finalModel, uniform=TRUE)
text(treeFit$finalModel, all=TRUE, cex=.8)

#The rpart.plot library creates a more visually appealing tree (install.packages("rpart.plot")) 
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(treeFit$finalModel)

#Now we're ready to predict our test data in order to evaluate the performance of the model
treePredict <- predict(treeFit, newdata = diabetes_test)
treePredict

#Now evaluate the classifier using the confusionMatrix() function
confusionMatrix(treePredict, diabetes_test$class, mode="everything")

###########################################################################################
#Train Naive Bayes Classifier using cross validation
###########################################################################################
ctrl <- trainControl(method="cv")
nbFit <- train(class ~ ., data = diabetes_train, method = "nb", trControl = ctrl, preProcess="scale")
nbFit

#Now we're ready to predict our test data in order to evaluate the performance of the model
nbPredict <- predict(nbFit, newdata = diabetes_test)
nbPredict

#Now evaluate the classifier using the confusionMatrix() function
confusionMatrix(nbPredict, diabetes_test$class, mode="everything")


rm(list = ls())

