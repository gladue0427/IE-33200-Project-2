# Algorithm that evaluates the five previous algorithms to determine the weighting of their results on the final classification.
# Load required libraries
library(caret)
library(e1071)
library(randomForest)
library(xgboost)
library(neuralnet)

# Load data and split into training and testing sets
data <- read.csv("data.csv")
trainIndex <- createDataPartition(data$Class, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train models on the training set
svmModel <- svm(Class ~ ., data = trainData)
rfModel <- randomForest(Class ~ ., data = trainData)
xgbModel <- xgboost(Class ~ ., data = trainData, nrounds = 10)
nnModel <- neuralnet(Class ~ ., data = trainData, hidden = c(5, 2))

# Use the trained models to predict the test set
svmPred <- predict(svmModel, testData)
rfPred <- predict(rfModel, testData)
xgbPred <- predict(xgbModel, testData)
nnPred <- predict(nnModel, testData)$net.result

# Combine predictions into a data frame
predDF <- data.frame(svmPred, rfPred, xgbPred, nnPred, testData$Class)

# Determine weights based on accuracy on the training set
svmAcc <- sum(svmPred == testData$Class) / nrow(testData)
rfAcc <- sum(rfPred == testData$Class) / nrow(testData)
xgbAcc <- sum(xgbPred == testData$Class) / nrow(testData)
nnAcc <- sum(nnPred == testData$Class) / nrow(testData)
totalAcc <- svmAcc + rfAcc + xgbAcc + nnAcc

svmWeight <- svmAcc / totalAcc
rfWeight <- rfAcc / totalAcc
xgbWeight <- xgbAcc / totalAcc
nnWeight <- nnAcc / totalAcc

# Calculate final prediction based on weighted average
finalPred <- (svmPred * svmWeight) + (rfPred * rfWeight) + (xgbPred * xgbWeight) + (nnPred * nnWeight)

# Evaluate final prediction accuracy
finalAcc <- sum(round(finalPred) == testData$Class) / nrow(testData)
