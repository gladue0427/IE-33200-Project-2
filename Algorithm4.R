# Method: Stack Generalization using majority voting
# Load required packages
library(caret)
library(mlr)
library(fastAdaboost)

# Load the training dataset
data <- read.csv("training_data.csv")

# Define the target variable
target_var <- "class"

# Split data into training and validation sets
set.seed(123)
trainIndex <- createDataPartition(data[,target_var], p = 0.8, list = FALSE)
train <- data[trainIndex,]
validation <- data[-trainIndex,]

# Define the feature set and create a task object
features <- names(data)[!names(data) %in% target_var]
task <- makeClassifTask(data = train, target = target_var, features = features)

# Define the base learners
base_learners <- list(
  makeLearner("classif.rpart"),
  makeLearner("classif.naiveBayes"),
  makeLearner("classif.randomForest")
)

# Define the ensemble learner
ensemble_learner <- makeStackedLearner(base_learners, method = "majority", predict.type = "prob")

# Train the ensemble model
model <- train(ensemble_learner, task)

# Evaluate the model on the validation set
predictions <- predict(model, newdata = validation)
confusionMatrix(predictions$data$truth, predictions$data$response)
