# Method: Random Pixel Flip Algorithm
# Loads required packages
library(mlr)
library(randomForest)
library(parallelMap)

# Set up parallel processing
parallelStartMulticore(4)

# Load training data
my_data <- read.csv("train.csv")

# Define task
task <- makeClassifTask(data = my_data, target = "label")

# Define learner
learner <- makeLearner("classif.randomForest")

# Train model
model <- train(learner, task)

# Load image data to classify
image <- readJPEG("image.jpg")

# Convert image to data frame
image_df <- data.frame(t(matrix(as.vector(image), ncol = 3)))

# Predict class of image
prediction <- predict(model, newdata = image_df)

# Convert prediction to character
prediction <- as.character(prediction)

# Save predicted class to file
writeLines(prediction, "predicted_class.txt")
