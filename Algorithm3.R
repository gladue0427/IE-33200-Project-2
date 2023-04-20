# Method: Fast Gradient Sign Method
# Contains support vector machine classifer
# Load required libraries
library(e1071)
library(adversarials)
library(imager)

# Load pre-trained classifier
classifier <- readRDS("svm_classifier.rds")

# Load example image
image <- load.image("example_image.png")

# Convert image to matrix and normalize pixel values
image_matrix <- as.matrix(image)
image_matrix <- (image_matrix - mean(image_matrix)) / sd(image_matrix)

# Define adversarial function using FGSM attack
fgsm_attack <- function(image, epsilon) {
  # Calculate gradient of loss with respect to input image
  gradient <- gradient(model = classifier, x = image, loss_function = "hinge_loss")
 
  # Add perturbation to image based on gradient
  perturbed_image <- image + epsilon * sign(gradient)
 
  # Clip pixel values to range [0, 1]
  perturbed_image <- pmax(0, pmin(perturbed_image, 1))
 
  return(perturbed_image)
}

# Define function to evaluate accuracy of classifier on adversarial examples
evaluate_adversarial_accuracy <- function(classifier, image, epsilon) {
  # Generate adversarial example using FGSM attack
  adversarial_image <- fgsm_attack(image, epsilon)
 
  # Classify adversarial example and return predicted class
  prediction <- predict(classifier, t(adversarial_image))
  return(prediction)
}

# Evaluate accuracy of classifier on original image
original_prediction <- predict(classifier, t(image_matrix))
cat("Classifier prediction for original image:", original_prediction, "\n")

# Evaluate accuracy of classifier on adversarial example with epsilon = 0.1
adversarial_prediction <- evaluate_adversarial_accuracy(classifier, image_matrix, 0.1)
cat("Classifier prediction for adversarial image with epsilon = 0.1:", adversarial_prediction, "\n")
