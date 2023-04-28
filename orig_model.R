library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
#library(here)

## RUN SEPARATELY ##
install_tensorflow(extra_packages="pillow")

## RUN SEPARATELY ##
install_keras()

############ RUN ALL THESE LINES BEFORE USING MODEL ####################

setwd("~/GitHub/IE-33200-Project-2")
source("Main_Algorithm.R")
source("Algorithm1.R")
source("Algorithm2.R")
source("Algorithm3.R")
source("Algorithm4.R")
source("Algorithm5.R")

# run separately - may take a long time
model <- load_model_tf("dandelion_model")

res=c("","")
width <- 224
height<- 224
target_size <- c(width, height)
rgb <- 3 #color channels

f_grass <- list.files("grass")
f_dande <- list.files("dandelions")


############################## SINGLE GRASS IMAGE ############################

# upload original image - change image index to use new picture
grass_image_index <- 1
orig_grass_name <-paste("grass/",f_grass[grass_image_index],sep="")
orig_grass <- image_load(orig_grass_name, target_size = target_size)
orig_grass <- image_to_array(orig_grass)
total_pixels <- dim(orig_grass)[1] * dim(orig_grass)[2]
orig_grass <- orig_grass/255

# plot original image
plot(1:dim(orig_grass)[1], 1:dim(orig_grass)[2], main=paste("Grass Image Index: ", grass_image_index,"\nOriginal"),type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(as.raster(orig_grass), 1, 1, dim(orig_grass)[1], dim(orig_grass)[2])

# modify image 
pixel_budget <- 0.01 #percentage of pixels to change
mod_grass <- mod_image(orig_grass, pixel_budget=pixel_budget, type=1)

# plot modified image
plot(1:dim(mod_grass)[1], 1:dim(mod_grass)[2], main=paste("Image Index: ", grass_image_index, "\nModified with Pixel Budget = ", pixel_budget),type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(as.raster(mod_grass), 1, 1, dim(mod_grass)[1], dim(mod_grass)[2])

# make prediction using model
mod_grass_pred <- array_reshape(mod_grass, c(1, dim(mod_grass)))
prediction <- model %>% predict(mod_grass_pred)
print(prediction)

############################## SINGLE DANDELION IMAGE ############################

# upload original image - change image index to use new picture
dan_image_index <- 1
orig_dan_name <-paste("dandelions/",f_dande[dan_image_index],sep="")
orig_dan <- image_load(orig_dan_name, target_size = target_size)
orig_dan <- image_to_array(orig_dan)
orig_dan <- orig_dan/255

# plot original image
plot(1:dim(orig_dan)[1], xlim=c(1,dim(orig_dan)[1]), ylim=c(1,dim(orig_dan)[2]), main=paste("Dandelion Image Index: ", dan_image_index,"\nOriginal Image"),type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(as.raster(orig_dan), 1, 1, dim(orig_dan)[1], dim(orig_dan)[2])

# modify image
pixel_budget <- 0.01
mod_dan <- mod_image(orig_dan, pixel_budget=pixel_budget, type=1)

# plot modified image
plot(1:dim(mod_dan)[1], xlim=c(1,dim(mod_dan)[1]), ylim=c(1,dim(mod_dan)[2]), main=paste("Dandelion Image Index: ", dan_image_index,"\nModified with Pixel Budget = ", pixel_budget),type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(as.raster(mod_dan), 1, 1, dim(mod_dan)[1], dim(mod_dan)[2])

# make prediction using model
mod_dan_pred <- array_reshape(mod_dan, c(1, dim(mod_dan)))
prediction <- model %>% predict(mod_dan_pred)
print(prediction)

####################### FINDING IMAGES FOOLED FOR A SPECIFIC PIXEL BUDGET###############################

# grass images
num_fooled <- 0
pixel_budget <- 0.05
for (i in f_grass){
  test_image <- image_load(paste("grass/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- x/255
  x <- mod_image(x, pixel_budget = pixel_budget, type=1)
  x <- array_reshape(x, c(1, dim(x)))
  pred <- model %>% predict(x)
  if(pred[1,2]<0.50){
    num_fooled <- num_fooled + 1
    print(i)
  }
}
print(num_fooled)


# dandelion images
res=c("","")
num_fooled <- 0
pixel_budget <- 300
for (i in f_dande){
  test_image <- image_load(paste("dandelions/",i,sep=""), target_size = target_size)
  x <- image_to_array(test_image)
  x <- x/255
  x <- mod_image(x, pixel_budget = pixel_budget, type=1)
  x_test <- array_reshape(x, c(1, dim(x)))
  pred <- model %>% predict(x_test)
  #print(pred)
  if(pred[1,1]<0.50){
    print(i)
    # plot(1:224, 1:224, type = "n")
    # rasterImage(as.raster(x), 1, 1, 224, 224)
    num_fooled <- num_fooled + 1
  }
}
print(num_fooled)
print(res)


##################### PERFORMANCE SCORE ##############################

# store all dandelion images in an array
dandelion_images <- array(0, dim = c(length(f_dande), 224, 224, 3))
for (i in 1:length(f_dande)){
  test_image <- image_load(paste("dandelions/",f_dande[i],sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- x/255
  dandelion_images[i,,,] <- x
}

# Calculate algorithm performance score for dandelion images
# This section could take a long time to run
# It helps to restart R before running it
P <- 224 * 224
b <- round(P/100)
test_start <- 400
test_end <- b
#scores <- c()
total_score <- 0
for (i in test_start:test_end) {
  num_fooled <- 0
  for (j in 1:dim(dandelion_images)[1]) {
  x <- mod_image(dandelion_images[j,,,], pixel_budget = i, type=1)
  x_test <- array_reshape(x, c(1, dim(x)))
  pred <- model %>% predict(x_test)
  #print(pred)
    if(pred[1,1]<0.50){
      #print(j)
      num_fooled <- num_fooled + 1
    }
  }
  score <-  num_fooled * P/i
  #scores <- c(scores, score)
  
  total_score <- total_score + score
  print(paste("Pixel Budget: ", i, ", Cumulative Score: ", round(total_score, 3)))
}

algorithm_score <- sum(scores)
print(algorithm_score)

##################### fgsm attempt #############################

#label <- tf$one_hot(208)
# tf$keras$Model$predict(model, x)
# tensor_image <- to_tensor(paste("grass/",f_grass[1],sep=""))
# labels <- tf$constant(c(as.integer(1), as.integer(2)), shape=c(as.integer(1), as.integer(2)), dtype=tf$int32)
# loss_obj <- tf$keras$losses$CategoricalCrossentropy(model)
# perturbation <- fgsm_adversify(tf$reshape(tensor_image, shape=c(as.integer(1), as.integer(224), as.integer(224), as.integer(3))), labels, loss_obj)
# perturbation <- tf$reshape(perturbation, shape=c(as.integer(224), as.integer(224), as.integer(3)))
# 
# display_image_tensor(perturbation * 0.5 + 0.5)
# print(pred)

