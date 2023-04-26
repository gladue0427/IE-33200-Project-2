library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
library(here)

setwd("~/GitHub/IE-33200-Project-2")

## RUN SEPARATELY ##
install_tensorflow(extra_packages="pillow")

## RUN SEPARATELY ##
install_keras()




# this whole file is pretty messy I'll clean it up tomorrow






model <- load_model_tf("dandelion_model")

res=c("","")
width <- 224
height<- 224
target_size <- c(width, height)
rgb <- 3 #color channels

f_grass <- list.files("grass")
f_dande <- list.files("dandelions")

############################## TEST CODE ############################
test_image <- image_load("grass",
                         target_size = target_size)

# test_image <- image_load("dandelions",
#                          target_size = target_size)

#y <- image_load(paste("grass/",f_grass[1],sep=""), target_size = target_size)
y <- image_load(paste("dandelions/",f_dande[1],sep=""),
                target_size = target_size)
y <- image_to_array(y)
y <- y/255

source("algos.R")
############### Find "most green" pixels in image ####################
pixel_budget <- 0.01

y_inv <- mod_image(y, 0.1)
# y_inv <- change_least_average(y)
plot(1:dim(y)[1], 1:dim(y)[2], type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(as.raster(y_inv), 1, 1, dim(y)[1], dim(y)[2])
y_test <- array_reshape(y_inv, c(1, dim(y_inv)))
model %>% predict(y_test)

#############################################################################

num_fooled <- 0
for (i in f_grass){
  test_image <- image_load(paste("grass/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- x/255
  x <- mod_image(x, 0.05)
  x <- array_reshape(x, c(1, dim(x)))
  pred <- model %>% predict(x)
  if(pred[1,2]<0.50){
    num_fooled <- num_fooled + 1
    #print(i)
  }
}
print(num_fooled)

res=c("","")
num_fooled = 0
for (i in f_dande){
  test_image <- image_load(paste("dandelions/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- x/255
  #x <- convert_mostGreen(x)
  x <- mod_image(x, pixel_budget = 0.05)
  x_test <- array_reshape(x, c(1, dim(x)))
  pred <- model %>% predict(x_test)
  print(pred)
  if(pred[1,1]<0.50){
    # print(i)
    # plot(1:224, 1:224, type = "n")
    # rasterImage(as.raster(x), 1, 1, 224, 224)
    num_fooled = num_fooled + 1
  }
}
print(num_fooled)
print(res)



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

