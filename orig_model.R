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



source("algos.R")
source("mod_image.R")

model <- load_model_tf("dandelion_model")

res=c("","")
width <- 224
height<- 224
target_size <- c(width, height)
rgb <- 3 #color channels

f_grass <- list.files("grass")
f_dande <- list.files("dandelions")

pixel_budget <- 0.05

############################## TEST CODE ############################
test_image <- image_load("grass",
                         target_size = target_size)

# test_image <- image_load("dandelions",
#                          target_size = target_size)

#y <- image_load(paste("grass/",f_grass[1],sep=""), target_size = target_size)
image_name <- paste("dandelions/",f_dande[1],sep="")
y <- image_load(image_name,
                target_size = target_size)
y <- image_to_array(y)
y <- y/255
plot(1:dim(y)[1], 1:dim(y)[2], main=paste("Image: ",image_name,"\nNo Changes "), type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(as.raster(y), 1, 1, dim(y)[1], dim(y)[2])
y_inv <- y
y_inv <- mod_image(y, pixel_budget = pixel_budget)
# y_inv <- change_least_average(y)
plot(1:dim(y_inv)[1], 1:dim(y_inv)[2], main=paste("Image: ",image_name,"\nPixel Budget: ", pixel_budget), type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(as.raster(y_inv), 1, 1, dim(y_inv)[1], dim(y_inv)[2])
y_test <- array_reshape(y_inv, c(1, dim(y_inv)))
pred <- model %>% predict(y_test)
print(pred)


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
  x <- mod_image(x, pixel_budget = pixel_budget)
  x_test <- array_reshape(x, c(1, dim(x)))
  pred <- model %>% predict(x_test)
  print(pred)
  if(pred[1,1]<0.50){
    # print(i)
    # plot(1:224, 1:224, type = "n")
    # plot(1:dim(x)[1], 1:dim(x)[2], main=paste("Image: ",i,"\nPixel Budget: ", pixel_budget), type = "n", xlab = "", ylab = "", axes = FALSE)
    # rasterImage(as.raster(x), 1, 1, dim(x)[1], dim(x)[2])
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

