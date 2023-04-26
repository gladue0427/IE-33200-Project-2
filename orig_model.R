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
y_inv <- y
#y_inv <- convert_mostGreen(y, pixel_budget = pixel_budget)
y_inv <- convert_mostYellow(y, pixel_budget = pixel_budget)
y_test <- array_reshape(y_inv, c(1, dim(y_inv)))
model %>% predict(y_test)
plot(1:224, 1:224, type = "n")
rasterImage(as.raster(y_inv), 1, 1, 224, 224)

##########################################################################

y_inv <- y
#y_inv[,,1] <- abs(y_inv[,,1] - y_inv[,,2])
#y_inv[abs(y[,,1] - y[,,2]) < 0.1] <- 0
low <- floor(45 * dim(y_inv)[1] / 100)
hi <- floor(55* dim(y_inv)[1] / 100)
ind_seq <- seq(from=0, to=dim(y)[1], by=10)
y_inv[ind_seq, ind_seq, 1:3] <- runif(3)
y_inv[low:hi, low:hi, ] <- 0.8
#y_inv[low:hi, low:hi, 3] <- 0
#y_inv[,,1] <- reduceUpperExtremities(y[,,1], 0.99)
y <- array_reshape(y, c(1, dim(y)))
model %>% predict(y)

y_rast <- as.raster(y_inv)
plot(1:224, 1:224, type = "n")
rasterImage(y_rast, 1, 1, 224, 224)


x <- image_to_array(test_image)
x <- array_reshape(x, c(1, dim(x)))
x <- x/255
pred <- model %>% predict(x)


##################### fgsm attempt #############################

#label <- tf$one_hot(208)
tf$keras$Model$predict(model, x)
tensor_image <- to_tensor(paste("grass/",f_grass[1],sep=""))
labels <- tf$constant(c(as.integer(1), as.integer(2)), shape=c(as.integer(1), as.integer(2)), dtype=tf$int32)
loss_obj <- tf$keras$losses$CategoricalCrossentropy(model)
perturbation <- fgsm_adversify(tf$reshape(tensor_image, shape=c(as.integer(1), as.integer(224), as.integer(224), as.integer(3))), labels, loss_obj)
perturbation <- tf$reshape(perturbation, shape=c(as.integer(224), as.integer(224), as.integer(3)))

display_image_tensor(perturbation * 0.5 + 0.5)
print(pred)


#############################################################################

for (i in f_grass){
  test_image <- image_load(paste("grass/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x/255
  pred <- model %>% predict(x)
  if(pred[1,2]<0.50){
    print(i)
  }
}

res=c("","")
for (i in f_dande){
  test_image <- image_load(paste("dandelions/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- x/255
  x <- convert_mostYellow(x, pixel_budget = )
  x <- array_reshape(x, c(1, dim(x)))
  pred <- model %>% predict(x)
  print(pred)
  if(pred[1,1]<0.50){
    print(i)
  }
}
print(res)