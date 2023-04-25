library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
library(here)

setwd(here())

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

f_grass <- list.files("data-for-332/grass")

test_image <- image_load(paste("data-for-332/grass/",f_grass[1],sep=""),
                         target_size = target_size)
y <- image_load(paste("data-for-332/grass/",f_grass[8],sep=""),
              target_size = target_size)
y <- image_to_array(y)
y <- y/255
y_inv <- y
y_test <- array_reshape(y, c(1, dim(y)))
model %>% predict(y_test)
plot(1:224, 1:224, type = "n")
rasterImage(as.raster(y), 1, 1, 224, 224)

y_inv <- y
y_inv[,,1] <- abs(y_inv[,,1] - y_inv[,,2])
y_inv[abs(y[,,1] - y[,,2]) < 0.1] <- 0
y_inv_test <- array_reshape(y_inv, c(1, dim(y_inv)))
model %>% predict(y_inv_test)

y_rast <- as.raster(y_inv)
plot(1:224, 1:224, type = "n")
rasterImage(y_rast, 1, 1, 224, 224)


x <- image_to_array(test_image)
x <- array_reshape(x, c(1, dim(x)))
x <- x/255
pred <- model %>% predict(x)
label <- tf$one_hot(208)
tf$keras$Model$predict(model, x)
tensor_image <- to_tensor(paste("data-for-332/grass/",f_grass[1],sep=""))
labels <- tf$constant(c(as.integer(0), as.integer(1)), shape=c(as.integer(1), as.integer(2)), dtype=tf$int32)
perturbation <- fgsm_adversify(tf$reshape(tensor_image, shape=c(as.integer(1), as.integer(224), as.integer(224), as.integer(3))), labels)
perturbation <- tf$reshape(perturbation, shape=c(as.integer(224), as.integer(224), as.integer(3)))
display_image_tensor(perturbation * 0.5 + 0.5)
print(pred)

for (i in f){
  test_image <- image_load(paste("data-for-332/grass/",i,sep=""),
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
f_dande <- list.files(here("data-for-332/dandelions"))
for (i in f){
  test_image <- image_load(paste("data-for-332/dandelions/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x/255
  pred <- model %>% predict(x)
  if(pred[1,1]<0.50){
    print(i)
  }
}
print(res)