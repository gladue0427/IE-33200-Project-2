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

model <- load_model_tf("model/dandelion_model")

res=c("","")
width <- 224
height<- 224
target_size <- c(width, height)
rgb <- 3 #color channels

f_grass <- list.files("data-for-332/grass")

test_image <- image_load(paste("data-for-332/grass/",f_grass[1],sep=""),
                         target_size = target_size)
x <- image_to_array(test_image)
x <- array_reshape(x, c(1, dim(x)))
x <- x/255
fgsm_adversify(x, 1)

pred <- model %>% predict(x)


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