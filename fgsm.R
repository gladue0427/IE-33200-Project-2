
fgsm_adversify <- function(input_image, input_label) {
  
  loss_object <- tf$keras$losses$categorical_crossentropy
  
  
  with(tf$GradientTape() %as% tape, {
    # training = TRUE is only needed if there are layers with different
    # behavior during training versus inference (e.g. Dropout).
    prediction <- model(c(NULL, input_image))
    loss <- loss_object(input_label, prediction)
    gradient <- tape$gradient(loss, input_image)
  })
  
  
  #signed_grad <- tf$sign(gradient)
  
  return(gradient)
}

to_tensor <- function(imagePath) {
  
  return (imagePath %>% tf$io$read_file() %>% tf$image$decode_image() %>% tf$cast(tf$float32) %>% tf$image$resize(tf$constant(c(as.integer(224), as.integer(224)), dtype=tf$int32)) %>% tf$keras$applications$mobilenet_v3$preprocess_input())
  
}