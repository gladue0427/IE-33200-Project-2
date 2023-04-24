
fgsm_adversify <- function(input_image, input_label) {
  
  loss_object <- tf$keras$losses$categorical_crossentropy
  
  
  with(tf$GradientTape() %as% tape, {
    # training = TRUE is only needed if there are layers with different
    # behavior during training versus inference (e.g. Dropout).
    prediction <- model %>% predict(input_image)
    loss <- loss_object(input_label, prediction)
    gradient <- tape$gradient(loss, input_image)
  })
  
  
  signed_grad <- tf$sign(gradient)
  
  return(signed_grad)
}