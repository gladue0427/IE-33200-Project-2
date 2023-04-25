
fgsm_adversify <- function(input_image, input_label, loss_object) {
  
  
  with(tf$GradientTape() %as% tape, {
    # training = TRUE is only needed if there are layers with different
    # behavior during training versus inference (e.g. Dropout).
    tape$watch(input_image)
    prediction <- model(input_image)
    loss <- loss_object(input_label, prediction)
    gradient <- tape$gradient(loss, input_image)
  })
  
  
  signed_grad <- tf$sign(gradient)
  
  return(gradient)
}

to_tensor <- function(imagePath) {
  
  return (imagePath %>% tf$io$read_file() %>% tf$image$decode_image() %>% tf$cast(tf$float32) %>% tf$image$resize(tf$constant(c(as.integer(224), as.integer(224)), dtype=tf$int32)) %>% tf$keras$applications$mobilenet_v3$preprocess_input())
  
}

display_image_tensor <- function(x, ..., max = 255,
                                 plot_margins = c(0, 0, 0, 0)) {
  if(!is.null(plot_margins))
    par(mar = plot_margins)
  
  x %>%
    as.array() %>%
    drop() %>%
    as.raster(max = max) %>%
    plot(..., interpolate = FALSE)
}