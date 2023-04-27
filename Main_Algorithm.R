mod_image <- function(y, pixel_budget = 0.01, type = 0) {
  # if the pixel budget was given as a percentage (< 1), then calculate the
  # number of pixels to be changed
  if (pixel_budget < 1) {
    pixel_budget <- floor(pixel_budget * dim(y)[1] * dim(y)[2])
  } else if (pixel_budget <= 0) {
    stop("ERROR: Pixel budget was non-positive")
  }
  
  # if original image is grass
  if (type == 0) {
    
  }

  else if (type == 1) {
    y <- change_most_average(y, pixel_budget = floor(pixel_budget/10) * 2)
    y <- change_least_average(y, pixel_budget = floor(pixel_budget/10) * 2)
    y <- convert_mostGreen(y, pixel_budget = floor(pixel_budget/10) * 2)
    y <- convert_mostYellow(y, pixel_budget = floor(pixel_budget/10) * 2)
    y <- randomPixels(y, pixel_budget = floor(pixel_budget/10) * 2)
  }
  return(y)
}