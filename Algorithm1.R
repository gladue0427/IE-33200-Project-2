# finds most green pixels to a random color
find_most_green <- function(y, pixel_budget=500) {
  # if the pixel budget was given as a percentage (< 1), then calculate the
  # number of pixels to be changed
  if (pixel_budget < 1) {
    pixel_budget <- floor(pixel_budget * dim(y)[1] * dim(y)[2])
  } else if (pixel_budget <= 0) {
    stop("ERROR: Pixel budget was non-positive")
  }
  
  # calculate the "greenness" of each pixel
  greenNess <- y[,,2] - y[,,1] - y[,,3]
  
  # finds indices of the most green pixels, limited to pixel budget
  most_green <- arrayInd(order(greenNess, decreasing = TRUE)[1:pixel_budget], dim(greenNess)) #which(greenNess > cutoff, arr.ind=TRUE)
  
  return(most_green)
}