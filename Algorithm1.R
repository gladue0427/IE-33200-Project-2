# converts most green pixels to a random color
convert_mostGreen <- function(y, pixel_budget=0.01) {
  # calculate the "greenness" of the image
  greenNess <- y[,,2] - y[,,1] - y[,,3]
  
  # finds indices of the most green pixels, limited to pixel budget
  cutoff <- quantile(greenNess, 1 - pixel_budget, type=1)
  most_green <- which(greenNess > cutoff, arr.ind=TRUE)
  
  #return(most_green)
  
  # changes most green pixels to a random color
  for (i in 1:dim(most_green)[1]) {
    pixel <- most_green[i,]
    
    # make pixel a random color
    y[pixel[1], pixel[2], 1] <- runif(1)
    y[pixel[1], pixel[2], 2] <- runif(1)
    y[pixel[1], pixel[2], 3] <- runif(1)
    
  }
  return(y)
}