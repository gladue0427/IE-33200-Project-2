# converts most yellow pixels to the average color of the image
find_most_yellow <- function(y, pixel_budget=500) {
  # if the pixel budget was given as a percentage (< 1), then calculate the
  # number of pixels to be changed
  if (pixel_budget < 1) {
    pixel_budget <- floor(pixel_budget * dim(y)[1] * dim(y)[2])
  } else if (pixel_budget <= 0) {
    stop("ERROR: Pixel budget was non-positive")
  }
  
  # mean values of each color in image
  mean_R <- mean(y[,,1])
  mean_G <- mean(y[,,2])
  mean_B <- mean(y[,,3])
  
  # calculate "yellowness" of each pixel
  yellowNess <- y[,,1] + y[,,2] - y[,,3]
  
  # store the indices of percentage of pixels indicated by the pixel budget that
  # are the most yellow
  #cutoff <- quantile(yellowNess, 1 - pixel_budget, type=1)
  most_yellow <- arrayInd(order(yellowNess, decreasing = TRUE)[1:pixel_budget], dim(yellowNess))#which(yellowNess > cutoff, arr.ind=TRUE)
  
  return(most_yellow)
  
  # change the color of the most yellow pixels
  # for (i in 1:dim(most_yellow)[1]) {
  #   pixel <- most_yellow[i,]
  #   
  #   ################ Make Pixel Green-ish #############
  #   # y[pixel[1], pixel[2], 1] <- runif(1, 0, 0.1)
  #   # y[pixel[1], pixel[2], 2] <- runif(1, 0.3, 0.8)
  #   
  #   
  #   ########Make Pixel Average Color of Image #######
  #   y[pixel[1], pixel[2], 1] <- mean_R
  #   y[pixel[1], pixel[2], 2] <- mean_G
  #   y[pixel[1], pixel[2], 3] <- mean_B
  # }
  # return(y)
}