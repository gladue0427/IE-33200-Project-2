# identifies the least average color pixels out of the image
find_least_average <- function(y, pixel_budget=500) {
  # if the pixel budget was given as a percentage (< 1), then calculate the
  # number of pixels to be changed
  if (pixel_budget < 1) {
    pixel_budget <- floor(pixel_budget * dim(y)[1] * dim(y)[2])
  } else if (pixel_budget <= 0) {
    stop("ERROR: Pixel budget was non-positive")
  }
  
  # averages for each color value in image
  mean_R <- mean(y[,,1])
  mean_G <- mean(y[,,2])
  mean_B <- mean(y[,,3])
  
  # calculates how far away each pixel's color is from the average image color
  dist_from_avg <- abs(y[,,1] - mean_R) + abs(y[,,2] - mean_G) + abs(y[,,3] - mean_B)
  
  # finds indices of the least average pixels, limited to the pixel budget
  #cutoff <- quantile(dist_from_avg, 1 - pixel_budget, type=1)
  least_avg <- arrayInd(order(dist_from_avg, decreasing = FALSE)[1:pixel_budget], dim(dist_from_avg)) #which(dist_from_avg > cutoff, arr.ind = TRUE)
  
  # changes colors of the least
  # for (i in 1:dim(least_avg)[1]) {
  #   pixel <- least_avg[i,]
  #   
  #   ############### Make pixel a random color #########
  #   # y[pixel[1], pixel[2], 1] <- runif(1)
  #   # y[pixel[1], pixel[2], 2] <- runif(1)
  #   # y[pixel[1], pixel[2], 3] <- runif(1)
  #   
  #   ########Make Pixel Average Color of Image #######
  #   y[pixel[1], pixel[2], 1] <- mean_R
  #   y[pixel[1], pixel[2], 2] <- mean_G
  #   y[pixel[1], pixel[2], 3] <- mean_B
  # }
  return(least_avg)
}