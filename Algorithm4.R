change_most_average <- function(y, pixel_budget=500) {
  if (pixel_budget < 1) {
    return(y)
  }
  # averages for each color value in image
  mean_R <- mean(y[,,1])
  mean_G <- mean(y[,,2])
  mean_B <- mean(y[,,3])
  
  # calculates how far away each pixel's color is from the average image color
  dist_from_avg <- abs(y[,,1] - mean_R) + abs(y[,,2] - mean_G) + abs(y[,,3] - mean_B)
  
  # finds indices of the most average pixels, limited to the pixel budget
  #cutoff <- quantile(dist_from_avg, pixel_budget, type=1)
  most_avg <- arrayInd(order(dist_from_avg, decreasing = TRUE)[1:pixel_budget], dim(dist_from_avg))#which(dist_from_avg < cutoff, arr.ind = TRUE)
  
  # changes colors of the most average pixels
  for (i in 1:dim(most_avg)[1]) {
    pixel <- most_avg[i,]
    
    ############### Make pixel a random color #########
    y[pixel[1], pixel[2], 1] <- runif(1)
    y[pixel[1], pixel[2], 2] <- runif(1)
    y[pixel[1], pixel[2], 3] <- runif(1)
  }
  return(y)
}