reduceUpperExtremities <- function(x, pixel_budget) {
  cutoff <- quantile(x, 1-pixel_budget, type=1)
  print(cutoff)
  ext_removed <- apply(x, 1, function(r){r[r > cutoff] <- 0})
  return(ext_removed)
}

convert_mostGreen <- function(x, pixel_budget=0.01) {
  y <- x
  greenNess <- y[,,2] - y[,,1] - y[,,3]
  cutoff <- quantile(greenNess, 1 - pixel_budget, type=1)
  most_green <- which(greenNess > cutoff, arr.ind=TRUE)
  for (i in 1:dim(most_green)[1]) {
    pixel <- most_green[i,]
    # y[pixel[1], pixel[2], 1] <- runif(1)
    # y[pixel[1], pixel[2], 2] <- runif(1)
    # y[pixel[1], pixel[2], 3] <- runif(1)
    y[pixel[1], pixel[2], 1:2] <- runif(1, 0.78, 1)
    
    y[pixel[1], pixel[2], 3] <- 0
  }
  return(y)
}

convert_mostYellow <- function(x, pixel_budget=0.01) {
  y <- x
  mean_R <- mean(y[,,1])
  mean_G <- mean(y[,,2])
  mean_B <- mean(y[,,3])
  # calculate "yellowness" of pictures
  #yellowNess <- abs(y[,,1]-y[,,2])
  #yellowNess <- y[,,1] * 0.89 + y[,,2] * 0.78 - y[,,3] * 0.65
  yellowNess <- y[,,1] + y[,,2] - y[,,3] 
  #yellowNess[x < 0] <- 0
  # determine what the 
  cutoff <- quantile(yellowNess, 1 - pixel_budget, type=1)
  most_yellow <- which(yellowNess > cutoff, arr.ind=TRUE)
  
  for (i in 1:dim(most_yellow)[1]) {
    pixel <- most_yellow[i,]
    # y[pixel[1], pixel[2], 1] <- runif(1, 0, 0.1)
    # y[pixel[1], pixel[2], 2] <- runif(1, 0.3, 0.8)
    # y[pixel[1], pixel[2], 1] <- runif(1)
    # y[pixel[1], pixel[2], 2] <- runif(1)
    # y[pixel[1], pixel[2], 3] <- runif(1)
    
    
    
    ############### Make Pixel Average Color of Image
    y[pixel[1], pixel[2], 1] <- mean_R
    y[pixel[1], pixel[2], 2] <- mean_G
    y[pixel[1], pixel[2], 3] <- mean_B
    
    ################ Make Pixels Yellow ###############
    # y[pixel[1], pixel[2], 1:2] <- runif(1, 0.78, 1)
    # 
    # y[pixel[1], pixel[2], 3] <- 0
  }
  return(y)
}

randomPixels <- function(x, pixel_budget) {
  change_pixels <- sample(x, floor(x * pixel_budget))
}