reduceUpperExtremities <- function(x, pixel_budget) {
  cutoff <- quantile(x, 1-pixel_budget, type=1)
  print(cutoff)
  ext_removed <- apply(x, 1, function(r){r[r > cutoff] <- 0})
  return(ext_removed)
}

convert_mostGreen <- function(y, pixel_budget=0.01) {
  # calculate the "greenness" of the image
  greenNess <- y[,,2] - y[,,1] - y[,,3]
  
  
  cutoff <- quantile(greenNess, 1 - pixel_budget, type=1)
  most_green <- which(greenNess > cutoff, arr.ind=TRUE)
  
  #return(most_green)
  for (i in 1:dim(most_green)[1]) {
    pixel <- most_green[i,]
    y[pixel[1], pixel[2], 1] <- runif(1)
    y[pixel[1], pixel[2], 2] <- runif(1)
    y[pixel[1], pixel[2], 3] <- runif(1)
    
  }
  return(y)
}

convert_mostYellow <- function(y, pixel_budget=0.01) {
  mean_R <- mean(y[,,1])
  mean_G <- mean(y[,,2])
  mean_B <- mean(y[,,3])
  # calculate "yellowness" of pictures
  yellowNess <- y[,,1] + y[,,2] - y[,,3]
  
  # store the indices of percentage of pixels indicated by the pixel budget that
  # are the most yellow
  cutoff <- quantile(yellowNess, 1 - pixel_budget, type=1)
  most_yellow <- which(yellowNess > cutoff, arr.ind=TRUE)
  
  #return(most_yellow)
  
  # change the color of the most yellow pixels
  for (i in 1:dim(most_yellow)[1]) {
    pixel <- most_yellow[i,]
    
    ################ Make Pixel Green-ish #############
    # y[pixel[1], pixel[2], 1] <- runif(1, 0, 0.1)
    # y[pixel[1], pixel[2], 2] <- runif(1, 0.3, 0.8)
    
    ############### Make pixel a random color #########
    # y[pixel[1], pixel[2], 1] <- runif(1)
    # y[pixel[1], pixel[2], 2] <- runif(1)
    # y[pixel[1], pixel[2], 3] <- runif(1)
    
    
    ########Make Pixel Average Color of Image #######
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


randomPixels <- function(x, pixel_budget = 0.01) {
  mean_R <- mean(x[,,1])
  mean_G <- mean(x[,,2])
  mean_B <- mean(x[,,3])
  
  numRows <- dim(x)[1]
  numCols <- dim(x)[2]
  change_pixels <- matrix(0, nrow=numRows * numCols * pixel_budget, ncol = 2)
  # change_pixels <- apply(change_pixels, 1, function(r){r[1] <- runif(1, min=1, max=numRows); r[2] <- runif(1, min=1, max=numCols)})
  change_pixels[,1] <- round(runif(dim(change_pixels)[1], min=1, max=numRows), 0)
  change_pixels[,2] <- round(runif(dim(change_pixels)[1], min=1, max=numCols), 0)
  
  # return(change_pixels)
  for (i in 1:dim(change_pixels)[1]) {
    pixel <- change_pixels[i,]
    
    ################ Make Pixel Green-ish #############
    # y[pixel[1], pixel[2], 1] <- runif(1, 0, 0.1)
    # y[pixel[1], pixel[2], 2] <- runif(1, 0.3, 0.8)
    
    ############### Make pixel a random color #########
    x[pixel[1], pixel[2], 1] <- runif(1)
    x[pixel[1], pixel[2], 2] <- runif(1)
    x[pixel[1], pixel[2], 3] <- runif(1)
    
    
    ########Make Pixel Average Color of Image #######
    # y[pixel[1], pixel[2], 1] <- mean_R
    # y[pixel[1], pixel[2], 2] <- mean_G
    # y[pixel[1], pixel[2], 3] <- mean_B
    
    ################ Make Pixels Yellow ###############
    # y[pixel[1], pixel[2], 1:2] <- runif(1, 0.78, 1)
    # 
    # y[pixel[1], pixel[2], 3] <- 0
  }
  return(x)
}

# identifies the most average color pixels out of the image
change_most_average <- function(y, pixel_budget=0.01) {
    mean_R <- mean(y[,,1])
    mean_G <- mean(y[,,2])
    mean_B <- mean(y[,,3])
    
    dist_from_avg <- abs(y[,,1] - mean_R) + abs(y[,,2] - mean_G) + abs(y[,,3] - mean_B)
    cutoff <- quantile(dist_from_avg, pixel_budget, type=1)
    most_avg <- which(dist_from_avg < cutoff, arr.ind = TRUE)
    for (i in 1:dim(most_avg)[1]) {
      pixel <- most_avg[i,]
      
      ################ Make Pixel Green-ish #############
      # y[pixel[1], pixel[2], 1] <- runif(1, 0, 0.1)
      # y[pixel[1], pixel[2], 2] <- runif(1, 0.3, 0.8)
      
      ############### Make pixel a random color #########
      y[pixel[1], pixel[2], 1] <- runif(1)
      y[pixel[1], pixel[2], 2] <- runif(1)
      y[pixel[1], pixel[2], 3] <- runif(1)
      
      
      ########Make Pixel Average Color of Image #######
      # y[pixel[1], pixel[2], 1] <- mean_R
      # y[pixel[1], pixel[2], 2] <- mean_G
      # y[pixel[1], pixel[2], 3] <- mean_B
      
      ################ Make Pixels Yellow ###############
      # y[pixel[1], pixel[2], 1:2] <- runif(1, 0.78, 1)
      # 
      # y[pixel[1], pixel[2], 3] <- 0
    }
    return(y)
}

# identifies the least average color pixels out of the image
change_least_average <- function(y, pixel_budget=0.01) {
  mean_R <- mean(y[,,1])
  mean_G <- mean(y[,,2])
  mean_B <- mean(y[,,3])
  
  dist_from_avg <- abs(y[,,1] - mean_R) + abs(y[,,2] - mean_G) + abs(y[,,3] - mean_B)
  cutoff <- quantile(dist_from_avg, 1 - pixel_budget, type=1)
  most_avg <- which(dist_from_avg > cutoff, arr.ind = TRUE)
  for (i in 1:dim(most_avg)[1]) {
    pixel <- most_avg[i,]
    
    ################ Make Pixel Green-ish #############
    # y[pixel[1], pixel[2], 1] <- runif(1, 0, 0.1)
    # y[pixel[1], pixel[2], 2] <- runif(1, 0.3, 0.8)
    
    ############### Make pixel a random color #########
    y[pixel[1], pixel[2], 1] <- runif(1)
    y[pixel[1], pixel[2], 2] <- runif(1)
    y[pixel[1], pixel[2], 3] <- runif(1)
    
    
    ########Make Pixel Average Color of Image #######
    # y[pixel[1], pixel[2], 1] <- mean_R
    # y[pixel[1], pixel[2], 2] <- mean_G
    # y[pixel[1], pixel[2], 3] <- mean_B
    
    ################ Make Pixels Yellow ###############
    # y[pixel[1], pixel[2], 1:2] <- runif(1, 0.78, 1)
    # 
    # y[pixel[1], pixel[2], 3] <- 0
  }
  return(y)
}