mod_image <- function(y, pixel_budget = 0.01, type = 1) {
  
  if(any(!is.numeric(pixel_budget), is.na(pixel_budget))) {
    stop("ERROR: Pixel budges was non-numeric")
  }
  
  if (pixel_budget <= 0) {
    stop("ERROR: Pixel budget was non-positive")
  } 
  # if the pixel budget was given as a percentage (< 1), then calculate the
  # number of pixels to be changed
  else if (pixel_budget < 1) {
    pixel_budget <- floor(pixel_budget * dim(y)[1] * dim(y)[2])
  } else if (pixel_budget > dim(y)[1] * dim(y)[2]) {
    stop("ERROR: Pixel budget was higher than number of pixels in image")
  }
  
  mean_R <- mean(y[,,1])
  mean_G <- mean(y[,,2])
  mean_B <- mean(y[,,3])
  
  most_green <- find_most_green(y, pixel_budget = pixel_budget)
  most_yellow <- find_most_yellow(y, pixel_budget = pixel_budget)
  random_pixels <- find_random_pixels(y, pixel_budget = pixel_budget)
  most_avg <- find_most_average(y, pixel_budget = pixel_budget)
  least_avg <- find_least_average(y, pixel_budget = pixel_budget)
  # if original image is grass
  if (type == 0) {
    weights <- c(0.1, 0.4, 0.05, 0.2, 0.2)
    weighted_num_pixels <- floor(weights * pixel_budget)
    
    
    for (i in sample(1:dim(most_green)[1], weighted_num_pixels[1])) {
      pixel <- most_green[i,]
      
      # make pixel a random color
      y[pixel[1], pixel[2], 1] <- runif(1)
      y[pixel[1], pixel[2], 2] <- runif(1)
      y[pixel[1], pixel[2], 3] <- runif(1)
      
    }
    
    for (i in sample(1:dim(most_yellow)[1], weighted_num_pixels[2])) {
      pixel <- most_yellow[i,]
      
      ################ Make Pixel Green-ish #############
      # y[pixel[1], pixel[2], 1] <- runif(1, 0, 0.1)
      # y[pixel[1], pixel[2], 2] <- runif(1, 0.3, 0.8)
      
      
      ########Make Pixel Average Color of Image #######
      y[pixel[1], pixel[2], 1] <- mean_R
      y[pixel[1], pixel[2], 2] <- mean_G
      y[pixel[1], pixel[2], 3] <- mean_B
    }
    #y <- change_most_average(y, pixel_budget = floor(pixel_budget/10) * 2)
    #y <- change_least_average(y, pixel_budget = floor(pixel_budget/10) * 2)
    
    
    #y <- convert_mostGreen(y, pixel_budget = floor(pixel_budget/10) * 2)
    #y <- convert_mostYellow(y, pixel_budget = floor(pixel_budget/10) * 2)
    #y <- randomPixels(y, pixel_budget = floor(pixel_budget/10) * 2)
    for (i in sample(1:dim(random_pixels)[1], weighted_num_pixels[3])) {
      pixel <- random_pixels[i,]
      
      ############### Make pixel a random color #########
      y[pixel[1], pixel[2], 1] <- runif(1)
      y[pixel[1], pixel[2], 2] <- runif(1)
      y[pixel[1], pixel[2], 3] <- runif(1)
    }
    for (i in sample(1:dim(most_avg)[1], weighted_num_pixels[4])) {
      pixel <- most_avg[i,]
      
      ############### Make pixel a random color #########
      y[pixel[1], pixel[2], 1] <- runif(1)
      y[pixel[1], pixel[2], 2] <- runif(1)
      y[pixel[1], pixel[2], 3] <- runif(1)
    }
    
    for (i in sample(1:dim(least_avg)[1], weighted_num_pixels[5])) {
      pixel <- least_avg[i,]
      
      ############### Make pixel a random color #########
      # y[pixel[1], pixel[2], 1] <- runif(1)
      # y[pixel[1], pixel[2], 2] <- runif(1)
      # y[pixel[1], pixel[2], 3] <- runif(1)
      
      ########Make Pixel Average Color of Image #######
      y[pixel[1], pixel[2], 1] <- mean_R
      y[pixel[1], pixel[2], 2] <- mean_G
      y[pixel[1], pixel[2], 3] <- mean_B
    }
  }
  
  # if original image is dandelion
  else if (type == 1) {
    weights <- c(0.1, 0.3, 0.2, 0.2, 0.2)
    weighted_num_pixels <- floor(weights * pixel_budget)
    
    
    for (i in sample(1:dim(most_green)[1], weighted_num_pixels[1])) {
      pixel <- most_green[i,]
      
      # make pixel a random color
      y[pixel[1], pixel[2], 1] <- runif(1)
      y[pixel[1], pixel[2], 2] <- runif(1)
      y[pixel[1], pixel[2], 3] <- runif(1)
      
    }
    
    for (i in sample(1:dim(most_yellow)[1], weighted_num_pixels[2])) {
      pixel <- most_yellow[i,]
      
      ################ Make Pixel Green-ish #############
      # y[pixel[1], pixel[2], 1] <- runif(1, 0, 0.1)
      # y[pixel[1], pixel[2], 2] <- runif(1, 0.3, 0.8)
      
      ########Make Pixel Average Color of Image #######
      y[pixel[1], pixel[2], 1] <- mean_R
      y[pixel[1], pixel[2], 2] <- mean_G
      y[pixel[1], pixel[2], 3] <- mean_B
    }
    
    for (i in sample(1:dim(random_pixels)[1], weighted_num_pixels[3])) {
      pixel <- random_pixels[i,]
      
      ############### Make pixel a random color #########
      y[pixel[1], pixel[2], 1] <- runif(1)
      y[pixel[1], pixel[2], 2] <- runif(1)
      y[pixel[1], pixel[2], 3] <- runif(1)
    }
    
    for (i in sample(1:dim(most_avg)[1], weighted_num_pixels[4])) {
      pixel <- most_avg[i,]
      
      ############### Make pixel a random color #########
      y[pixel[1], pixel[2], 1] <- runif(1)
      y[pixel[1], pixel[2], 2] <- runif(1)
      y[pixel[1], pixel[2], 3] <- runif(1)
    }
    
    for (i in sample(1:dim(least_avg)[1], weighted_num_pixels[5])) {
      pixel <- least_avg[i,]
      
      ########Make Pixel Average Color of Image #######
      y[pixel[1], pixel[2], 1] <- mean_R
      y[pixel[1], pixel[2], 2] <- mean_G
      y[pixel[1], pixel[2], 3] <- mean_B
    }
  }
  return(y)
}