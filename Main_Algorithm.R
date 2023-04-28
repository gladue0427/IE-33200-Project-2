mod_image <- function(y, pixel_budget = 0.01, type = 0) {
  # y - 3 layer matrix representing the image with y[,,1] being Red,
  #     y[,,2] being Green, and y[,,3] being Blue pixels
  #     e.g. y[10, 20, 2] maps to the Green value of the pixel in the 10th row
  #     and 20th column
  # pixel_budget - if 0 < pixel_budget < 1, this represents the percentage of
  #     of the pixels in the image that will be changed if pixel_budget > 1, 
  #     it will be the absolute number of pixels to change
  # type - 0 if original image is grass, 1 if it is a dandelion
  
  
  # throws error if pixel budget is non-numeric or NA
  if(any(!is.numeric(pixel_budget), is.na(pixel_budget))) {
    stop("ERROR: Pixel budges was non-numeric or NA")
  }
  # throws error if pixel budget was <=0
  if (pixel_budget <= 0) {
    stop("ERROR: Pixel budget must be positive (greater than 0)")
  }  
  #throws error if pixel budget is higher than the total number of pixels in the image
  else if (pixel_budget > dim(y)[1] * dim(y)[2]) {
    stop("ERROR: Pixel budget was higher than number of pixels in image")
  }
  # if the pixel budget was given as a percentage (< 1), then calculate the
  # number of pixels to be changed
  else if (pixel_budget < 1) {
    pixel_budget <- floor(pixel_budget * dim(y)[1] * dim(y)[2])
  }
  
  set.seed(123)
  
  # mean values for each color of the image
  mean_R <- mean(y[,,1])
  mean_G <- mean(y[,,2])
  mean_B <- mean(y[,,3])
  
  # finds most green pixels, limited to pixel budget
  most_green <- find_most_green(y, pixel_budget = pixel_budget)
  # finds most yellow pixels
  most_yellow <- find_most_yellow(y, pixel_budget = pixel_budget)
  # chooses random pixels to change
  random_pixels <- find_random_pixels(y, pixel_budget = pixel_budget)
  # finds pixels closest to average color values of the whole image
  most_avg <- find_most_average(y, pixel_budget = pixel_budget)
  # finds pixels furthest away from the average color values of the whole image
  least_avg <- find_least_average(y, pixel_budget = pixel_budget)
  
  
  # if original image is grass
  if (type == 0) {
    # weights for different algorithms
    weights <- c(0.4, 0.2, 0.1, 0.2, 0.1)
    # number of pixels each algorithm will be changing
    weighted_num_pixels <- floor(weights * pixel_budget)
    
    # changes most green pixels
    for (i in sample(1:dim(most_green)[1], weighted_num_pixels[1])) {
      pixel <- most_green[i,]
      
      # make pixel a shade of yellow in the realistic range of a dandelion
      y[pixel[1], pixel[2], 1] <- runif(1, min=0.78, max=0.95)
      y[pixel[1], pixel[2], 2] <- runif(1, min=0.78, max=0.95)
      y[pixel[1], pixel[2], 3] <- runif(1, min=0.01, max=0.2)
      
    }
     
    # changes most yellow pixels
    for (i in sample(1:dim(most_yellow)[1], weighted_num_pixels[2])) {
      pixel <- most_yellow[i,]
      
      # make pixel a shade of yellow in the realistic range of a dandelion
      y[pixel[1], pixel[2], 1] <- runif(1, min=0.78, max=0.95)
      y[pixel[1], pixel[2], 2] <- runif(1, min=0.78, max=0.95)
      y[pixel[1], pixel[2], 3] <- runif(1, min=0.01, max=0.2)
    }
    
    # changes random pixels
    for (i in sample(1:dim(random_pixels)[1], weighted_num_pixels[3])) {
      pixel <- random_pixels[i,]
      
      # make pixel a shade of yellow in the realistic range of a dandelion
      y[pixel[1], pixel[2], 1] <- runif(1, min=0.78, max=0.95)
      y[pixel[1], pixel[2], 2] <- runif(1, min=0.78, max=0.95)
      y[pixel[1], pixel[2], 3] <- runif(1, min=0.01, max=0.2)
    }
    
    # changes most average pixels
    for (i in sample(1:dim(most_avg)[1], weighted_num_pixels[4])) {
      pixel <- most_avg[i,]
      
      # make pixel a shade of yellow in the realistic range of a dandelion
      y[pixel[1], pixel[2], 1] <- runif(1, min=0.78, max=0.95)
      y[pixel[1], pixel[2], 2] <- runif(1, min=0.78, max=0.95)
      y[pixel[1], pixel[2], 3] <- runif(1, min=0.01, max=0.2)
    }
    
    # changes least average pixels
    for (i in sample(1:dim(least_avg)[1], weighted_num_pixels[5])) {
      pixel <- least_avg[i,]
      
      # make pixel a shade of yellow in the realistic range of a dandelion
      y[pixel[1], pixel[2], 1] <- runif(1, min=0.78, max=0.95)
      y[pixel[1], pixel[2], 2] <- runif(1, min=0.78, max=0.95)
      y[pixel[1], pixel[2], 3] <- runif(1, min=0.01, max=0.2)
    }
  }
  
  # if original image is dandelion
  else if (type == 1) {
    
    # assigns weights to each algorithm's results
    weights <- c(0.05, 0.35, 0.3, 0.2, 0.1)
    
    # calculates the number of pixels to modify from the set of pixels
    # each algoirthm votes to change
    weighted_num_pixels <- floor(weights * pixel_budget)
    
    # changes most green pixels
    for (i in sample(1:dim(most_green)[1], weighted_num_pixels[1])) {
      pixel <- most_green[i,]
      
      # make pixel a random color
      y[pixel[1], pixel[2], 1] <- runif(1)
      y[pixel[1], pixel[2], 2] <- runif(1)
      y[pixel[1], pixel[2], 3] <- runif(1)
    
    }
    
    # changes most yellow pixels
    for (i in sample(1:dim(most_yellow)[1], weighted_num_pixels[2])) {
      pixel <- most_yellow[i,]
      
      ################ Make Pixel Green-ish #############
      # y[pixel[1], pixel[2], 1] <- runif(1, 0, 0.1)
      # y[pixel[1], pixel[2], 2] <- runif(1, 0.3, 0.8)
      
      #######Make Pixel Average Color of Image #######
      y[pixel[1], pixel[2], 1] <- mean_R
      y[pixel[1], pixel[2], 2] <- mean_G
      y[pixel[1], pixel[2], 3] <- mean_B
    }
    
    # changes random pixels
    for (i in sample(1:dim(random_pixels)[1], weighted_num_pixels[3])) {
      pixel <- random_pixels[i,]
      
      ############## Make pixel a random color #########
      y[pixel[1], pixel[2], 1] <- runif(1)
      y[pixel[1], pixel[2], 2] <- runif(1)
      y[pixel[1], pixel[2], 3] <- runif(1)
    }
    
    # changes most average pixels
    for (i in sample(1:dim(most_avg)[1], weighted_num_pixels[4])) {
      pixel <- most_avg[i,]
      
      ############## Make pixel a random color #########
      y[pixel[1], pixel[2], 1] <- runif(1)
      y[pixel[1], pixel[2], 2] <- runif(1)
      y[pixel[1], pixel[2], 3] <- runif(1)
    }
    
    # changes least average pixels
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