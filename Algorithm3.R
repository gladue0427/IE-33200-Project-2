# changes random pixels in image
find_random_pixels <- function(y, pixel_budget = 500) {
  # if the pixel budget was given as a percentage (< 1), then calculate the
  # number of pixels to be changed
  if (pixel_budget < 1) {
    pixel_budget <- floor(pixel_budget * dim(y)[1] * dim(y)[2])
  } else if (pixel_budget <= 0) {
    stop("ERROR: Pixel budget was non-positive")
  }
  
  # initialize matrix where the indices of the pixels to be changed will be stored
  # # of rows = number of pixels to be changed
  # first element of row is the row index of the pixel to be changed
  # second element is the column index
  numRows <- dim(y)[1]
  numCols <- dim(y)[2]
  change_pixels <- matrix(0, nrow=pixel_budget, ncol = 2)
  
  # fills matrix with random values corresponding to 
  # the number of rows or columns as appropriate
  change_pixels[,1] <- round(runif(dim(change_pixels)[1], min=1, max=numRows), 0)
  change_pixels[,2] <- round(runif(dim(change_pixels)[1], min=1, max=numCols), 0)
  
  return(change_pixels)
  
  # for (i in 1:dim(change_pixels)[1]) {
  #   pixel <- change_pixels[i,]
  #   
  #   ############### Make pixel a random color #########
  #   y[pixel[1], pixel[2], 1] <- runif(3)
  #   y[pixel[1], pixel[2], 2] <- runif(1)
  #   y[pixel[1], pixel[2], 3] <- runif(1)
  # }
  # return(y)
}