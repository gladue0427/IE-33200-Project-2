reduceUpperExtremities <- function(x, pixel_budget) {
  cutoff <- quantile(x, 1-pixel_budget, type=1)
  print(cutoff)
  ext_removed <- apply(x, 1, function(r){r[r > cutoff] <- 0})
  return(ext_removed)
}

convert_mostGreen_to_yellow <- function(y, pixel_budget=0.01) {
  greenNess <- abs(y[,,2] - y[,,1])
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
  return(dim(most_green)[1])
}

randomPixels <- function(x, pixel_budget) {
  change_pixels <- sample(x, floor(x * pixel_budget))
}