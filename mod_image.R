mod_image <- function(y, pixel_budget = 0.01, type=1) {
  y <- change_most_average(y, pixel_budget = pixel_budget/5)
  y <- convert_mostGreen(y, pixel_budget = pixel_budget/5)
  y <- convert_mostYellow(y, pixel_budget = pixel_budget/5)
  y <- randomPixels(y, pixel_budget = pixel_budget/5)
  y <- change_least_average(y, pixel_budget = pixel_budget/5)
  return(y)
}