#' Creates a color gradient for a given vector
#'
#' @param x a vector of numeric values
#' @param colors the colors for the gradient
#' @param x_range a vector of length 2 that specifies the range of x, defaults to min/max of x
#' @param colsteps the number of color-steps, defaults to 100
#' @param na_color the default color if a point is outside of the x_range, defaults to gray
#'
#' @return a vector of colors of the same length as x
#' @export
#'
#' @examples
#' x <- c(5, 1, 2, 5, 4, 3)
#' color_gradient(x, c("red", "green"))
#' color_gradient(x, c("red", "green"), x_range = c(0, 10))
#'
#' color_gradient(x, c("red", "black", "green"))
#'
#' x <- seq(-10, 10, length.out = 100)
#' y <- abs(x^2 - 20)
#' plot(x, y, col = color_gradient(y), pch = 19, cex = 2)
#' plot(x, y, col = color_gradient(y, c("yellow", "blue")), pch = 19, cex = 2)
#' plot(x, y, col = color_gradient(y, x_range = c(10, 20)), pch = 19, cex = 2)
#'
color_gradient <- function(x, colors = c("green", "yellow", "red"), x_range = NA, na_color = "gray", colsteps = 100) {
  cols <- colorRampPalette(colors)(colsteps)
  if (is.na(x_range[1])) {
    x_range <- c(
      min(x, na.rm = TRUE),
      max(x, na.rm = TRUE)
    )
  }
  col_x <- rep(na_color, length(x))
  in_range <- x <= max(x_range) & x >= min(x_range)
  col_x[in_range] <- cols[findInterval(x[in_range], seq(min(x_range), max(x_range), length.out = colsteps))]

  return(col_x)
}
