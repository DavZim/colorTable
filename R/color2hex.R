#' Replaces color names with hex-color names
#'
#' @param x a vector, matrix, or data.frame containing color names
#'
#' @return hex-color-names in the same structur as x
#' @export
#'
#' @examples
#' color2hex("red")
#' color2hex("#ff0000")
#'
#' color2hex(c("red", "green"))
#'
#' color2hex(matrix(c("red", "green", "blue", "yellow"), 2, 2))
#'
#' color2hex(data.frame(x = c("red", "green"),
#'                      y = c("blue", "yellow")))
color2hex <- function(x) {
  if (is.matrix(x)) {
    x <- matrix(apply(x, 2, function(i) rgb(t(col2rgb(i) / 255))), ncol = ncol(x), nrow = nrow(x))
  } else if (is.data.frame(x)) {
    x <- as.data.frame(apply(x, 2, function(i) rgb(t(col2rgb(i) / 255))),
                       stringsAsFactors = F)
  } else {
    # vector
    x <- rgb(t(col2rgb(x) / 255))
  }
  return(x)
}
