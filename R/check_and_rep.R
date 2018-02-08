#' Checks that a vector has a given lengths, if x is null or NA, the default is repeated a given amount
#'
#' @param x a vector, a matrix, or a data.frame
#' @param req_size a required size of x (a single value for a vector or a vector of 2 values)
#' @param default default values if x is NULL/NA
#' @param name a name of x (for the error message only)
#'
#' @return a vector of size req_size or a matrix (if req_size has length 2)
#' @export
#' @keywords internal
#'
#' @examples
#'
#' # vector
#' check_and_rep(NA, 3, "red")
#' check_and_rep("green", 3, "red")
#' \dontrun{
#'     # produces an error
#'     check_and_rep(c("red", "green"), 3, "red")
#'     # change the name of x
#'     check_and_rep(c("red", "green"), 3, "red", "fancy_vector")
#' }
#' check_and_rep(c("red", "green", "blue"), 3, "red")
#'
#' # Matrix
#' check_and_rep("green", c(2, 3), "white")
#'
#' x <- matrix(c("red", "green", "blue", "yellow"), 2, 2)
#'
#' check_and_rep(x, c(2, 2), "white")
check_and_rep <- function(x, req_size, default = "white", name = "x") {

  # check if x is supposed to be a vector
  if (length(req_size) == 1) {
    if (length(x) <= 1) {
      x <- rep(ifelse(is.null(x) || is.na(x),
                      default, x), req_size)
    } else {
      if (length(x) != req_size)
        stop(name,
             " must be either NA/NULL, a single value, or a vector of length ",
             req_size)
    }
    return(x)
  } else if (length(req_size) == 2) { # check if x is supposed to be 2 dimensional
    if (length(x) <= 1) {
      x_ <- rep(ifelse(is.null(x) || is.na(x),
                       default, x), prod(req_size))

      x_ <- matrix(x_, nrow = req_size[1], ncol = req_size[2])
      if (is.data.frame(x)) {
        x <- as.data.frame(x, stringsAsFactors = F)
      } else {
        x <- x_
      }
    } else {
      if (any(dim(x) != req_size))
        stop(name,
             " must be either NA/NULL, a single value, or a matrix/data.frame of dim ",
             paste(req_size, collapse = " "))
    } # else, keep x as it is!
  } else {
    stop("req_size must be a single value or a vector of length 2")
  }

  return(x)
}
