#' Views HTML-code in a browser
#'
#' @param x a string of html-code
#'
#' @return NULL
#' @export
#'
#' @examples
#' view_html("<html><body>Hello there!</body></html>")
view_html <- function(x){
  x <- paste(x, collapse = " ")
  tf <- tempfile(fileext = ".html")
  writeLines(x, tf)
  if ("rstudioapi" %in% rownames(installed.packages()) &&
      rstudioapi::isAvailable()) {
    rstudioapi::viewer(tf)
  } else {
    utils::browseURL(tf)
  }
  return(invisible(NULL))
}
