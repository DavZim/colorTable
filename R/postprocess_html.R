#' postprocesses the html-code
#'
#' The goal is to exclude the span and shift the styles to td/th
#'
#' This makes sure that the whole box is colored instead of only the values
#'
#' @param x a structure containing the html code that can be coerced to
#' characters
#' @param width the width of the cells, defaults to 15px
#'
#' @return a knitr_kable object
#' @export
#' @keywords internal
#'
#' @examples
#' # internally used only
postprocess_html <- function(x, width = "15px") {
  x <- stringr::str_split(as.character(x), "\\n")[[1]]
  x <- sapply(x, function(x_) {
    ins <- stringr::str_extract(x_, "(?<=<span style=).*(?=\\\")")
    if (!is.na(ins)) {
      ins <- stringr::str_replace_all(ins, "[\"]", "")
      x_ <- stringr::str_replace(x_, "<span[^>]*>", "")
      x_ <- stringr::str_replace(x_, "</span[^>]*>", "")
      x_ <- stringr::str_replace(x_,
                                 "(?<=<t[dh] style=\\\")",
                                 paste0(ins, "width: ", width, ";"))
    }
    return(x_)
  }, USE.NAMES = F)

  x <- structure(matrix(x, 1), format = "html", class = 'knitr_kable')
  return(x)
}
