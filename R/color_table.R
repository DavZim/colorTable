#' Creates a html or latex colored table
#'
#' @param x a matrix or data.frame to be displayed
#' @param header_bg a single value or a vector of the same size as the number of
#'  columns of x, containing the background color(s) for the header
#' @param body_color a single value or a matrix/data.frame of the same size of
#' x with the text colors for the body of the table, if NA (default) takes the
#' same value as the background color
#' @param header_color a single value or a vector of the same size as the number
#'  of columns of x, containing the text-color(s) for the header
#' @param body_bg a single value or a matrix/data.frame of the same size of x
#' with the background colors for the body of the table
#' @param align The alignment of the columns
#' @param hide_body if the contents of the body of x are hidden
#' (replaced with "")
#' @param format a format for the code, is passed to \code{knitr::kable},
#' allowed is either html or latex
#' @param view T/F when the format is html, should it be displayed directly?
#' @param header_bold T/F if the header-text is bold
#' @param body_bold T/F if the body-text is bold
#' @param row_bgs a vector of colors for the rows (will be extended if
#' necessary)
#' @param col_bgs a vector of colors for the columns (will be extended if
#' necessary)
#' @param cell_width a minimum width of the columns (applies to html only),
#' defaults to 15px
#'
#' @return the latex/html code (invisible in case of html-output and view = T)
#' @export
#'
#' @examples
#' x <- data.frame(x = 1:3, y = 2:4, z = 3:5)
#' color_table(x, format = "html", view = T)
#'
#' # remove the text color of the body
#' color_table(x, body_color = NA, format = "html", view = T)
#' color_table(x, body_color = NA, header_color = NA, format = "html", view = T)
#'
#' # remove the text color of the header
#' color_table(x, body_color = NA, header_color = NA, format = "html", view = T)
#'
#' # change the colors:
#' color_table(x, body_color = "red", header_color = "white", format = "html",
#'             view = T)
#'
#' color_table(x, body_color = "red", header_color = c("red", "green", "blue"),
#'             format = "html", view = T)
#'
#' # color columns:
#' color_table(x, col_bgs = c("red", "green", "blue"), format = "html", view = T)
#'
#' # color rows:
#' color_table(x, row_bgs = c("red", "green", "blue"), format = "html", view = T)
#' # can be subsets:
#' large_x <- data.frame(x = 1:9, y = 1:9, z = 1:9)
#' color_table(large_x, row_bgs = c("blue", "green"), format = "html", view = T)
#'
#'
#' # color cells individually
#' colors1 <- matrix(c("red", "green", "blue", "yellow", "purple", "white",
#'                     "black", "gray", "yellowgreen"),
#'                   nrow = nrow(x), ncol = ncol(x), byrow = T)
#'
#' color_table(x, body_bg = colors1, format = "html", view = T)
#'
#' # special case: body_bg/color contains the header-information as well
#' colors2 <- matrix(c("red", "red", "red",
#'                     "green", "blue", "green",
#'                     "blue", "green", "blue",
#'                     "green", "blue", "green"),
#'                   ncol = 3, byrow = T)
#' color_table(x, body_bg = colors2, format = "html", view = T)
#'
#' # specify widths of the cell
#' x2 <- data.frame(x = 1:10, x2 = 2^(1:10))
#' color_table(x2, format = "html", view = T) -> a
#'
#' color_table(x2, cell_width = "75px", format = "html", view = T)
color_table <- function(x,
                        body_bg = "#a6d96a",
                        header_bg = "#74add1",
                        body_color = "black",
                        body_bold = FALSE,
                        header_color = "black",
                        header_bold = TRUE,
                        align = "c",
                        hide_body = FALSE,
                        row_bgs = NA,
                        col_bgs = NA,
                        view = FALSE,
                        cell_width = "15px",
                        format) {

  if (!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = F)
  if (hide_body) x[, ] <- "."

  if (missing(format)) {
    if (knitr::is_latex_output()) {
      format <- "latex"
    } else if (knitr::is_html_output()) {
      format <- "html"
    } else {
      format <- "html"
      # stop("format must be either html or latex (can be ommitted inside a pdf or html markdown document")
    }
  }

  format <- match.arg(format, c("latex", "html"))

  # check the row/column colors
  if (!all(is.na(row_bgs))) {
    if (length(row_bgs) < nrow(x)) {
      row_bgs <- rep(row_bgs, 1 + nrow(x) %/% length(row_bgs))[seq(nrow(x))]
    }
    body_bg <- matrix(row_bgs, nrow = nrow(x), ncol = ncol(x))
  }

  if (!all(is.na(col_bgs))) {
    if (length(col_bgs) < ncol(x)) {
      col_bgs <- rep(col_bgs, 1 + ncol(x) %/% length(col_bgs))[seq(ncol(x))]
    }
    body_bg <- matrix(col_bgs, nrow = nrow(x), ncol = ncol(x), byrow = T)
    header_bg <- body_bg[1,]
  }

  # check the colors
  no_header <- FALSE
  no_body <- FALSE

  # if the body-information contains the header information as well
  if (!is.null(dim(body_bg)) && all(dim(body_bg) == dim(x) + c(1, 0))) {
    header_bg <- body_bg[1, ]
    body_bg <- body_bg[-1, ]
  }
  if (!is.null(dim(body_color)) && all(dim(body_color) == dim(x) + c(1, 0))) {
    header_color <- body_color[1, ]
    body_color <- body_color[-1, ]
  }

  header_bg <- check_and_rep(header_bg, ncol(x), "white")
  if (all(is.na(header_color))) {
    no_header <- TRUE
    header_color <- header_bg
  } else {
    header_color <- check_and_rep(header_color, ncol(x), "black")
  }

  body_bg <- check_and_rep(body_bg, dim(x), "white")
  if (all(is.na(body_color))) {
    no_body <- TRUE
    body_color <- body_bg
  } else {
    body_color <- check_and_rep(body_color, dim(x), "black")
  }

  # body_bg/color is now the same size as x
  # header_bg/color has now the size of ncol(x)

  # replace color names with hex-values
  header_bg <- color2hex(header_bg)
  header_color <- color2hex(header_color)
  body_bg <- color2hex(body_bg)
  body_color <- color2hex(body_color)


  # check for valid color names
  ## Checks that a given element has a valid color
  test_color_name <- function(x, name_ = "x") {
    if (inherits(try(col2rgb(as.matrix(x)), silent = T), "try-error"))
      stop(name_, " must contain valid color names or hex-colors")
  }
  test_color_name(header_bg, "header_bg")
  test_color_name(header_color, "header_color")
  test_color_name(body_bg, "body_bg")
  test_color_name(body_color, "body_color")

  if (is.null(dim(body_bg)) || any(dim(x) != dim(body_bg)))
    stop("body_bg must be a single value or must have the same dimension as x")
  if (is.null(dim(body_color)) || any(dim(x) != dim(body_color)))
    stop("body_color must be a single value or must have the same dimension as x")

  # format the body
  for (row_ in seq(nrow(x))) {
    for (col_ in seq(ncol(x))) {
      # if the body text has the same color as the bg,
      # replace it with the header text to have the same width
      x[row_, col_] <- kableExtra::cell_spec(ifelse(no_body,
                                                    names(x)[col_],
                                                    x[row_, col_]),
                                             format = format,
                                             background = body_bg[row_, col_],
                                             color = body_color[row_, col_],
                                             bold = body_bold)
    }
  }

  # format the header
  for (col_ in seq(ncol(x))) {
    names(x)[col_] <- kableExtra::cell_spec(names(x)[col_], format = format,
                                            color = header_color[col_],
                                            background = header_bg[col_],
                                            bold = header_bold)
  }

  x_table <- knitr::kable(x, format, escape = FALSE, booktabs = TRUE,
                          linesep = "", align = align)


  if (format == "html") {

    # shift all html styles from span to td/th to make sure the cell has the width
    # and not the span itself (contains only the text)
    x_table <- postprocess_html(x_table, width = cell_width)

    if (view) {
      view_html(x_table)
      return(invisible(x_table))
    }
  }
  return(x_table)
}
