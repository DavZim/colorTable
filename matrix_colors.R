#' Gathers a matrix where the first row contains the names
#'
#' By default, the var/val names are replaced the first element of the id-variables
#'
#' @param x a matrix, where the variable names are in the first row
#' @param id the index(indices) of the id
#' @param var_name the variable name
#' @param val_name the value name
#'
#' @return a long matrix
#' @export
#'
#' @examples
#' x <- matrix(c("Year", "Alice", "Bob", "Charlie",
#'               "2010", "105", "100", "97"), byrow = T, nrow = 2)
#' gather_colors(x, 1, "var", "val")
#'
#' x <- matrix(c("Year", "Alice", "Bob", "Charlie",
#'               "2010", "105", "100", "97",
#'               "2011", "110", "105", "85"), byrow = T, nrow = 3)
#' gather_colors(x, 1, "var", "val")
#'
#' x <- matrix(c("Year", "Region", "Alice", "Bob", "Charlie",
#'               "2010", "North", "105", "100", "97",
#'               "2010", "South", "151", "102", "75",
#'               "2011", "North", "110", "105", "85",
#'               "2011", "South", "112", "123", "13"), byrow = T, nrow = 5)
#' gather_colors(x, 1:2, "var", "val")
#'
#' colors <- matrix(c("#74add1", "#a6d96a", "#a6d96a", "#a6d96a",
#'                    "#fc9272", "#9e9ac8", "#9e9ac8", "#9e9ac8",
#'                    "#fc9272", "#9e9ac8", "#9e9ac8", "#9e9ac8"),
#'                  nrow = 3, byrow = T)
#' gather_colors(colors, 1)
gather_colors <- function(x, id, var_name = NA, val_name = NA) {
  if (is.na(var_name)) var_name <- x[id, 1][1]
  if (is.na(val_name)) val_name <- x[id, 1][1]

  x <- as.data.frame(matrix(x[-1, ],
                            ncol = ncol(x),
                            dimnames = list(NULL, x[1, ])),
                     stringsAsFactors = FALSE)

  vals <- reshape2::melt(x, id = names(x)[id])
  vals <- as.matrix(vals)
  vals <- rbind(c(names(x)[id], var_name, val_name), vals)
  colnames(vals) <- NULL

  return(vals)
}

#' Spreads a long matrix of colors to the wide format
#'
#' @param x a long matrix
#' @param id the group ids of the columns
#' @param var_id the variable ids
#' @param val_id the value ids
#'
#' @return a matrix im the wide format
#' @export
#'
#' @examples
#' x <- matrix(c("Year", "2010", "2010", "2010", "var", "Alice", "Bob",
#'               "Charlie", "val", "105", "100", "97"), ncol = 3)
#' spread_colors(x, 1, 2, 3)
#'
#' x <- matrix(c("Year", "2010", "2011", "2010", "2011", "2010", "2011",
#'               "var", "Alice", "Alice", "Bob", "Bob", "Charlie", "Charlie",
#'               "val", "105", "110", "100", "105", "97", "85"), ncol = 3)
#' spread_colors(x, 1, 2, 3)
#'
#' x <- matrix(c("Year", "2010", "2010", "2011", "2011", "2010", "2010",
#'               "2011", "2011", "2010", "2010", "2011", "2011", "Region", "North",
#'               "South", "North", "South", "North", "South", "North", "South",
#'               "North", "South", "North", "South", "var", "Alice", "Alice",
#'               "Alice", "Alice", "Bob", "Bob", "Bob", "Bob", "Charlie", "Charlie",
#'               "Charlie", "Charlie", "val", "105", "151", "110", "112", "100",
#'               "102", "105", "123", "97", "75", "85", "13"), ncol = 4)
#' spread_colors(x, 1:2, 3, 4)
#'
#' colors <- matrix(c("#74add1", "#fc9272", "#fc9272", "#74add1", "#a6d96a",
#'                    "#a6d96a", "#74add1", "#9e9ac8", "#9e9ac8"), ncol = 3)
#' spread_colors(colors, 1, 2, 3)
spread_colors <- function(x, id, var_id, val_id) {
  x <- as.data.frame(matrix(x[-1, ],
                            ncol = ncol(x),
                            dimnames = list(NULL, x[1, ])),
                     stringsAsFactors = FALSE)

  res <- lapply(split(x, f = x[, id]), function(x_) {
    vars_ <- x_[, var_id]
    vals_ <- matrix(c(x_[1, id], x_[, val_id]), ncol = length(vars_) + 1)
    colnames(vals_) <- c(names(x_)[id], vars_)
    return(vals_)
  })
  vals <- do.call(rbind, res)

  vals <- as.matrix(vals)
  vals <- rbind(colnames(vals), vals)
  colnames(vals) <- NULL
  return(vals)
}
