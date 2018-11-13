#' Reverse rows or columns
#'
#' Reverses a matrix or a data frame by row or column
#'
#' @param x a matrix or a data frame.
#' @margin margin integer, 1 for rows, 2 for columns.
#'
#' @return an object of the same class as \code{x}.
#'
#' @export
#'
#' @examples
#' mtcars
#' reverse(mtcars, 1)
#' reverse(mtcars, 2)
#'
reverse <- function(x, margin) {
  if (margin > 1) return(x[, ncol(x):1])
  else return(x[nrow(x):1, ])
}
