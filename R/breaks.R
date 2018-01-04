#' Breaks values
#'
#' Returns the breaks values of the output of the \code{base::cut} function
#'
#' @param x an output of the \code{base::cut} function.
#'
#' @return a numeric vector of break values.
#'
#' @export
#' @author Marc Choisy
#'
#' @examples
#' x <- rnorm(10000)
#' y <- cut(x, breaks = -6:6)
#' breaks(y)
#'
breaks <- function(x) {
  lev <- levels(x)
  lev <- gsub("\\(|\\]", "", lev)
  lev <- gsub(",", " ", lev)
  lev <- strsplit(paste(lev, collapse = " "), " ")
  sort(unique(as.numeric(unlist(lev)))) # unlist because strsplit returns a list
}
