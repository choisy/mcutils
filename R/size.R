#' Report the Space Allocated for an Object
#'
#' Provides an estimate of the memory that is being used to store an R object.
#'
#' This is a wraper around the \code{\link[utils]{object.size}} function,
#' returning a nicer output.
#'
#' @param x an R object.
#'
#' @export
size <- function(x) {
  print(object.size(x), unit = "auto")
}
