#' Data Sets
#'
#' Shows the datasets of a package
#'
#' Returns a character vector containing the names of the datasets in the
#' package given as the argument
#'
#' @param pkg the name of a package
#' @export
datasets <- function(pkg) {
  data(package = pkg)$results[, "Item"]
}
