#' Create the full sequence of values in a vector
#'
#' This function is a wrapper around the \code{\link[tidyr]{full_seq}} function
#' and automatically calculates the smallest period is missing.
#' @param x A numeric vector.
#' @param period Gap between each observation. The existing data will be checked
#'               to ensure that it is actually of this periodicity. Automatically
#'               calculated if missing.
#' @param tol Numerical tolerance for checking periodicity.
#' @export
#' @seealso \code{\link[tidyr]{full_seq}}
#' @author Marc Choisy
#' @importFrom tidyr full_seq
full_seq2 <- function(x, period, tol = 1e-6) {
  if(missing(period)) period <- diff(unique(x))[1]
  full_seq(x, period, tol)
}
