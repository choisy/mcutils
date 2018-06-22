#' Fix names of provinces
#'
#' \code{fix_names} allows to correct the names of the provinces, adopting a
#' standard convention.
#'
#' Ease the integration of data from multiple sources.
#'
#' @param x a vector of characters.
#' @return A vector of characters, the corrected version of the x input.
#' @author Marc Choisy
#' @examples
#' fix_names(c("Soc T rang", "SOn ia", "Son ia"))
fix_names <- function(x) {
  dictionary <- dictionary[dictionary$from %in% x, ]
  attach(dictionary)
  for(i in 1:nrow(dictionary)) x <- sub(from[i], to[i], x)
  x
}
