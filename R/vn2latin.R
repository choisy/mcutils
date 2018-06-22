#' Conversion from Vietnamese characters to base Latin
#'
#' \code{vn2latin} returns the base Latin equivalent of Vietnamese Unicode
#' characters.
#'
#' @param x A vector of characters.
#' @return The translation of x in Basic Latin.
#' @examples
#' vn2latin("ẳ")
#' @export
#' @author Marc Choisy
vn2latin <- function(x) {
  for(i in 1:nrow(to_latin)) x <- gsub(to_latin[i, 1], to_latin[i, 2], x)
  x
}
