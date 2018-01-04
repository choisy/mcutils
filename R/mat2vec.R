#' Converts matrix to vector
#'
#' \code{mat2vec} converts a matrix to a NA-separated vector where the NA
#' values separate either rows or columns.
#'
#' @param mat the matrix to convert to a vector
#' @param margin indicates whether NA values separate the rows (margin = 1) or
#'               the columns (margin = 2). Default value is 2
#' @export
#'
mat2vec <- function(mat, margin = 2) {
  if(margin < 2) mat <- t(mat)
  vec <- as.vector(rbind(mat, NA))
  vec[-length(vec)]
}
