#' Write lines to a file
#'
#' Write text lines to a file.
#'
#' A wrap-up around the base::writeLines function.
#'
#' @param text A character vector.
#' @param file The path to a file.
#' @param sep character string. A string to be written to the connection after
#'            each line of text.
#' @param useBytes logical. See base::writeLines
#'
#' @author Marc Choisy
writelines <- function(text, file, sep = "\n", useBytes = FALSE) {
  fileConn <- file(file)
  writeLines(text, fileConn)
  close(fileConn)
}
