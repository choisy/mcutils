#' Retrieve publishDir from a TOML file
#'
#' @param toml character string naming the TOML file to parse.
#'
#' @return Returns the path of the publishDir in the input TOML file.
#'
#' @author Marc Choisy
#'
#' @importFrom readtext readtext
#'
#' @export
publishdir <- function(toml) {
  toml <- readtext(toml, verbosity = 0)
  toml <- strsplit(toml$text, "\n")[[1]]
  toml <- grep("publishDir", toml, value = TRUE)
  strsplit(toml, "\n")[[1]][2]
}
