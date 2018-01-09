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
  if (missing(toml)) toml <- grep("\\.toml$", dir(), value = TRUE)
  if (length(toml) < 1) stop("There is no TOML file in the working directory.")
  if (length(toml) > 1) stop(paste("There are more than one TOML file in the",
                                   "working directory. Choose one of them."))
  toml <- readtext(toml, verbosity = 0)
  toml <- strsplit(toml$text, "\n")[[1]]
  toml <- grep("publishDir", toml, value = TRUE)
  strsplit(toml, "\"")[[1]][2]
}
