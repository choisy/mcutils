print_units <- function(x) {
  un <- units(x)
  numerator <- un$numerator
  denominator <- un$denominator
  if (length(numerator) > 0) {
    if (length(denominator) > 0) un <- paste0(numerator, "/", denominator)
    else un <- numerator
  } else {
    if (length(denominator) > 0) un <- paste0("/", denominator)
    else un <- ""
  }
  paste0(x, " [", un, "]")
}
