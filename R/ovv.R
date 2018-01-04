#' Overview of a data frame.
#'
#' \code{ovv} prints an overview of a data frame.
#'
#' Mostly used for in side effect printing of an overview of the data frame.
#'
#' @param x a data frame, matrix or table.
#' @param digits integer indicating the number of decimal places (round) or
#'               significant digits (signif) to be used (default value: 4). See
#'               the \code{base::round} function.
#' @param interspace integer indicating the number of dots to print between the
#'                   head and the tail of the object.
#' @return Invisibly return the input data frame.
#' @author Marc Choisy
#' @examples
#' # overviews of data.frames:
#' ovv(mtcars)
#' ovv(iris)
#'
#' # overviews of matrices:
#' ovv(as.matrix(mtcars))
#' ovv(as.matrix(iris))
#'
#' # overviews of tables:
#' library(magrittr) # for %>%
#' library(lubridate) # for ymd, isoweek, isoyear
#' # a fonction used to create the data set below:
#' fct <- function(y) {
#' g <- function(x, y) ymd(as.integer(paste0(y, x)))
#'   start_end <- lapply(c("0101", "1231"), g, y)
#'   seq(start_end[[1]], start_end[[2]], by = "day")
#' }
#' # creating a fake data set:
#' cal <- 10:17 %>%
#'   lapply(fct) %>%
#'   do.call(c, .) %>%
#'   (function(x) lapply(list(isoweek, isoyear), function(f) f(x))) %>%
#'   do.call(cbind, .) %>%
#'   as.data.frame %>%
#'   setNames(c("week", "year")) %>%
#'   unique
#' # an overview of its table:
#' ovv(table(cal))
#'
#' @export
#' @author Marc Choisy
ovv <- function(x, n = 6L, digits = 4L, interspace = 3L) {
  UseMethod("ovv")
}


# data.frame method ------------------------------------------------------------


#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr bind_rows mutate_all mutate_if
#' @importFrom stats setNames
#' @importFrom utils head tail
#' @method ovv data.frame
#' @export
ovv.data.frame <- function(x, n = 6L, digits = 4L, interspace = 3L) {
  if (nrow(x) > 2 * n + interspace) {
    h <- head(x, n)
    t <- tail(x, n)
    hn <- rownames(h)
    tn <- rownames(t)
    h %<>% mutate_if(is.numeric, round, digits = digits)
    t %<>% mutate_if(is.numeric, round, digits = digits)
    h %<>% mutate_all(as.character)
    t %<>% mutate_all(as.character)
    m <- setNames(as.data.frame(matrix(".", interspace, ncol(x)),
                                stringsAsFactors = FALSE), names(x))
    out <- bind_rows(h, m, t)
    out <- cbind(c(hn, rep(".", interspace), tn), out)
    names(out)[1] <- ""
    print(out, row.names = FALSE)
  } else print(x)
  invisible(x)
}


# matrix  method ---------------------------------------------------------------

#' @method ovv matrix
#' @export
ovv.matrix <- function(x, n = 6L, digits = 4L, interspace = 3L) {
  ovv(as.data.frame(x), n, digits, interspace)
}


# table method -----------------------------------------------------------------

#' @method ovv table
#' @export
ovv.table <- function(x, n = 6L, digits = 4L, interspace = 3L) {
  ovv(unclass(x), n, digits, interspace)
}
