#' @title Print an `lm_list_lmhelprs`-Class
#' Object
#'
#' @description Print the content of the
#' output of [many_lm()].
#'
#' @details
#' Adapted from the package `manymome`
#' such that `many_lm()` can be used
#' with or without `manymome`.
#'
#' @return `x` is returned invisibly.
#' Called for its side effect.
#'
#' @param x The output of [many_lm()].
#'
#' @param ...  Other arguments. Not
#' used.
#'
#'
#'
#' @examples
#'
#' data(data_test1)
#' mod <- "x3 ~ x2 + x1
#'         x4 ~ x3
#'         x5 ~ x4*x1"
#' out <- many_lm(mod, data_test1)
#' out
#'
#' @export

print.lm_list_lmhelprs <- function(x, ...) {
    cat("\nThe models:\n")
    lapply(x, function(y) print(y$call$formula))
    cat("\n")
    invisible(x)
  }
