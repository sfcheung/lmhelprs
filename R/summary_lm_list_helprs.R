#' @title Summary of an `lm_list_lmhelprs`-Class
#' Object
#'
#' @description The summary of content
#' of the output of [many_lm()].
#'
#' @return [summary.lm_list_lmhelprs()] returns a
#' `summary_lm_list_lmhelprs`-class object, which
#' is a list of the [summary()] outputs
#' of the [lm()] outputs stored.
#'
#' [print.summary_lm_list_lmhelprs()] returns `x`
#' invisibly. Called for its side
#' effect.
#'
#' Adapted from the package `manymome`
#' such that `many_lm()` can be used
#' without `manymome`.
#'
#' @param object The output of
#' [many_lm()].
#'
#' @param x An object of class
#' `summary_lm_list_lmhelprs`.
#'
#' @param digits The number of
#' significant digits in printing
#' numerical results.
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
#' summary(out)
#'
#' @export

summary.lm_list_lmhelprs <- function(object, ...) {
    out <- lapply(object, summary)
    class(out) <- c("summary_lm_list_lmhelprs", "summary_lm_list", class(out))
    attr(out, "cases_removed") <- attr(object, "cases_removed")
    attr(out, "call") <- attr(object, "call")
    out
  }

#' @describeIn summary.lm_list_lmhelprs Print
#' method for output of summary for
#' lm_list_lmhelprs.
#'
#' @export

print.summary_lm_list_lmhelprs <- function(x, digits = 3, ...) {
    x_call <- attr(x, "call")
    cat("Call:\n")
    print(x_call)
    for (xi in x) {
        cat("\nModel:\n")
        print(xi$call$formula)
        stats::printCoefmat(xi$coefficients, digits = digits, ...)
        rsq0 <- formatC(xi$r.squared, digits = digits, format = "f")
        adjrsq0 <- formatC(xi$adj.r.squared, digits = digits, format = "f")
        f0 <- paste0("F(", round(xi$fstatistic["numdf"]),
                     ", ", xi$fstatistic["dendf"],
                     ") = ",
                     formatC(xi$fstatistic["value"], digits = digits, format = "f"))
        p1 <- stats::pf(xi$fstatistic["value"],
                        xi$fstatistic["numdf"],
                        xi$fstatistic["dendf"], lower.tail = FALSE)
        p0 <- ifelse(p1 < .001,
                     "p < .001",
                     paste0("p = ", formatC(p1, digits = digits, format = "f")))
        fstr <- paste0(f0, ", ", p0)
        tmp <- paste0("R-square = ", rsq0,
                      ". Adjusted R-square = ", adjrsq0,
                      ". ", fstr)
        cat(tmp)
        cat("\n")
      }
    cases_removed <- attr(x, "cases_removed")
    if (!is.null(cases_removed)) {
        n_removed <- length(cases_removed)
        if (n_removed > 0) {
            cat("\n")
            cat(n_removed, "cases removed (row numbers): \n")
            cat(strwrap(paste(cases_removed, collapse = ", ")),
                sep = "\n")
          }
      }
    invisible(x)
  }