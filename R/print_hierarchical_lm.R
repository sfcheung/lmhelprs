#' @title Print a `hierarchial_lm` Class Object
#'
#' @description Print the content of
#' a 'hierarchical_lm`-class object.
#'
#' @details The printout is very similar
#' to that of the print method of
#' an `anova` object. It simply
#' overrides the default values for
#' some arguments, notably `esp.Pvalue`
#' to prevent small *p*-values to be
#' presented in scientific notation.
#'
#' @return `x` is returned invisibly.
#' Called for its side effect.
#'
#' @param x A `hierarchical_lm`-class
#' object, usually the output of
#' `hierarchical_lm()`.
#'
#' @param digits The minimum number of
#' significant digits to be used for
#' most numbers. To be used by the print
#' method of `anova`-class objects.
#'
#' @param signif.stars Logical. To be
#' used by the print method of
#' `anova`-class objects.
#'
#' @param eps.Pvalue To be passed to
#' `format.pval()`. It controls how
#' small *p*-values are displayed.
#' Default is `.001`. That is,
#' *p*-values less than `.001` will be
#' displayed as `<.001`.
#'
#' @param ...  Optional arguments. To
#' be passed to the print method of
#' `anova`-class objects.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [hierarchical_lm()]
#'
#' @examples
#'
#' dat <- data_test1
#' lm1 <- lm(y ~ x1 + x2, dat)
#' lm2 <- lm(y ~ x1 + x2 + x3 + x4, dat)
#' lm3 <- lm(y ~ x1 + cat1 + cat2 + x2 + x3 + x4, dat)
#' lm4 <- lm(y ~ x1 + x2*x3 + x4, dat)
#'
#' hierarchical_lm(lm1, lm3, lm2)
#' hierarchical_lm(lm1, lm2, lm4)
#'
#' @export
print.hierarchical_lm <- function(x,
                                  digits = 4,
                                  signif.stars = getOption("show.signif.stars"),
                                  eps.Pvalue = .001,
                                  ...) {
    NextMethod(digits = digits,
               signif.stars = signif.stars,
               eps.Pvalue = eps.Pvalue)
    invisible(x)
  }