#' @title Check Models Hierarchy
#'
#' @description Check a list of 'lm'
#' objects to see whether they are be
#' ordered in a way for doing
#' hierarchical regression analysis.
#'
#' @details Two models can be compared
#' by hierarchical regression analysis
#' if one model can be formed by adding
#' one or more terms to the other model.
#'
#' This function checks whether a list
#' of `lm` outputs can be ordered from
#' the simplest model to the most
#' complex model, with a more complex
#' model formed by adding one or more
#' terms to a simpler model.
#'
#' ## How it works
#'
#' It extracts the terms in each model
#' by [stats::terms()] and then extracts
#' the labels of the terms by
#' [labels()]. The labels are then used
#' to determine the hierarchical order.
#'
#' Therefore, in principle, this
#' function can be used for the outputs
#' of other model fitting functions as
#' long as their outputs support the
#' [stats::terms()] and the labels can
#' be used to determine hierarchical
#' order of two models.
#'
#' @return
#' If the models can be ordered in a
#' hierarchical way, the output is a
#' list of the original `lm`
#' outputs, sorted from the model with
#' the smallest number of terms to the
#' model with the largest number of
#' terms. If the models cannot be
#' ordered this way, `NA` is returned.
#'
#' @param ... The outputs of `lm()`,
#' that is, one or more `lm`-class
#' objects. The outputs of other model
#' fitting functions may also be used,
#' but should be used with cautions.
#' Please refer to the "How it works"
#' section in "Details."
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [stats::lm()]
#'
#' @examples
#'
#' dat <- data_test1
#' lm1 <- lm(y ~ x1 + x2, dat)
#' lm2 <- lm(y ~ x1 + x2 + x3 + x4, dat)
#' lm3 <- lm(y ~ x1 + cat1 + cat2 + x2 + x3 + x4, dat)
#' lm4 <- lm(y ~ x1 + x2*x3 + x4, dat)
#'
#' # The order of entry does not matter
#' hierarchical(lm1, lm4, lm2)
#'
#' # The following three models yield NA
#' hierarchical(lm3, lm4, lm2)
#'
#' @export
#'
hierarchical <- function(...) {
    lm_outs <- list(...)
    if (length(lm_outs) == 1) {
        return(lm_outs[[1]])
      }
    p_all <- sapply(lm_outs,
                    function(x) {
                        length(labels(stats::terms(x)))
                      })
    lm_order <- order(p_all)
    lm_sorted <- lm_outs[lm_order]
    for (i in seq_len(length(lm_sorted) - 1)) {
        if (!isTRUE(hierarchical_i(lm_sorted[[i]],
                             lm_sorted[[i + 1]]))) {
            return(NA)
          }
      }
    return(lm_sorted)
  }

#' @noRd

hierarchical_i <- function(a, b) {
    termsa <- stats::terms(a)
    termsb <- stats::terms(b)
    labelsa <- labels(termsa)
    labelsb <- labels(termsb)
    b_wi_a <- setequal(union(labelsa, labelsb), labelsa)
    a_wi_b <- setequal(union(labelsa, labelsb), labelsb)
    if (b_wi_a && a_wi_b) {
        return(NA)
      }
    if (b_wi_a) {
        return(FALSE)
      }
    if (a_wi_b) {
        return(TRUE)
      }
    NA
  }
