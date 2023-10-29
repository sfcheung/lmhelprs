#' @title Test the Highest Order Term by ANOVA
#'
#' @description Identify the highest
#' order terms in a model fitted by
#' 'lm()', and compare this model to a
#' model with this term removed using
#' ANOVA.
#'
#' @details The function [test_highest()]
#' first check if a model fitted by
#' [stats::lm()] has a unique highest
#' order term (e.g., the term `x1:x2`,
#' in the model `y ~ x1 + x2 + x1:x2`).
#' If yes, it will fit a model with this
#' term removed, and then call
#' [hierarchical_lm()] to compare the
#' original model with this reduced
#' model.
#'
#' If the model does not have a unique
#' highest order term, an error will
#' be raised.
#'
#' # Limitation
#'
#' It relies on terms created by
#' [stats::lm()] to determine the order
#' of each term. If a higher order term
#' is created manually (e.g.,
#' `I(x1 * x2)`), then it cannot know
#' that this term is a second order
#' term.
#'
#' @return A `hierarchical_lm`-class
#' object, which is the output of
#' [hierarchical_lm()]. Two models
#' are compared, the original model and
#' the model with the unique highest
#' order term removed.
#'
#' @param lm_out The output of
#' [stats::lm()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [hierarchical_lm()]
#'
#' @examples
#'
#' dat <- data_test1
#'
#' lm1 <- lm(y ~ x1 + x2 + cat1*x3, dat)
#' lm2 <- lm(y ~ x1 + x2*x3 + x4, dat)
#'
#' test_highest(lm1)
#' test_highest(lm2)
#'
#' highest_order(lm1)
#' highest_order(lm2)
#'
#' # The followings will yield an error
#'
#' lm3 <- lm(y ~ x1 + x2 + x3, dat)
#' summary(lm3)
#' tryCatch(test_highest(lm3), error = function(e) e)
#' tryCatch(highest_order(lm3), error = function(e) e)
#'
#' lm4 <- lm(y ~ x1 + x2*x3 + x4*x5, dat)
#' summary(lm4)
#' tryCatch(test_highest(lm4), error = function(e) e)
#' tryCatch(highest_order(lm4), error = function(e) e)
#'
#' @describeIn test_highest Test the highest order term.
#' @order 1
#' @export

test_highest <- function(lm_out) {
    lm_highest <- highest_order(lm_out)
    call2 <- stats::update(lm_out,
                           paste0("~ .-", lm_highest),
                           evaluate = FALSE)
    lm_out2 <- eval(call2,
                    envir = parent.frame())
    hierarchical_lm(lm_out, lm_out2)
  }

#' @describeIn test_highest Find the highest order term.
#' @order 2
#' @export

highest_order <- function(lm_out) {
    terms_x <- stats::terms(lm_out)
    labels_x <- labels(terms_x)
    order_x <- attr(terms_x, "order")
    order_max <- which.max(order_x)
    order_min <- which.min(order_x)
    max_n <- sum(order_x == max(order_x))
    if ((order_max == order_min) ||
        (max_n != 1)) {
        stop("No unique highest order term.")
      }
    labels_x[order_max]
  }

