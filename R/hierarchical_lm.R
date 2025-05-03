#' @title Hierarchical Regression Analysis
#'
#' @description Do hierarchical
#' regression analysis on two or more
#' models fitted by 'lm()'.
#'
#' @details It conducted hierarchical
#' regression analysis on two or more
#' models fitted by [stats::lm()].
#' The models must be able to be ordered
#' from the simplest to the most complex,
#' with each more complex model formed
#' by adding one or more terms to the
#' simpler model.
#'
#' ANOVA will be conducted to compare
#' each model with the next more
#' complex model in the order, with
#' R-squared change computed.
#'
#' @return
#' If the models can be ordered in a
#' hierarchical way, the output is an
#' ANOVA table with the R-squared
#' estimate of each model, and the
#' R-squared change of each model
#' compared to the simpler model
#' preceding this model in the order.
#' The class of the output is
#' `hierarchical_lm`, with a print
#' method. If the models cannot be
#' ordered this way, `NA` is returned.
#'
#' ## How it works
#'
#' It call [hierarchical()] firsts to
#' order the outputs for [stats::lm()],
#' If they can be ordered in a
#' hierarchical way, they will be passed
#' to [stats::anova()]. R-squared and
#' R-squared change will be computed
#' if they are available in the
#' [summary()] method applied to each
#' model.
#'
#' Therefore, in principle, this
#' function can also be used for the
#' outputs of other model fitting
#' functions if their outputs have
#' [stats::anova()] and [summary()]
#' methods.
#'
#' ## Check Datasets Used
#'
#' The comparison is meaningful only
#' if all models are fitted to the
#' same datasets. There is not way
#' to guarantee this is the case, given
#' only the output of [lm()]. However,
#' there are necessary conditions to
#' claim that the same datasets are used:
#' the number of cases are the same,
#' the means, variances, and covariances
#' of numerical variables, and the
#' frequency distributions of variables
#' common to two models are identical.
#' If at least one of these conditions
#' is not met, then two models must have
#' been fitted to two different datasets.
#'
#' The function will check these
#' conditions and raise an error if
#' any of these necessary conditions
#' are not met.
#'
#' @param ... The outputs of `lm()`,
#' that is, one or more `lm`-class
#' objects. The outputs of other model
#' fitting functions may also be used,
#' but should be used with cautions.
#' Please refer to the "How it works"
#' section in "Details." It also supports
#' the output of [many_lm()], and can mix
#' the outputs of [many_lm()] with those
#' of [lm()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [stats::lm()], [hierarchical()]
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
#' # The following models will yield an error message:
#' tryCatch(hierarchical_lm(lm1, lm3, lm2, lm4), error = function(e) e)
#'
#' @export
#'
hierarchical_lm <- function(...) {
    lm_outs <- list(...)
    # Handle lm_list object
    lm_outs <- lapply(lm_outs, function(xx) {
                      if (inherits(xx, "lm_list")) {
                        # unlist() does not work
                        tmp <- lapply(xx,
                                      function(x) x)
                        return(tmp)
                      } else {
                        return(list(xx))
                      }
                    })
    lm_outs <- unlist(lm_outs, recursive = FALSE)
    check_y <- do.call(same_response, lm_outs)
    if (!check_y) {
        stop("The models do not have the same outcome variable.")
      }
    tmp <- do.call(same_lm_n,
                   lm_outs)
    if (!isTRUE(tmp)) {
        stop(tmp)
      }
    tmp <- do.call(same_lm_means,
                   lm_outs)
    if (!isTRUE(tmp)) {
        stop(tmp)
      }
    tmp <- do.call(same_lm_cov,
                   lm_outs)
    if (!isTRUE(tmp)) {
        stop(tmp)
      }
    lm_sorted <- do.call(hierarchical, lm_outs)
    if (isTRUE(is.na(lm_sorted))) {
        stop("The models do not have hierarchical relations.")
      }
    is_glm <- all(sapply(lm_outs,
                         inherits,
                         what = "glm"))
    if (is_glm) {
        out <- do.call(stats::anova,
                       c(lm_sorted,
                         list(test = "Chisq")))
      } else {
        out <- do.call(stats::anova,
                       lm_sorted)
      }
    summary_all <- lapply(lm_sorted,
                          summary)
    rsq_all <- sapply(summary_all,
                      function(x) x$r.squared)
    adj_rsq_all <- sapply(summary_all,
                      function(x) x$adj.r.squared)
    has_rsq_all <- all(!sapply(rsq_all, is.null))
    has_adj_rsq_all <- all(!sapply(adj_rsq_all, is.null))
    out1 <- out
    if (has_rsq_all) {
        rsq_cha <- c(0, diff(rsq_all))
        out1 <- cbind(R.sq = rsq_all,
                      R.sq.change = rsq_cha,
                      out)
      }
    if (has_adj_rsq_all) {
        out1 <- cbind(adj.R.sq = adj_rsq_all,
                      out1)
      }
    class(out1) <- class(out)
    attr(out1, "heading") <- attr(out, "heading")
    class(out1) <- c("hierarchical_lm", class(out1))
    out1
  }
