#' @title Check Number of Cases
#'
#' @description Check whether the numbers
#' of cases in two or more lm() outputs
#' are the same.
#'
#' @noRd

same_lm_n <- function(...) {
    lm_outs <- list(...)
    k <- length(lm_outs)
    lm_pairs <- utils::combn(k, 2, simplify = FALSE)
    for (lm_i in lm_pairs) {
        chk <- same_lm_n_i(lm_outs[[lm_i[1]]],
                           lm_outs[[lm_i[2]]])
        if (!isTRUE(chk)) {
            return(chk)
          }
      }
    TRUE
  }

#' @noRd

same_lm_n_i <- function(a, b) {
    n_a <- nrow(stats::model.frame(a))
    n_b <- nrow(stats::model.frame(b))
    if (n_a != n_b) {
        call1 <- deparse(stats::getCall(a))
        call2 <- deparse(stats::getCall(b))
        out <- paste(sQuote(call1),
                      "and",
                      sQuote(call2),
                      "do not have the same number of cases.")
        return(out)
      }
    return(TRUE)
  }

#' @title Check Variable Means
#'
#' @description Check whether the means
#' of common variables in two or more lm() outputs
#' are the same.
#'
#' @noRd

same_lm_means <- function(...) {
    lm_outs <- list(...)
    k <- length(lm_outs)
    lm_pairs <- utils::combn(k, 2, simplify = FALSE)
    for (lm_i in lm_pairs) {
        chk <- same_lm_means_i(lm_outs[[lm_i[1]]],
                               lm_outs[[lm_i[2]]])
        if (!isTRUE(chk)) {
            return(chk)
          }
      }
    TRUE
  }

#' @noRd

same_lm_means_i <- function(a, b) {
    mm_a <- stats::model.frame(a)
    mm_b <- stats::model.frame(b)
    vars <- intersect(colnames(mm_a),
                      colnames(mm_b))
    vars_numeric <- vars[sapply(mm_a[, vars, drop = FALSE], is.numeric)]
    if (length(vars_numeric) > 0) {
        means_a <- colMeans(mm_a[, vars_numeric, drop = FALSE])
        means_b <- colMeans(mm_b[, vars_numeric, drop = FALSE])
        if (!isTRUE(all.equal(means_a, means_b))) {
            call1 <- deparse(stats::getCall(a))
            call2 <- deparse(stats::getCall(b))
            out <- paste(sQuote(call1),
                         "and",
                         sQuote(call2),
                         "have different means",
                         "on at least one variable.",
                         "They used different datasets.")
            return(out)
        }
      }
    vars_cat1 <- colnames(mm_a)[sapply(mm_a[, vars, drop = FALSE], is.character)]
    for (xx in vars_cat1) {
        mm_a[, xx] <- as.factor(mm_a[, xx, drop = TRUE])
      }
    vars_cat1 <- colnames(mm_b)[sapply(mm_b, is.character)]
    for (xx in vars_cat1) {
        mm_b[, xx] <- as.factor(mm_b[, xx, drop = TRUE])
      }
    vars_cat2 <- vars[sapply(mm_a, is.factor)]
    if (length(vars_cat2) > 0) {
        for (xx in vars_cat2) {
            freq_a <- table(mm_a[, xx, drop = TRUE])
            freq_b <- table(mm_b[, xx, drop = TRUE])
            if (!identical(freq_a, freq_b)) {
                call1 <- deparse(stats::getCall(a))
                call2 <- deparse(stats::getCall(b))
                out <- paste(sQuote(call1),
                            "and",
                            sQuote(call2),
                            "have different frequencies",
                            "on at least one categorical variable.",
                            "They used different datasets.")
                return(out)
              }
          }
      }
    TRUE
  }

#' @title Check Variable Covariances
#'
#' @description Check whether the covariances
#' of common variables in two or more lm() outputs
#' are the same.
#'
#' @noRd

same_lm_cov <- function(...) {
    lm_outs <- list(...)
    k <- length(lm_outs)
    lm_pairs <- utils::combn(k, 2, simplify = FALSE)
    for (lm_i in lm_pairs) {
        chk <- same_lm_cov_i(lm_outs[[lm_i[1]]],
                             lm_outs[[lm_i[2]]])
        if (!isTRUE(chk)) {
            return(chk)
          }
      }
    TRUE
  }

#' @noRd

same_lm_cov_i <- function(a, b) {
    mm_a <- stats::model.frame(a)
    mm_b <- stats::model.frame(b)
    vars <- intersect(colnames(mm_a),
                      colnames(mm_b))
    vars_numeric <- vars[sapply(mm_a[, vars], is.numeric)]
    if (length(vars_numeric) > 0) {
        cov_a <- stats::cov(mm_a[, vars_numeric, drop = FALSE])
        cov_b <- stats::cov(mm_b[, vars_numeric, drop = FALSE])
        if (!isTRUE(all.equal(cov_a, cov_b))) {
            call1 <- deparse(stats::getCall(a))
            call2 <- deparse(stats::getCall(b))
            out <- paste(sQuote(call1),
                         "and",
                         sQuote(call2),
                         "have different variances and/or covariances",
                         "on at least one pair of variables.",
                         "They used different datasets.")
            return(out)
         }
      }
    vars_cat1 <- colnames(mm_a)[sapply(mm_a[, vars], is.character)]
    for (xx in vars_cat1) {
        mm_a[, xx] <- as.factor(mm_a[, xx])
      }
    vars_cat1 <- colnames(mm_b)[sapply(mm_b[, vars], is.character)]
    for (xx in vars_cat1) {
        mm_b[, xx] <- as.factor(mm_b[, xx])
      }
    vars_cat2 <- vars[sapply(mm_a, is.factor)]
    if (length(vars_cat2) > 1) {
        xtabs_a <- stats::xtabs(data = mm_a[, vars_cat2])
        xtabs_b <- stats::xtabs(data = mm_b[, vars_cat2])
        if (!isTRUE(all.equal(as.matrix(xtabs_a),
                              as.matrix(xtabs_b),
                              check.attributes = FALSE))) {
            call1 <- deparse(stats::getCall(a))
            call2 <- deparse(stats::getCall(b))
            out <- paste(sQuote(call1),
                         "and",
                         sQuote(call2),
                         "have different frequencies",
                         "on at least one combination of categorical variables.",
                         "They used different datasets.")
            return(out)
          }
      }
    TRUE
  }
