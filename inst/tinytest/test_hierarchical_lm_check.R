if (FALSE) {
# WIP

library(lmhelprs)

dat <- data_test1

# Different numbers of cases

lm1a <- lm(y ~ x1 + x2, dat)
lm1b <- lm(y ~ x1 + x2 + x3 + x4, dat[-1, ])
lm1c <- lm(y ~ x1 + x2 + x3 + x4 + cat2, dat[-3, ])
lm2a <- lm(y ~ x2 + x3 + cat1, dat[-2, ])
lm2b <- lm(y ~ x2 + x3 + cat1 + cat2, dat)

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
        if (!chk) {
            return(FALSE)
          }
      }
    TRUE
  }

same_lm_n_i <- function(a, b) {
    n_a <- nrow(stats::model.frame(a))
    n_b <- nrow(stats::model.frame(b))
    if (n_a != n_b) {
        return(FALSE)
      }
    return(TRUE)
  }

expect_false(same_lm_n(lm1a, lm1b, lm1c))
expect_true(same_lm_n(lm1b, lm1c))
expect_true(same_lm_n(lm1b, lm1c, lm2a))

# Different means

dat1 <- dat
dat2 <- dat
dat3 <- dat
dat2$x2 <- dat2$x2 + 2
dat3$cat2[1:2] <- c("West", "East")

lm1a <- lm(y ~ x1 + x2, dat1)
lm1b <- lm(y ~ x1 + x2 + x3 + cat2, dat2)
lm1c <- lm(y ~ x1 + x2 + x3 + x4 + cat2, dat3)
lm2a <- lm(y ~ x2 + x3 + cat1, dat)
lm2b <- lm(y ~ x2 + x3 + cat1 + cat2, dat)

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
        if (!chk) {
            return(FALSE)
          }
      }
    TRUE
  }

same_lm_means_i <- function(a, b) {
    mm_a <- stats::model.frame(a)
    mm_b <- stats::model.frame(b)
    vars <- intersect(colnames(mm_a),
                      colnames(mm_b))
    vars_numeric <- vars[sapply(mm_a, is.numeric)]
    if (length(vars_numeric) > 0) {
        means_a <- colMeans(mm_a[, vars_numeric])
        means_b <- colMeans(mm_b[, vars_numeric])
        if (!isTRUE(all.equal(means_a, means_b))) {
          return(FALSE)
        }
      }
    vars_cat1 <- colnames(mm_a)[sapply(mm_a, is.character)]
    for (xx in vars_cat1) {
        mm_a[, xx] <- as.factor(mm_a[, xx])
      }
    vars_cat1 <- colnames(mm_b)[sapply(mm_b, is.character)]
    for (xx in vars_cat1) {
        mm_b[, xx] <- as.factor(mm_b[, xx])
      }
    vars_cat2 <- vars[sapply(mm_a, is.factor)]
    if (length(vars_cat2) > 0) {
        for (xx in vars_cat2) {
            freq_a <- table(mm_a[, xx])
            freq_b <- table(mm_b[, xx])
            if (!identical(freq_a, freq_b)) {
                return(FALSE)
              }
          }
      }
    TRUE
  }

expect_false(same_lm_means(lm1a, lm1b, lm1c))
expect_false(same_lm_means(lm1b, lm1c))
expect_true(same_lm_n(lm2a, lm2b))

# Different covariance matrix

dat1 <- dat
dat2 <- dat
dat3 <- dat
dat4 <- dat
dat5 <- dat
tmp2 <- mean(dat2$x2)
tmp3 <- mean(dat2$x3)
dat2$x2 <- (dat2$x2 - tmp2) * 2 + tmp2
dat2$x3 <- (dat2$x3 - tmp3) / 2 + tmp3
dat4$cat2[1:4] <- dat4$cat2[4:1]

lm1a <- lm(y ~ x1 + x2, dat1)
lm1b <- lm(y ~ x1 + x2 + x3 + cat2, dat2)
lm1c <- lm(y ~ x1 + x2 + x3 + x4 + cat2, dat3)
lm2a <- lm(y ~ x2 + cat1 + cat2, dat4)
lm2b <- lm(y ~ x2 + cat1 + cat2 + x3, dat5)

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
        if (!chk) {
            return(FALSE)
          }
      }
    TRUE
  }

same_lm_cov_i <- function(a, b) {
    mm_a <- stats::model.frame(a)
    mm_b <- stats::model.frame(b)
    vars <- intersect(colnames(mm_a),
                      colnames(mm_b))
    vars_numeric <- vars[sapply(mm_a, is.numeric)]
    if (length(vars_numeric) > 0) {
        cov_a <- cov(mm_a[, vars_numeric])
        cov_b <- cov(mm_b[, vars_numeric])
        if (!isTRUE(all.equal(cov_a, cov_b))) {
          return(FALSE)
        }
      }
    vars_cat1 <- colnames(mm_a)[sapply(mm_a, is.character)]
    for (xx in vars_cat1) {
        mm_a[, xx] <- as.factor(mm_a[, xx])
      }
    vars_cat1 <- colnames(mm_b)[sapply(mm_b, is.character)]
    for (xx in vars_cat1) {
        mm_b[, xx] <- as.factor(mm_b[, xx])
      }
    vars_cat2 <- vars[sapply(mm_a, is.factor)]
    if (length(vars_cat2) > 1) {
        xtabs_a <- stats::xtabs(data = mm_a[, vars_cat2])
        xtabs_b <- stats::xtabs(data = mm_b[, vars_cat2])
        if (!identical(xtabs_a, xtabs_b)) {
            return(FALSE)
          }
      }
    TRUE
  }

expect_false(same_lm_cov(lm1a, lm1b, lm1c))
expect_false(same_lm_cov(lm1b, lm1c))
expect_false(same_lm_cov(lm2a, lm2b))

}