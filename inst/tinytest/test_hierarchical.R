library(lmhelprs)

dat <- data_test1

lm1a <- lm(y ~ x1 + x2, dat)
lm1b <- lm(y ~ x1 + x2 + x3 + x4, dat)
lm1c <- lm(y ~ x1 + x2 + x3 + x4 + cat2, dat)
lm2a <- lm(y ~ x2 + x3 + cat1, dat)
lm2b <- lm(y ~ x2 + x3 + cat1 + cat2, dat)
lm3a <- lm(y ~ x1 + x2 + x3, dat)
lm3b <- lm(y ~ x1 + x2*x3, dat)
lm3c <- lm(y ~ x1 + x2 + x3 + I(x2*x3), dat)
lm4a <- lm(y ~ x1 + x2 + cat2, dat)
lm4b <- lm(y ~ x1 + x2*cat2, dat)
lm5a <- lm(y ~ x1 + cat2 + cat1, dat)
lm5b <- lm(y ~ x1 + cat2*cat1, dat)
lm5d <- lm(y ~ x1 + cat2 + cat1 + cat2:cat1, dat)
lm5e <- lm(y ~ x1 + cat2:cat1 + cat2 + cat1, dat)
lm6a <- lm(y ~ x1 + cat2*cat1, dat)
lm6b <- lm(y ~ x1 + cat2*cat1*x3, dat)
lm11a <- lm(x2 ~ x1 + cat2*cat1*x3, dat)

all_ps <- function(x) {
    out <- sapply(x,
                  function(x) length(labels(stats::terms(x))))
    out
  }

expect_true(hierarchical_i(lm1a, lm1b))
expect_false(hierarchical_i(lm1b, lm1a))
expect_true(is.na(hierarchical_i(lm1b, lm1b)))
expect_true(hierarchical_i(lm6a, lm6b))
expect_false(hierarchical_i(lm6b, lm6a))
expect_true(is.na(hierarchical_i(lm6a, lm6a)))
expect_true(is.na(hierarchical_i(lm1a, lm2a)))

tmp <- hierarchical(lm1b, lm1a, lm1c)
expect_equal(all_ps(tmp), c(2, 4, 5))
tmp <- hierarchical(lm6b, lm6a)
expect_equal(all_ps(tmp), c(4, 8))
tmp <- hierarchical(lm6b, lm1c, lm2b)
expect_equal(tmp, NA)
