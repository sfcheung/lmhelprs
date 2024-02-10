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

tmp <- hierarchical_lm(lm1b, lm1a, lm1c)
tmp_chk <- summary(lm1b)$r.squared - summary(lm1a)$r.squared
tmp_print <- capture.output(print(tmp, digits = 4, eps.Pvalue = .001))
expect_equal(tmp$R.sq.change[2], tmp_chk)
expect_equal(tmp$R.sq[3], summary(lm1c)$r.squared)
expect_equal(length(which(grepl("<0.001", tmp_print))), 1)

tmp <- hierarchical_lm(lm6b, lm6a)
tmp_chk <- summary(lm6b)$r.squared - summary(lm6a)$r.squared
expect_equal(tmp$R.sq.change[2], tmp_chk)
expect_equal(tmp$adj.R.sq[2], summary(lm6b)$adj.r.squared)

glm1a <- glm(y ~ x1 + x2, dat, family = gaussian)
glm1b <- glm(y ~ x1 + x2 + x3 + x4, dat, family = gaussian)
glm1c <- glm(y ~ x1 + x2 + x3 + x4 + cat1, dat, family = gaussian)
tmp <- hierarchical_lm(glm1a, glm1b, glm1c)
tmp_print <- capture.output(print(tmp, digits = 4, eps.Pvalue = .001))
expect_equal(length(which(grepl("Chi)", tmp_print))), 1)

expect_error(hierarchical_lm(lm5b, lm5d))
expect_error(hierarchical_lm(lm1a, lm11a))





