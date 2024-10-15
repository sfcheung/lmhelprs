library(lmhelprs)

set.seed(1234)
x <- c("City A", "City B", "City C", "City A", "City B", "City C")
w <- rnorm(length(x))
y <- rnorm(length(x))
lm_tmp1 <- lm(y ~ x)
lm_tmp2 <- lm(y ~ x + w)

library(lmhelprs)
tmp <- hierarchical_lm(lm_tmp1, lm_tmp2)
expect_equal(summary(lm_tmp2)$r.squared - summary(lm_tmp1)$r.squared,
             tmp[2, "R.sq.change"])
