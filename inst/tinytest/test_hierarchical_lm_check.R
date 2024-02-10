library(lmhelprs)

dat <- data_test1

# Different numbers of cases

lm1a <- lm(y ~ x1 + x2, dat)
lm1b <- lm(y ~ x1 + x2 + x3 + x4, dat[-1, ])
lm1c <- lm(y ~ x1 + x2 + x3 + x4 + cat2, dat[-3, ])
lm2a <- lm(y ~ x2 + x3 + cat1, dat[-2, ])
lm2b <- lm(y ~ x2 + x3 + cat1 + cat2, dat)

expect_false(isTRUE(lmhelprs:::same_lm_n(lm1a, lm1b, lm1c)))
expect_true(is.character(lmhelprs:::same_lm_n(lm1a, lm1b, lm1c)))
expect_true(lmhelprs:::same_lm_n(lm1b, lm1c))
expect_true(lmhelprs:::same_lm_n(lm1b, lm1c, lm2a))

expect_error(hierarchical_lm(lm1a, lm1b, lm1c), "number of cases")
expect_error(hierarchical_lm(lm1b, lm1c), "means")
expect_error(hierarchical_lm(lm1b, lm1c, lm2a), "means")

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

expect_false(isTRUE(lmhelprs:::same_lm_means(lm1a, lm1b, lm1c)))
expect_false(isTRUE(lmhelprs:::same_lm_means(lm1b, lm1c)))
expect_true(is.character(lmhelprs:::same_lm_means(lm1a, lm1b, lm1c)))
expect_true(is.character(lmhelprs:::same_lm_means(lm1b, lm1c)))
expect_true(lmhelprs:::same_lm_n(lm2a, lm2b))

expect_error(hierarchical_lm(lm1a, lm1b, lm1c), "means")
expect_error(hierarchical_lm(lm1b, lm1c), "means")

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
lm3a <- lm(y ~ x2 + cat1 + cat2, dat)
lm3b <- lm(y ~ x2 + cat1 + cat2 + x3, dat)

expect_false(isTRUE(lmhelprs:::same_lm_cov(lm1a, lm1b, lm1c)))
expect_false(isTRUE(lmhelprs:::same_lm_cov(lm1b, lm1c)))
expect_false(isTRUE(lmhelprs:::same_lm_cov(lm2a, lm2b)))
expect_true(isTRUE(lmhelprs:::same_lm_cov(lm3a, lm3b)))

expect_error(hierarchical_lm(lm1a, lm1b, lm1c), "covariances")
expect_error(hierarchical_lm(lm1b, lm1c), "covariances")
expect_error(hierarchical_lm(lm2a, lm2b), "frequencies")
