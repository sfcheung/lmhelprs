library(lmhelprs)

dat <- data_test1

m <- matrix(FALSE, nrow(dat), ncol(dat))
nstar <- length(m)
set.seed(874130)
i <- sample.int(nstar, round(nstar * .10))
m[i] <- TRUE
datm <- dat
datm[m] <- NA

mod1 <- "

# Comments
    y ~ x1 + x2
        # Second models


         y ~ x2 + x1 + x3 + x4
x3 ~ cat1 + x1 + x2*x4
  # The last line


         "
modc1 <- ""
modc2 <- c("## ", "   ", "##  ")

lm1 <- lm(y ~ x1 + x2, dat)
lm2 <- lm(y ~ x2 + x1 + x3 + x4, dat)
lm3 <- lm(x3 ~ cat1 + x1 + x2*x4, dat)
out1 <- many_lm(mod1, dat)

expect_error(parse_models(modc1))
expect_error(parse_models(modc2))

expect_error(many_lm(modc1))
expect_error(many_lm(modc2))
expect_error(many_lm(mod1))

expect_equal(coef(out1[[1]]), coef(lm1))
expect_equal(coef(out1[[2]]), coef(lm2))
expect_equal(coef(out1[[3]]), coef(lm3))

lm1m <- lm(y ~ x1 + x2, datm)
lm2m <- lm(y ~ x2 + x1 + x3 + x4, datm)
lm3m <- lm(x3 ~ cat1 + x1 + x2*x4, datm)
outm1 <- many_lm(mod1, datm)
outm2 <- many_lm(mod1, datm, na_omit_all = FALSE)
outm3 <- many_lm(mod1, datm, subset = cat1 != "Alpha")

n1 <- sapply(outm1, function(x) nrow(model.frame(x)))
n2 <- sapply(outm2, function(x) nrow(model.frame(x)))
n2_chk <- sapply(list(lm1m, lm2m, lm3m),
                  function(x) nrow(model.frame(x)))

nm <- nrow(na.omit(datm[, c("y", "x1", "x2", "x3", "x4", "cat1")]))

expect_equal(n1, rep(nm, 3))
expect_equal(n2, n2_chk)
