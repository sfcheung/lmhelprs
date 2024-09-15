library(lmhelprs)

dat <- data_test1

m <- matrix(FALSE, nrow(dat), ncol(dat))
nstar <- length(m)
set.seed(874130)
i <- sample.int(nstar, round(nstar * .10))
m[i] <- TRUE
datm <- dat
datm[m] <- NA
row.names(datm) <- paste0("case_", seq_len(nrow(datm)))

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
outm4 <- many_lm(mod1, datm, subset = c(1, 3, 5, 7, 9, 11, 13, 15, 17))
s5 <- logical(nrow(datm))
s5[c(1, 3, 5, 7, 9, 11, 13, 15, 17)] <- TRUE
outm5 <- many_lm(mod1, datm, subset = s5)
outm6 <- many_lm(mod1, datm, subset = which(s5))
s7 <- paste0("case_", c(5, 3, 1, 7, 11, 9, 13, 15, 17))
outm7 <- many_lm(mod1, datm, subset = s7)

n1 <- sapply(outm1, function(x) nrow(model.frame(x)))
n2 <- sapply(outm2, function(x) nrow(model.frame(x)))
n2_chk <- sapply(list(lm1m, lm2m, lm3m),
                  function(x) nrow(model.frame(x)))

n3 <- sapply(outm3, nobs)
i1 <- datm$cat1 != "Alpha"
i1[is.na(i1)] <- FALSE
tmp <- model.frame(y ~ x1 + x2 + x3 + x4 + cat1, data = datm)
i2 <- attr(tmp, "na.action")
i3 <- !logical(nrow(datm))
i3[i2] <- FALSE
i4 <- i1 & i3
n3_chk <- nrow(datm[i4, ])

datm5 <- datm[s5, ]
tmp <- model.frame(y ~ x1 + x2 + x3 + x4 + cat1, data = datm5)
n5chk <- nrow(tmp)
n5 <- sapply(outm5, nobs)
n6 <- sapply(outm6, nobs)
n7 <- sapply(outm7, nobs)

nm <- nrow(na.omit(datm[, c("y", "x1", "x2", "x3", "x4", "cat1")]))

expect_equal(n1, rep(nm, 3))
expect_equal(n2, n2_chk)
expect_true(all(n3 == n3_chk))
expect_true(all(n5 == n5chk))
expect_true(all(n6 == n5chk))
expect_true(all(n7 == n5chk))

# No missing

out4 <- many_lm(mod1, dat)
expect_true(all(sapply(out4, nobs) == nrow(dat)))

out5 <- many_lm(mod1, dat, subset = cat1 != "Alpha")
expect_true(all(sapply(out5, nobs) == sum(dat$cat1 != "Alpha")))

# Summary

expect_stdout(print(summary(outm3)), pattern = "cases removed")
expect_false(any(grepl("cases remove",
                       capture.output(print(summary(out4))),
                       fixed = TRUE)))
