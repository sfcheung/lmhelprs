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
    y ~ x1 +
    x2
        # Second models


         y ~ x2 +
          x1 +
        x3 + x4
x3 ~
 cat1 + x1 +

x2*x4
  # The last line


         "
modc1 <- ""
modc2 <- c("## ", "   ", "##  ")

lm1 <- lm(y ~ x1 + x2, dat)
lm2 <- lm(y ~ x2 + x1 + x3 + x4, dat)
lm3 <- lm(x3 ~ cat1 + x1 + x2*x4, dat)
out1 <- many_lm(mod1, dat)

expect_equal(coef(out1[[1]]), coef(lm1))
expect_equal(coef(out1[[2]]), coef(lm2))
expect_equal(coef(out1[[3]]), coef(lm3))
