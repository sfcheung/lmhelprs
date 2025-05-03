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
  # The last line


         "
modc1 <- ""
modc2 <- c("## ", "   ", "##  ")

lm1 <- lm(y ~ x1 + x2, dat)
lm2 <- lm(y ~ x2 + x1 + x3 + x4, dat)
lm3 <- lm(y ~ x2 + x1 + x3, dat)
out1 <- many_lm(mod1, dat)

tmp1 <- hierarchical_lm(lm2, lm1)

tmp2 <- hierarchical_lm(out1)

expect_equal(tmp1$R.sq.change[2], tmp2$R.sq.change[2])

tmp1b <- hierarchical_lm(lm2, lm1, lm3)

tmp2b <- hierarchical_lm(out1, lm3)

expect_equal(tmp1b$R.sq.change[2], tmp2b$R.sq.change[2])
expect_equal(tmp1b$R.sq.change[3], tmp2b$R.sq.change[3])


