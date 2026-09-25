library(lmhelprs)

dat <- data_test1
dat[1:5, "x2"] <- NA
dat[3:6, "y"] <- NA

mod1 <- "

# Comments
         y ~ x2 + x1 + x3 + x4
x3 ~ cat1 + x1 + x2*x4
  # The last line


         "

out <- many_lm(
  mod1,
  dat
)

out_nobs <- nobs(out)

expect_equal(
  out_nobs,
  nobs(out[[1]])
)
