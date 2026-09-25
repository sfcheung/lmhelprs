library(lmhelprs)

dat <- data_test1

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

out_vcov <- vcov(out)

expect_equal(
  unname(vcov(out[[1]])),
  unname(out_vcov[1:5, 1:5])
)
expect_equal(
  unname(vcov(out[[2]])),
  unname(out_vcov[-c(1:5), -c(1:5)])
)
