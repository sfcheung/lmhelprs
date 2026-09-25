library(lmhelprs)

dat <- data_test1

mod1 <- "

# Comments
    y ~ x1 + x2
        # Second models


         y ~ x2 + x1 + x3 + x4
x3 ~ cat1 + x1 + x2*x4
  # The last line


         "

mod2 <- "

         y ~ x2 + x1 + x3 + x4
x3 ~ cat1 + x1 + x2*x4
  # The last line


         "

out <- many_lm(
  mod1,
  dat
)

out2 <- update(
  out,
  data = dat[1:20, ]
)

out2chk <- many_lm(
  mod1,
  dat[1:20, ]
)

expect_equal(
  coef(out2),
  coef(out2chk)
)

out3 <- update(
  out,
  models = mod2
)

out3chk <- many_lm(
  mod2,
  dat
)

expect_equal(
  coef(out3),
  coef(out3chk)
)
