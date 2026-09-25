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

out_ci <- confint(out)
out_ci2 <- confint(out, level = .90)

chk <- confint(out[[1]])
chk2 <- confint(out[[2]], level = .90)

expect_equal(
  unname(out_ci[1:5, ]),
  unname(chk)
)
expect_equal(
  unname(out_ci2[-c(1:5), ]),
  unname(chk2)
)
