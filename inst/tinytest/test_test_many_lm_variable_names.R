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

out_vnames <- variable.names(out)

out_chk <- lapply(
  out,
  variable.names
)
out_chk <- sort(unique(unlist(out_chk)))

expect_true(
  setequal(out_vnames, out_chk)
)