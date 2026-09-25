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

chk <-
sort(c("y~1", "y~x2", "y~x1", "y~x3", "y~x4", "x3~1", "x3~cat1Beta",
"x3~cat1Gamma", "x3~x1", "x3~x2", "x3~x4", "x3~x2:x4"))

expect_equal(
  sort(names(coef(out))),
  chk
)

chk2 <-
sort(c("x3~1", "x3~cat1Beta", "x3~cat1Gamma", "x3~x1", "x3~x2", "x3~x4",
"x3~x2:x4"))

expect_equal(
  sort(names(coef(out, y = "x3"))),
  chk2
)

expect_error(
  coef(out, y = "x")
)
