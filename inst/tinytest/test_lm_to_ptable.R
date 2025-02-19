# WIP

library(lmhelprs)

dat <- data_test1

mod1 <-
"
x2 ~ x1
x3 ~ x2 + x1
"

out1 <- many_lm(mod1, dat)

expect_true(is.data.frame(lm_list_to_partable(out1)))
expect_true(is.data.frame(lm_list_to_partable(out1, ci = TRUE)))
