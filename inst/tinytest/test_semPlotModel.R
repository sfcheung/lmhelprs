# WIP

library(lmhelprs)

dat <- data_test1
dat$x1 <- dat$x1 * 10
dat$x2 <- dat$x2 * 5
dat$x3 <- dat$x3 * 20
dat$x4 <- dat$x4 * 30
dat$x5 <- dat$x5 * 5
dat$y <- dat$y * 20

mod1 <- "

# Comments
    x5 ~ x1 + x2
        # Second models


         y ~ x2 + x1 + x3 + x4 + x5
x3 ~ cat1 + x1 + x2*x4
  # The last line


         "

out1 <- many_lm(mod1, dat)

out1b <- lm_list_to_partable(out1)
expect_equivalent(out1b[(out1b$lhs == "y") &
                        (out1b$op == "~"), "est"],
                  coef(out1[[2]])[-1])
se_chk <- sqrt(diag(vcov(out1[[2]])))
expect_equivalent(out1b[(out1b$lhs == "y") &
                        (out1b$op == "~"), "se"],
                  se_chk[-1])

out1c <- lm_list_to_partable(out1, keep_intercepts = TRUE)
expect_equivalent(out1c[(out1c$lhs == "y") &
                        (out1c$op == "~1"), "est"],
                  coef(out1[[2]])[1])

if (FALSE) {
# Not used for now

# many_lm_std_selected <- function(object,
#                                  to_standardize = ~ .) {
#     out0 <- lapply(object,
#                    stdmod::std_selected,
#                    to_standardize = to_standardize)
#     class(out0) <- class(object)
#     a_names <- setdiff(names(attributes(object)), "class")
#     for (xx in a_names) {
#         attr(out0, xx) <- attr(object, xx)
#       }
#     out0
#   }

# std1 <- many_lm_std_selected(out1)
# std2 <- many_lm_std_selected(out1,
#                              to_standardize = ~ x1)
# summary(std2)

# std1b <- lm_list_to_partable(std1)
# std1b

# std1c <- lm_list_to_partable(std1, keep_intercepts = TRUE)
# std1c

}

if (requireNamespace("semPlot", quietly = TRUE)) {
library(semPlot)
# To be tested in an interactive session

# m <- layout_matrix(x1 = c(1, 1),
#                    x2 = c(2, 1),
#                    x4 = c(3, 1),
#                    cat1Beta = c(4, 1),
#                    cat1Gamma = c(5, 1),
#                    "x2:x4" = c(6, 1),
#                    x5 = c(1, 2),
#                    x3 = c(5, 2),
#                    y = c(3, 3))
m <- structure(c("x1", "x2", "x4", "cat1Beta", "cat1Gamma", "x2:x4",
"x5", NA, NA, NA, "x3", NA, NA, NA, "y", NA, NA, NA), dim = c(6L,
3L))

p <- semPaths(out1b,
              what = "paths",
              whatLabels = "est",
              nCharNodes = 0,
              style = "ram",
              label.scale = FALSE,
              layout = m,
              exoVar = FALSE,
              exoCov = FALSE,
              residuals = FALSE,
              DoNotPlot = TRUE)
plot(p)

# pstd <- semPaths(std1b,
#                  what = "paths",
#                  whatLabels = "est",
#                  nCharNodes = 0,
#                  style = "lisrel",
#                  label.scale = FALSE,
#                  layout = m,
#                  exoVar = FALSE,
#                  exoCov = FALSE,
#                  residuals = FALSE,
#                  DoNotPlot = TRUE)
# plot(pstd)
}
