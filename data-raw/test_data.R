# Generate data

set.seed(31415)
n <- 100
p <- 5
sigma <- matrix(.2, p, p)
diag(sigma) <- 1
xx <- MASS::mvrnorm(n, rep(0, p), sigma)
i <- matrix(.3, p, 1)
esd <- sqrt(1 - t(i) %*% sigma %*% i)
y <- xx %*% i + rnorm(n, 0, esd)
dat <- data.frame(xx, y)
colnames(dat) <- c(paste0("x", seq_len(p)), "y")
dat$cat1 <- sample(c("Alpha", "Beta", "Gamma"), n, replace = TRUE)
dat$cat2 <- sample(c("North", "South", "East", "West"), n, replace = TRUE)
data_test1 <- dat
usethis::use_data(data_test1, overwrite = TRUE)

