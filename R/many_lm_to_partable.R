#' @title Convert a 'lm_list' Object To a Parameter Table
#'
#' @description Convert the output of
#' `many_lm()` to a `lavann`-style
#' parameter table.
#'
#' @details This function convert a a
#' lit of `lm` objects, such as the
#' output of [many_lm()] or
#' [manymome::lm2list()], to a table of
#' parameter estimates similar to the
#' output of [lavaan::parameterTable].
#'
#' The output is designed to be used by
#' [semPlot::semPaths()] and so contains
#' only information necessary for the
#' plot.
#'
#' The output of [stats::lm()] is
#' already supported by
#' [semPlot::semPaths()], and it can
#' also combine a list of regression
#' models into on single plot. However,
#' it will convert interaction terms to
#' knots. Moreover, if two interaction
#' terms in two different models share
#' the a variable, it will be incorrectly
#' combined to become a single knot
#' (Version 1.1.6). Therefore, this
#' function was developed to let users
#' to draw the model as if it were a
#' path model in structural equation
#' modeling.
#'
#' @return
#' A data frame object with columns such
#' as `lhs`, `op`, `rhs`, and `est`,
#' major columns of the output of
#' [lavaan::parameterTable()] necessary
#' for plotting the model using
#' [semPlot::semPaths()].
#'
#' @param object The output of
#' [many_lm()] or [manymome::lm2list()].
#'
#' @param keep_intercepts Logical. If
#' `TRUE`, the intercepts of the
#' regression models and the means of
#' the "pure" predictors (variables not
#' being the outcome variables of any of
#' the regression models) are kept in
#' the parameter table. If `FALSE`, the
#' default, all intercepts and means
#' will be removed.
#'
#' @param vcov_args A named list of
#' arguments to be passed to [stats::vcov()]
#' when computing the standard errors
#' of the regression coefficients.
#' Default is `list()`, an empty list.
#'
#' @param pvalue_fun The function to be
#' used to compute the *p*-values of
#' regression coefficients. Ignored for
#' now. Included for adding this feature
#' in the future.
#'
#' @param rsquare Logical. Whether
#' R-squares will be included in the
#' output, with `r2` as the operator
#' in the column `op`. Default is
#' `FALSE`. Not included by default
#' because [semPlot::semPaths()] will
#' draw the R-squares over the residual
#' variances.
#'
#' @param ci Logical. If `TRUE`,
#' confidence intervals will be added,
#' computed by [stats::confint()].
#'
#' @param ci_fun The function to be used
#' to form the confidence intervals for
#' regression coefficients. Default
#' is `stats::confint`
#'
#' @param ci_args A named list of
#' arguments to be passed to `ci_fun`.
#' Default is `list(level = .95)`,
#' requesting 95% confidence intervals.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [many_lm()] and [manymome::lm2list()].
#'
#' @examples
#'
#' data(data_test1)
#' mod <- "x3 ~ x2*x1
#'         x4 ~ x3
#'         x5 ~ x4 + x3"
#' out <- many_lm(mod, data_test1)
#' out_ptable <- lm_list_to_partable(out)
#' out_ptable
#'
#' m <- matrix(c("x1", "x2", "x2:x1", NA, "x3", NA, "x4", NA, NA, NA, "x5", NA),
#'             nrow = 3, ncol = 4)
#' m
#'
#' # The output can be used by semPlot::semPaths()
#'
#' if (requireNamespace("semPlot", quietly = TRUE)) {
#'   library(semPlot)
#'   p <- semPaths(out_ptable,
#'                 what = "paths",
#'                 whatLabels = "est",
#'                 nCharNodes = 0,
#'                 style = "ram",
#'                 layout = m,
#'                 exoCov = FALSE,
#'                 DoNotPlot = TRUE)
#'   plot(p)
#'
#'   # If it is desired to use knots to
#'   # denote interaction terms, then,
#'   # the output of many_lm() can be used
#'   # directly.
#'
#'   m2 <- matrix(c("x1", NA, "x2", NA, "x3", NA, "x4", NA, NA, NA, "x5", NA),
#'             nrow = 3, ncol = 4)
#'   p2 <- semPaths(out,
#'                  what = "paths",
#'                  whatLabels = "est",
#'                  nCharNodes = 0,
#'                  style = "ram",
#'                  layout = m2,
#'                  exoCov = FALSE,
#'                  intercepts = FALSE,
#'                  DoNotPlot = TRUE)
#'   plot(p2)
#'
#'   # This illustrates the problem with using
#'   # the list of lm-outputs directly when
#'   # a variable is involved in the interaction terms
#'   # of two or more models.
#'
#'   m3 <- matrix(c("x2",   NA, "x1",   NA, "x3",
#'                    NA,   NA,   NA,   NA,   NA,
#'                    NA, "x4",   NA, "x5",   NA),
#'             nrow = 5, ncol = 3)
#'   mod3 <- "x4 ~ x2*x1
#'            x5 ~ x3*x1"
#'   out3 <- many_lm(mod3, data_test1)
#'   p3 <- semPaths(out3,
#'                  what = "paths",
#'                  whatLabels = "est",
#'                  nCharNodes = 0,
#'                  style = "ram",
#'                  layout = m3,
#'                  exoCov = FALSE,
#'                  intercepts = FALSE,
#'                  DoNotPlot = TRUE)
#'   plot(p3)
#'
#' }
#'
#' @export

lm_list_to_partable <- function(object,
                                keep_intercepts = FALSE,
                                vcov_args = list(),
                                pvalue_fun = NULL,
                                rsquare = FALSE,
                                ci = FALSE,
                                ci_fun = stats::confint,
                                ci_args = list(level = .95)) {

    ci_fun <- match.fun(ci_fun)

    out_ptable <- lm2ptable_basic(object,
                              vcov_args = vcov_args,
                              pvalue_fun = pvalue_fun)
    out0 <- out_ptable$est

    if (ci) {
      ci_out <- lm_list_to_ci(object,
                              ci_fun = ci_fun,
                              ci_args = ci_args)
      out0 <- merge(x = out0,
                    y = ci_out,
                    all.x = TRUE,
                    all.y = FALSE)
    }

    if (!rsquare) {
        out0 <- out0[-which(out0$op == "r2"), ]
      }

    # Add error variances and means for x-variables
    evar <- out_ptable$sigma
    p <- length(evar)
    tmp1 <- data.frame(lhs = names(evar),
                       op = "~~",
                       rhs = names(evar),
                       est = evar,
                       se = rep(NA, p),
                       pvalue = rep(NA, p),
                       row.names = NULL)

    # Add means for for x-variables
    ovx <- ov_names(out0)
    x_means <- out_ptable$implied_stats$mean[ovx]
    tmp2 <- data.frame(lhs = ovx,
                       op = "~1",
                       rhs = rep("", length(ovx)),
                       est = x_means,
                       se = rep(NA, length(ovx)),
                       pvalue = rep(NA, length(ovx)),
                       row.names = NULL)

    # Add covariances of x-variables
    ovx <- ov_names(out0)
    tmp3 <- mm_cov(out_ptable$implied_stats$cov[ovx, ovx, drop = FALSE])

    if (ci) {
      tmp1$ci.lower <- NA
      tmp1$ci.upper <- NA
      tmp2$ci.lower <- NA
      tmp2$ci.upper <- NA
      tmp3$ci.lower <- NA
      tmp3$ci.upper <- NA
    }

    # Generate the final output
    out <- rbind(out0,
                 tmp1,
                 tmp2,
                 tmp3)
    if (!keep_intercepts) {
        out <- out[out$op != "~1", ]
      }
    out$ustart <- out$est
    p <- nrow(out)
    i_free <- !is.na(out$se)
    out$free <- 0
    out$free[i_free] <- seq_len(sum(i_free))
    out$plabel <- paste0(".p",
                         seq_len(p),
                         ".")
    row.names(out) <- NULL
    out
  }

#' @noRd
lm2ci <- function(object,
                  ci_fun,
                  ci_args) {
  ci_out <- do.call(ci_fun,
                    c(list(object),
                      ci_args))
  colnames(ci_out) <- c("ci.lower", "ci.upper")
  ci_out
}

#' @noRd
lm_list_to_ci <- function(object,
                          ci_fun = stats::confint,
                          ci_args = list(level = .95)) {
  ci_out <- sapply(object,
                    lm2ci,
                    ci_fun = ci_fun,
                    ci_args = ci_args,
                    simplify = FALSE)
  ynames <- sapply(object,
                    get_response)
  ci_out1 <- mapply(ci_table,
                    ci_i = ci_out,
                    y = ynames,
                    SIMPLIFY = FALSE)
  ci_out2 <- do.call(rbind,
                     ci_out1)
  ci_out2
}

#' @noRd

ci_table <- function(ci_i, y) {
  ci_i <- as.data.frame(ci_i)
  p <- nrow(ci_i)
  lhs <- rep(y, p)
  rhs <- rownames(ci_i)
  rhs <- gsub("(Intercept)",
              "",
              rhs,
              fixed = TRUE)
  op <- c("~1", rep("~", p - 1))
  ci_i2 <- data.frame(lhs = lhs,
                      op = op,
                      rhs = rhs,
                      ci.lower = ci_i$ci.lower,
                      ci.upper = ci_i$ci.upper)
  ci_i2
}

# The following functions are adapted from manymome

# https://github.com/cran/manymome/blob/1870ee57601decfab459446494b24e770430cd3c/R/lm2ptable.R

#' @noRd

lm2ptable_basic <- function(lm_list,
                            vcov_args = list(),
                            pvalue_fun = NULL) {
    # Revised
    mm <- merge_model_matrices(lm_list)
    ys <- sapply(lm_list,
                 get_response)
    sigma_y <- sapply(lm_list,
                      stats::sigma)
    names(sigma_y) <- ys
    implied_stats <- list(cov = stats::cov(mm),
                          mean = colMeans(mm))
    coefs <- lapply(lm_list,
                    FUN = coef2lor,
                    vcov_args = vcov_args,
                    pvalue_fun = pvalue_fun)
    out <- do.call(rbind, coefs)
    row.names(out) <- NULL
    list(est = out,
         implied_stats = implied_stats,
         sigma = sigma_y)
  }

#' @noRd

merge_model_matrices <- function(lm_list) {
    # Revised
    mm <- lapply(lm_list,
                 function(x) {
                    out <- stats::model.matrix(x,
                              contrasts.arg = x$contrasts)[, -1, drop = FALSE]
                    y <- get_response(x)
                    y_data <- stats::model.frame(x)[, y, drop = FALSE]
                    cbind(y_data, out)
                  })
    vnames <- unique(unlist(sapply(mm, colnames)))
    idname <- newname(vnames)
    mm1 <- lapply(mm, function(x) {
                          j <- colnames(x)
                          out <- cbind(x, seq_len(nrow(x)))
                          colnames(out) <- c(j, idname)
                          out
                        })
    `%merge%` <- function(x, y) {
        xnames <- colnames(x)
        ynames <- colnames(y)
        ykeep <- c(idname, ynames[!(ynames %in% xnames)])
        y1 <- y[, ykeep, drop = FALSE]
        merge(x, y1,
              by = idname)
      }
    mm2 <- Reduce(`%merge%`, mm1)
    mm2[, idname] <- NULL
    mm2
  }

#' @noRd

get_response <- function(x) {
    # Revised
    x_formula <- stats::formula(x)
    y <- all.vars(x_formula)[attr(stats::terms(x_formula), "response")]
    y
  }

#' @noRd

newname <- function(x) {
    # Revised
    out0 <- make.unique(c(make.names(substring(tempfile(pattern = "id_", tmpdir = ""), 2)), x))
    out <- setdiff(out0, x)
    out
  }

#' @noRd

coef2lor <- function(x,
                     vcov_args = list(),
                     pvalue_fun = NULL) {
    # Revised

    # Not used for now
    pvalue_fun <- NULL

    x_summary <- summary(x)

    y <- get_response(x)
    bs <- stats::coef(x)
    bnames <- names(bs)
    k <- length(bnames)
    lhs <- rep(y, k)
    op <- rep("~", k)
    rhs <- bnames

    # Standard Errors
    vcov_args_final <- utils::modifyList(list(object = x),
                                         vcov_args)
    x_vcov <- do.call(stats::vcov,
                      vcov_args_final)
    se <- sqrt(diag(x_vcov))

    # P-values
    if (is.null(pvalue_fun)) {
        x_coefmat <- x_summary$coefficients
        pvalue <- x_coefmat[, "Pr(>|t|)"]
      } else {
        # TODO: To be added in the future
      }

    # Handle the intercept
    j <- which(bnames == "(Intercept)")
    if (length(j) > 0) {
        op[j] <- "~1"
        rhs[j] <- ""
      }

    # R-squares
    rsq <- x_summary$r.squared
    fstat <-  x_summary$fstatistic
    rsq_pvalue <- stats::pf(q = fstat[1],
                            df1 = fstat[2],
                            df2 = fstat[3])
    lhs <- c(lhs, y)
    op <- c(op, "r2")
    rhs <- c(rhs, y)
    bs <- c(bs, rsq)
    se <- c(se, NA)
    pvalue <- c(pvalue, rsq_pvalue)

    out <- data.frame(lhs = lhs,
                      op = op,
                      rhs = rhs,
                      est = bs,
                      se = se,
                      pvalue = pvalue)
    out
  }

#' @noRd

mm_cov <- function(object) {
    vnames0 <- colnames(object)
    vnames1 <- expand.grid(lhs = vnames0,
                           rhs = vnames0,
                           stringsAsFactors = FALSE)
    cov_v <- as.vector(object)
    i <- upper.tri(object,
                   diag = TRUE)
    i <- as.vector(i)
    cov_v <- cov_v[i]
    vnames1 <- vnames1[i, ]
    out <- data.frame(lhs = vnames1$lhs,
                      op = "~~",
                      rhs = vnames1$rhs,
                      est = cov_v,
                      se = NA,
                      pvalue = NA)
    out
  }

#' @noRd

ov_names <- function(object) {
    i <- (object$op == "~")
    out0 <- object[i, ]
    rhs <- unique(out0$rhs)
    lhs <- unique(out0$lhs)
    pure_x <- setdiff(rhs, lhs)
    pure_x
  }