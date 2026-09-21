#' @title Fit Linear Models Defined By
#' Model Syntax
#'
#' @description Fit a list of linear
#' models defined by model syntax.
#'
#' @details This function extracts
#' linear model formulas from a
#' model syntax (a character vector),
#' fits each of them by `lm()`, and
#' stores the results in a list.
#'
#' Lines with the first non-whitespace
#' character `"#"` are treated as comments
#' and ignored.
#'
#' Each line must be a valid formula
#' for `lm()`.
#'
#' ## Listwise deletion
#'
#' If `na_omit_all` is `TRUE`, the
#' default, then cases with missing
#' data on at least one of the variables
#' used in the model will be removed.
#' Each call to [lm()] will have `subset`
#' set to an integer vector of cases
#' *not* removed (i.e., cases retained)
#'
#' ## Handling the `subset` argument
#'
#' If `subset` is used when calling this
#' function, it will also be used to
#' select cases.
#'
#' Note that the `subset` argument in
#' the call in each model will be replaced
#' by a numeric vector of cases retained,
#' determined by both missing data and
#' the original value of the `subset`.
#'
#' @return
#' A list of the output of `lm()`. The
#' class is `lm_list_lmhelprs`.
#'
#' @param models Character. Model syntax.
#' See Details.
#'
#' @param data The data frame. Must be
#' supplied if `na_omit_all` is `TRUE`.
#' If `na_omit_all` is `FALSE`, it can
#' be omitted (though not suggested).
#'
#' @param na_omit_all How missing data
#' is handled across models. If `TRUE`,
#' the default, then only cases with
#' no missing data on all variables used
#' at least one of the models will be
#' retained (i.e., listwise deletion).
#' If `FALSE`, then missing data will be
#' handled in each model separately by
#' `lm()`.
#'
#' @param ... Additional arguments. For
#' [many_lm()], these arguments will
#' be passed to `lm()`.
#' For the update method of the output
#' of [many_lm()], these arguments will
#' used to update the call to [many_lm()].
#' For the coefficient method, these
#' arguments will be ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [stats::lm()]
#'
#' @examples
#'
#' data(data_test1)
#' mod <- "x3 ~ x2 + x1
#'         x4 ~ x3
#'         x5 ~ x4*x1"
#' out <- many_lm(mod, data_test1)
#' summary(out)
#'
#'
#'
#'
#' @export

many_lm <- function(models,
                    data,
                    na_omit_all = TRUE,
                    ...) {
    my_call <- match.call()
    my_call$na_omit_all <- NULL
    my_call[[1L]] <- quote(stats::lm)
    my_mm <- my_call
    my_mm[[1L]] <- quote(stats::model.frame)
    my_mm$models <- NULL
    if (is.null(my_call$data) && na_omit_all) {
        stop("The argument 'data' must be set if na_omit_all is TRUE.")
      }
    models1 <- parse_models(models)
    data_full <- eval(my_call$data,
                      envir = parent.frame())
    if (na_omit_all) {
        all_vars <- sapply(models1,
                            function(x) {
                                all.vars(stats::as.formula(x))
                              },
                          USE.NAMES = FALSE)
        all_vars <- unique(unlist(all_vars))
        # Get a vector of cases omitted by subset
        if (!is.null(my_call$subset)) {
            all_ys <- sapply(models1,
                                function(x) {
                                    all.vars(stats::as.formula(x))[1]
                                  },
                              USE.NAMES = FALSE)
            ys1 <- all_ys[1]
            f0 <- paste(ys1, "~", paste(setdiff(all_vars, ys1), collapse = " + "))
            my_mm0 <- my_mm
            my_mm0$formula <- f0
            my_mm0$na.action <- "na.pass"
            # This solution is not ideal but should work in most scenarios.
            data_selected <- tryCatch(eval(my_mm0,
                                           envir = parent.frame()),
                                      error = function(e) e)
            if (inherits(data_selected, "error")) {
                subset_i <- eval(my_mm0$subset,
                                 envir = parent.frame())
                if (is.logical(subset_i)) {
                    subset_i <- sapply(subset_i, isTRUE)
                    omitted_subset <- which(!subset_i)
                  } else if (is.character(subset_i)) {
                    omitted_subset <- !(row.names(data_full) %in% subset_i)
                    omitted_subset[is.na(omitted_subset)] <- TRUE
                    omitted_subset <- which(omitted_subset)
                  } else {
                    # Numeric
                    omitted_subset <- setdiff(seq_len(nrow(data_full)),
                                              subset_i)
                  }
              } else {
                subset_i <- match(row.names(data_selected),
                                  row.names(data_full))
                subset_i <- stats::na.omit(subset_i)
                omitted_subset <- setdiff(seq_len(nrow(data_full)),
                                          subset_i)
              }
          } else {
            omitted_subset <- numeric(0)
          }
        # omitted_subset: An integer vector of cases to be omitted
        data <- stats::na.omit(eval(my_call$data,
                                    envir = parent.frame())[, all_vars])
        data_omitted <- attr(data,
                             "na.action")
        if (!is.null(data_omitted)) {
            omitted_listwise <- as.numeric(data_omitted)
          } else {
            omitted_listwise <- numeric(0)
          }
        omitted <- c(omitted_subset,
                     omitted_listwise)
        omitted <- unique(sort(omitted))
        selected <- !logical(nrow(data_full))
        selected[omitted] <- FALSE
        my_call$subset <- which(selected)
      } else {
        selected <- !logical(nrow(data_full))
      }
    tmpfct <- function(x, env = parent.frame()) {
         call0 <- my_call
         call0$models <- NULL
         call0$formula <- stats::as.formula(x, env = env)
         outtmp <- eval(call0, envir = env)
         outtmp
      }
    out1 <- lapply(models1,
                   tmpfct,
                   env = parent.frame())
    # No need to check whether listwise is used.
    out2 <- lm2list_free(out1)
    attr(out2, "cases_removed") <- which(!selected)
    attr(out2, "call") <- match.call()
    out2
  }

#' @details
#' The output of [many_lm()] has a update
#' method.
#'
#' @return
#' For the update method of the output
#' of [many_lm()], if `evaluate` is `TRUE`,
#' the updated output of [many_lm()]
#' will be returned. If `evaluate` is
#' `FALSE`, the updated call will be
#' returned.
#'
#' @param object For the update method,
#' it is the output of [many_lm()].
#'
#' @param evaluate If `TRUE`, the
#' updated call will be evaluated. If
#' `FALSE`, the updated call will be
#' returned.
#'
#' @rdname many_lm
#' @export
update.lm_list_lmhelprs <- function(
  object,
  ...,
  evaluate = TRUE
) {
  # Adapted from update.default()
  call <- attr(object, "call")
  extras <- match.call(expand.dots = FALSE)$...
  if (length(extras)) {
    existing <- !is.na(match(names(extras), names(call)))
    for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
    if (any(!existing)) {
      call <- c(as.list(call), extras[!existing])
      call <- as.call(call)
    }
  }
  if (evaluate) {
    return(eval(call, parent.frame()))
  } else {
    return(call)
  }
}

#' @details
#' The `coef` method extracts the
#' regression coefficients in the
#' `lavaan` style: `y ~ x`, `y`
#' the response variable and `x` a term.
#'
#' @return
#' The `coef` method returns a numeric
#' vector of the coefficients.
#'
#' @param y For some methods,
#' if set to a character vector,
#' only the coefficients of the models
#' of the response variables listed in
#' `y` will be returned. If `NULL`, all
#' coefficients will be returned.
#'
#' @rdname many_lm
#' @export
coef.lm_list_lmhelprs <- function(
  object,
  y = NULL,
  ...
) {
  out0 <- lm_list_to_partable(
    object,
    keep_intercepts = TRUE
  )
  i <- out0$op %in% c("~", "~1")
  out1 <- out0[i, , drop = FALSE]
  out1$lavlabel <- paste0(
    out1$lhs,
    out1$op,
    out1$rhs
  )
  vnames <- unique(out1$lhs)
  j <- sapply(
        vnames,
        \(x) {
          !any(out1[out1$lhs == x, "op", drop = TRUE] %in% "~")
        })
  ov_x <- vnames[j]
  ov_y <- vnames[!j]
  if (!is.null(y)) {
    if (!is.null(y)) {
      tmp <- y %in% ov_y
      if (!all(tmp)) {
        tmp2 <- y[!tmp]
        stop("y not all in the model: ",
            paste0(tmp2, collapse = ", "))
      }
    }
    ov_y <- y
  }
  out1 <- out1[out1$lhs %in% ov_y, , drop = FALSE]
  out2 <- out1$est
  names(out2) <- out1$lavlabel
  out2
}

#' @return
#' The `vcov` method returns the
#' variance-covariance matrix of the
#' parameter estimates of the models.
#'
#' @details
#' The `vcov` method is used to extract
#' the variance-covariance matrices of
#' the models.
#'
#' @param vcov_args A named list of
#' arguments to be passed to [stats::vcov()]
#' when computing the variance-covariance
#' matrices
#' of the regression coefficients.
#' Default is `list()`, an empty list.
#'
#' @rdname many_lm
#' @export
vcov.lm_list_lmhelprs <- function(
  object,
  y = NULL,
  vcov_args = list(),
  ...
) {
  est <- stats::coef(object, y = y, ...)
  pvalue_fun <- NULL

  # VCOV
  vcov_all <- lapply(
    object,
    \(x) {
      do.call(
        stats::vcov,
        c(list(object = x),
          vcov_args)
      )
    }
  )
  y_names <- sapply(
    object,
    get_response
  )
  for (i in seq_along(vcov_all)) {
    y_name <-y_names[i]
    tmp1 <- vcov_all[[i]]
    tmp2 <- colnames(tmp1)
    tmp2 <- gsub("(Intercept)",
                 "1",
                 tmp2,
                 fixed = TRUE)
    tmp2 <- paste0(y_name, "~", tmp2)
    colnames(tmp1) <- rownames(tmp1) <- tmp2
    vcov_all[[i]] <- tmp1
  }
  p <- length(est)
  out <- matrix(0, p, p)
  est_names <-names(est)
  colnames(out) <- rownames(out) <- est_names
  for (vcov_i in vcov_all) {
    tmp <- intersect(est_names, colnames(vcov_i))
    if (length(tmp) < 1) next
    out[tmp, tmp] <- vcov_i[tmp, tmp]
  }
  out
}

#' @details
#' The `confint` method computes the
#' confidence intervals for the coefficients.
#'
#' @return
#' The `confint` method returns a two-column
#' matrix of the confidence intervals.
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
#'
#' @param parm The parameters for which
#' the confidence intervals will be returned.
#' If `NULL`, the confidence intervals
#' for all coefficients will be returned.
#'
#' @param level The level of confidence
#' of the confidence levels.
#'
#' @rdname many_lm
#' @export
confint.lm_list_lmhelprs <- function(
  object,
  parm = NULL,
  level = .95,
  ci_fun = stats::confint,
  ci_args = list(),
  ...
) {
  ci_args <- utils::modifyList(
    ci_args,
    list(level = level)
  )
  out0 <- lm_list_to_partable(
    object = object,
    keep_intercepts = TRUE,
    ci = TRUE,
    ci_fun = ci_fun,
    ci_args = ci_args
  )
  est <- stats::coef(object)
  out0$lavlabel <- paste0(
    out0$lhs,
    out0$op,
    out0$rhs
  )
  out0 <- out0[out0$lavlabel %in% names(est), ]
  tmp <- do.call(
    ci_fun,
    c(list(object = object[[1]]),
      ci_args)
  )
  out1 <- out0[, c("ci.lower", "ci.upper")]
  out1 <- as.matrix(out1)
  rownames(out1) <- out0$lavlabel
  out1 <- out1[names(est), ]
  if (!is.null(parm)) {
    out1 <- out1[parm, ]
  }
  colnames(out1) <- colnames(tmp)
  out1
}
#' @noRd

lm2list_free <- function(...) {
    outputs <- list(...)
    if ((is.list(outputs)) && (length(outputs) == 1)) {
        outputs <- unlist(outputs, recursive = FALSE)
      }
    class(outputs) <- c("lm_list_lmhelprs", "lm_list", class(outputs))
    outputs
  }

#' @noRd

parse_models_old <- function(x) {
    if (!is.character(x)) {
        stop("Input not characters/strings.")
      }
    if (length(x) > 1) {
        x <- paste(x, collapse = "\n")
      }
    xx <- strsplit(x, "\n")[[1]]
    ws <- grepl("^\\s*$", xx)
    xx <- xx[!ws]
    cm <- grepl("^\\s*\\#", xx)
    xx <- xx[!cm]
    if (length(xx) == 0) {
        stop("No valid formulas.")
      }
    xx <- trimws(xx)
    xx
  }

#' @noRd

parse_models <- function(x) {
  if (!is.character(x)) {
      stop("Input not characters/strings.")
    }
  if (length(x) > 1) {
      x <- paste(x, collapse = "\n")
    }
  xx <- strsplit(x, "\n")[[1]]
  ws <- grepl("^\\s*$", xx)
  xx <- xx[!ws]
  cm <- grepl("^\\s*\\#", xx)
  xx <- xx[!cm]
  if (length(xx) == 0) {
      stop("No valid formulas.")
    }
  xx <- trimws(xx)
  # Find line with a rigbt-hand side term
  op <- grepl("~", xx, fixed = TRUE)
  tmp <- cut(
            seq_along(xx),
            breaks = c(0, which(op), length(xx) + 1),
            right = FALSE
          )
  out <- split(xx,
               tmp,
               drop = TRUE)
  out <- sapply(
            out,
            paste,
            collapse = " "
          )
  unname(out)
}

