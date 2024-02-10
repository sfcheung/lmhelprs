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
#' @param ... Additional arguments. To
#' be passed to `lm()`.
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
    if (is.null(my_call$data) && na_omit_all) {
        stop("The argument 'data' must be set if na_omit_all is TRUE.")
      }
    models1 <- parse_models(models)
    if (na_omit_all) {
        all_vars <- sapply(models1,
                            function(x) {
                                all.vars(stats::as.formula(x))
                              },
                          USE.NAMES = FALSE)
        all_vars <- unique(unlist(all_vars))
        data <- stats::na.omit(eval(my_call$data,
                                    envir = parent.frame())[, all_vars])
        my_call$data <- data
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
    out2
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
    xx
  }

