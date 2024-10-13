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

