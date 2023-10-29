if (FALSE) {

#' @title One Line Title
#'
#' @description One paragraph description
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @param arg1 Argument description.
#' @param ... Additional arguments.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @references
#' Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
#' (2022) Improving an old way to measure moderation effect in standardized
#' units. Advance online publication. *Health Psychology*.
#' \doi{10.1037/hea0001188}
#'
#' @seealso [functionname()]
#'
#' @family relatedfunctions
#'
#' @examples
#' \donttest{
#' }
#'
#' @export
#'
#' @describeIn topic Description of this function
#' @order 1
data_listwise <- function(...) {
    out <- model_frame_list(...)
    return(out)
  }

#' @noRd

lm_listwise <- function(...,
                        original_data) {
    if (missing(original_data)) {
        stop("original_data must be supplied.")
      }
    lm_outs <- list(...)
    allvars <- sapply(lm_outs,
                  function(x) {all.vars(stats::terms(x))})
    allvars <- unique(unlist(allvars))
    dat1 <- original_data[, allvars, drop = FALSE]
    dat1 <- dat1[stats::complete.cases(dat1), , drop = FALSE]
    out <- lapply(lm_outs,
                  function(x) {
                      stats::update(x,
                                    data = dat1,
                                    evaluate = FALSE)
                    })
    out2 <- lapply(out,
                   function(x) {
                      x$formula <- eval(x$formula,
                                        envir = dat1)
                      x$data <- NULL
                      x
                      eval(x,
                           envir = parent.frame())
                    })
    out2
  }


#' @noRd

model_frame_list <- function(...) {
    lm_outs <- list(...)
    mfs <- lapply(lm_outs,
                  stats::model.frame)
    if (length(unique(sapply(mfs, nrow))) != 1) {
        stop("Sample sizes are not the same for all analyses.")
      }
    n <- unique(sapply(mfs, nrow))
    allvars <- sapply(lm_outs,
                      function(x) {all.vars(stats::terms(x))})
    allvars <- unique(unlist(allvars))
    out <- Reduce(merge,
                  mfs)
    if (!setequal(allvars, colnames(out))) {
        stop("Merge failed. The variables in the merged dataset ",
             "different from those in the models")
      }
    if (n != nrow(out)) {
        stop("Merge failed. The analyses were not based on",
             "exactly the same ", n, " cases.")
      }
    # TODO: Somethings' wrong. The case with lm4a and lm4b cannot be detected.
    out
  }

#' @noRd

library(lmhelprs)

dat <- data_test1
dat_miss <- dat
dat_miss[1, "x3"] <- NA
dat_miss[1, "x2"] <- NA

lm1a <- lm(y ~ x1 + x2, dat)
lm1b <- lm(y ~ x1 + x2 + x3 + x4, dat)
lm1c <- lm(y ~ x1 + x2 + x3 + x4 + cat2, dat)
lm2a <- lm(y ~ x2 + x3 + cat1, dat)

lm3a <- lm(y ~ x1 + x2, dat[-1, ])
lm3b <- lm(y ~ x1 + x2 + x3 + x4, dat[-2, ])

lm4a <- lm(y ~ x1 + x2, dat_miss)
lm4b <- lm(y ~ x1 + x2 + x3 + x4, dat_miss)

tmp <- lm_listwise(lm3a, lm3b)

expect_equal(nrow(lmhelprs:::model_frame_list(lm1a, lm1c, lm1b, lm2a)), 100)
expect_error(lmhelprs:::model_frame_list(lm3a, lm1a))
expect_error(lmhelprs:::model_frame_list(lm3a, lm3b))
expect_error(lmhelprs:::model_frame_list(lm4a, lm4b))

expect_equal(nrow(data_listwise(lm1a, lm1c, lm1b, lm2a)), 100)
expect_error(data_listwise(lm3a, lm1a))
expect_error(data_listwise(lm3a, lm3b))

}
