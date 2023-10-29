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
hierarchical <- function(...) {
    lm_outs <- list(...)
    if (length(lm_outs) == 1) {
        return(lm_outs[[1]])
      }
    p_all <- sapply(lm_outs,
                    function(x) {
                        length(labels(stats::terms(x)))
                      })
    lm_order <- order(p_all)
    lm_sorted <- lm_outs[lm_order]
    for (i in seq_len(length(lm_sorted) - 1)) {
        if (!isTRUE(hierarchical_i(lm_sorted[[i]],
                             lm_sorted[[i + 1]]))) {
            return(NA)
          }
      }
    return(lm_sorted)
  }

#' @noRd

hierarchical_i <- function(a, b) {
    termsa <- stats::terms(a)
    termsb <- stats::terms(b)
    labelsa <- labels(termsa)
    labelsb <- labels(termsb)
    b_wi_a <- setequal(union(labelsa, labelsb), labelsa)
    a_wi_b <- setequal(union(labelsa, labelsb), labelsb)
    if (b_wi_a && a_wi_b) {
        return(NA)
      }
    if (b_wi_a) {
        return(FALSE)
      }
    if (a_wi_b) {
        return(TRUE)
      }
    NA
  }
