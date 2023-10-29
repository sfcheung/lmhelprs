#' @noRd

same_response <- function(...) {
    lm_outs <- list(...)
    terms_all <- lapply(lm_outs,
                        stats::terms)
    allvars_all <- lapply(terms_all,
                          all.vars)
    response_id <- sapply(terms_all,
                          attr,
                          which = "response")
    response_all <- mapply(function(x, y) {x[y]},
                           x = allvars_all,
                           y = response_id)
    if (length(unique(response_all)) == 1) {
        return(TRUE)
      } else {
        return(FALSE)
      }
  }
