#' @title Sample Data: For Testing
#'
#' @description A eight-variable dataset with 100 cases.
#'
#' @format A data frame with 100 rows
#' and 8 variables:
#' \describe{
#'   \item{x1}{Predictor. Numeric.}
#'   \item{x2}{Predictor. Numeric.}
#'   \item{x3}{Predictor. Numeric.}
#'   \item{x4}{Predictor. Numeric.}
#'   \item{x5}{Predictor. Numeric.}
#'   \item{y}{Outcome. Numeric.}
#'   \item{cat1}{Predictor. String. Values: "Alpha", "Beta", "Gamma"}
#'   \item{cat2}{Predictor. String. Values: "North", "South", "East", "West"}
#' }
#'
#' @examples
#' data(data_test1)
#' lm(y ~ x1 + cat2 + cat1 + cat2:cat1, data_test1)
"data_test1"