% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/many_lm.R
\name{many_lm}
\alias{many_lm}
\title{Fit Linear Models Defined By
Model Syntax}
\usage{
many_lm(models, data, na_omit_all = TRUE, ...)
}
\arguments{
\item{models}{Character. Model syntax.
See Details.}

\item{data}{The data frame. Must be
supplied if \code{na_omit_all} is \code{TRUE}.
If \code{na_omit_all} is \code{FALSE}, it can
be omitted (though not suggested).}

\item{na_omit_all}{How missing data
is handled across models. If \code{TRUE},
the default, then only cases with
no missing data on all variables used
at least one of the models will be
retained (i.e., listwise deletion).
If \code{FALSE}, then missing data will be
handled in each model separately by
\code{lm()}.}

\item{...}{Additional arguments. To
be passed to \code{lm()}.}
}
\value{
A list of the output of \code{lm()}. The
class is \code{lm_list_lmhelprs}.
}
\description{
Fit a list of linear
models defined by model syntax.
}
\details{
This function extracts
linear model formulas from a
model syntax (a character vector),
fits each of them by \code{lm()}, and
stores the results in a list.

Lines with the first non-whitespace
character \code{"#"} are treated as comments
and ignored.

Each line must be a valid formula
for \code{lm()}.
\subsection{Listwise deletion}{

If \code{na_omit_all} is \code{TRUE}, the
default, then cases with missing
data on at least one of the variables
used in the model will be removed.
Each call to \code{\link[=lm]{lm()}} will have \code{subset}
set to an integer vector of cases
\emph{not} removed (i.e., cases retained)
}

\subsection{Handling the \code{subset} argument}{

If \code{subset} is used when calling this
function, it will also be used to
select cases.

Note that the \code{subset} argument in
the call in each model will be replaced
by a numeric vector of cases retained,
determined by both missing data and
the original value of the \code{subset}.
}
}
\examples{

data(data_test1)
mod <- "x3 ~ x2 + x1
        x4 ~ x3
        x5 ~ x4*x1"
out <- many_lm(mod, data_test1)
summary(out)




}
\seealso{
\code{\link[stats:lm]{stats::lm()}}
}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}
}
