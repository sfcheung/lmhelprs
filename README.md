<!-- badges: start -->
[![Lifecycle: Experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Code size](https://img.shields.io/github/languages/code-size/sfcheung/lmhelprs.svg)](https://github.com/sfcheung/lmhelprs)
[![Last Commit at Main](https://img.shields.io/github/last-commit/sfcheung/lmhelprs.svg)](https://github.com/sfcheung/lmhelprs/commits/main)
[![R-CMD-check](https://github.com/sfcheung/lmhelprs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sfcheung/lmhelprs/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

(Version 0.0.0.9006, updated on 2023-10-29, [release history](https://sfcheung.github.io/lmhelprs/news/index.html))

# lmhelprs: A collection of helper functions for some common tasks in fitting linear models, mainly by `lm()`.

A collection of helper functions for
multiple regression models fitted by `lm()`.
Most of them are simple functions for
simple tasks which can be done with coding,
but may not be easy for occasional users of `R`.

For more information on this package, please visit its GitHub page:

https://sfcheung.github.io/lmhelprs/

# Installation

Stable release versions of this package can be downloaded below:

https://github.com/sfcheung/lmhelprs/releases

The latest developmental version of this package can be installed by `remotes::install_github`:

```r
remotes::install_github("sfcheung/lmhelprs")
```

# Background

Most of the tasks I covered are those sometimes I needed when
using the [`manymome` package](https://sfcheung.github.io/manymome/)
(Cheung & Cheung, 2023) and
and the [`stdmod` package](https://sfcheung.github.io/stdmod/)
(Cheung, Cheung, Lau, Hui, and Vong, 2022).
Therefore, when ready, these two packages
will make use of the functions from
`lmhelprs`. However, most of the functions
can also be used in other scenarios.
Therefore, I named it `lmhelprs`.

# References

- Cheung, S. F., & Cheung, S.-H. (2023).
  *manymome*: An R package for computing
  the indirect effects, conditional
  effects, and conditional indirect
  effects, standardized or
  unstandardized, and their bootstrap
  confidence intervals, in many (though
  not all) models. *Behavior Research
  Methods*.
  https://doi.org/10.3758/s13428-023-02224-z

- Cheung, S. F., Cheung, S.-H., Lau, E.
  Y. Y., Hui, C. H., & Vong, W. N. (2022)
  Improving an old way to measure
  moderation effect in standardized
  units. *Health Psychology, 41*(7),
  502-505.
  https://doi.org/10.1037/hea0001188.

# Issues

If you have any suggestions and found any bugs, please feel
feel to open a GitHub issue. Thanks.