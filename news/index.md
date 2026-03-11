# Changelog

## lmhelprs 0.4.4

CRAN release: 2026-03-10

### New Features

- Updated
  [`many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.md)
  to support a model formula that spans more than one line. (0.4.3.1)

## lmhelprs 0.4.3

CRAN release: 2025-05-04

### New Features

- Users can request confidence intervals when using
  [`lm_list_to_partable()`](https://sfcheung.github.io/lmhelprs/reference/lm_list_to_partable.md).
  (0.4.2)

- Updated
  [`hierarchical_lm()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical_lm.md)
  to support the output of
  [`many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.md).
  (0.4.3)

### Bug Fixes

- Fixed an error in
  [`lm_list_to_partable()`](https://sfcheung.github.io/lmhelprs/reference/lm_list_to_partable.md)
  when a model has only one exogenous variables. (0.4.1)

## lmhelprs 0.4.0

CRAN release: 2024-10-15

### New Features

- [`many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.md)
  should now properly support `subset`. (0.3.0.1)

- The `print`-method of the
  [`summary()`](https://rdrr.io/r/base/summary.html) of
  [`many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.md)
  output now prints the cases removed, if any. (0.3.0.1)

- Added
  [`lm_list_to_partable()`](https://sfcheung.github.io/lmhelprs/reference/lm_list_to_partable.md)
  to converted a list of [`lm()`](https://rdrr.io/r/stats/lm.html)
  outputs to a data frame similar to the output of
  [`lavaan::parameterTable()`](https://rdrr.io/pkg/lavaan/man/parTable.html),
  that can be used in
  [`semPlot::semPaths()`](https://rdrr.io/pkg/semPlot/man/semPaths.html)
  to draw the path model fitted by several regression models. (0.3.0.2,
  0.3.0.4)

### Bug Fixes

- Fix a error in
  [`hierarchical_lm()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical_lm.md)
  for a model with only one predictor. (0.3.0.3)

## lmhelprs 0.3.0

CRAN release: 2024-02-18

### New Features

- Added
  [`many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.md).
  (0.2.0.2, 0.2.0.3)
- Added internal checks to
  [`hierarchical_lm()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical_lm.md)
  to check whether any two models are not fitted to an identical
  dataset. (0.2.0.4)

### Miscellaneous

- Added a logo. (0.2.0.5)

## lmhelprs 0.2.0

CRAN release: 2023-10-30

- Added `data_test1`, a test dataset. (0.0.0.9002)
- Added
  [`hierarchical()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical.md).
  (0.0.0.9003)
- Added
  [`hierarchical_lm()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical_lm.md)
  and the `print` method for its output. (0.0.0.9004, 0.0.0.9005,
  0.0.0.9006)
- Added
  [`test_highest()`](https://sfcheung.github.io/lmhelprs/reference/test_highest.md)
  and
  [`highest_order()`](https://sfcheung.github.io/lmhelprs/reference/test_highest.md).
  (0.0.0.9007)
- Added a vignetted. (0.0.0.9008)
- First submission to CRAN. (0.1.0, 0.2.0)
