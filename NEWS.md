# lmhelprs 0.4.3

## New Features

- Users can request confidence
  intervals when using
  `lm_list_to_partable()`. (0.4.2)

- Updated `hierarchical_lm()` to support
  the output of `many_lm()`. (0.4.3)

## Bug Fixes

- Fixed an error in `lm_list_to_partable()`
  when a model has only one exogenous
  variables. (0.4.1)

# lmhelprs 0.4.0

## New Features

- `many_lm()` should now properly
  support `subset`. (0.3.0.1)

- The `print`-method of the `summary()`
  of `many_lm()` output now prints
  the cases removed, if any. (0.3.0.1)

- Added `lm_list_to_partable()` to
  converted a list of `lm()` outputs
  to a data frame similar to the output
  of `lavaan::parameterTable()`, that
  can be used in `semPlot::semPaths()`
  to draw the path model fitted by
  several regression models.
  (0.3.0.2, 0.3.0.4)

## Bug Fixes

- Fix a error in `hierarchical_lm()`
  for a model with only one predictor.
  (0.3.0.3)

# lmhelprs 0.3.0

## New Features

- Added `many_lm()`. (0.2.0.2, 0.2.0.3)
- Added internal checks to `hierarchical_lm()`
  to check whether any two models are
  not fitted to an identical dataset.
  (0.2.0.4)

## Miscellaneous

- Added a logo. (0.2.0.5)

# lmhelprs 0.2.0

- Added `data_test1`, a test dataset. (0.0.0.9002)
- Added `hierarchical()`. (0.0.0.9003)
- Added `hierarchical_lm()` and the
  `print` method for its output. (0.0.0.9004, 0.0.0.9005, 0.0.0.9006)
- Added `test_highest()` and
  `highest_order()`. (0.0.0.9007)
- Added a vignetted. (0.0.0.9008)
- First submission to CRAN. (0.1.0, 0.2.0)
