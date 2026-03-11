# Check Models Hierarchy

Check a list of 'lm' objects to see whether they are be ordered in a way
for doing hierarchical regression analysis.

## Usage

``` r
hierarchical(...)
```

## Arguments

- ...:

  The outputs of [`lm()`](https://rdrr.io/r/stats/lm.html), that is, one
  or more `lm`-class objects. The outputs of other model fitting
  functions may also be used, but should be used with cautions. Please
  refer to the "How it works" section in "Details."

## Value

If the models can be ordered in a hierarchical way, the output is a list
of the original `lm` outputs, sorted from the model with the smallest
number of terms to the model with the largest number of terms. If the
models cannot be ordered this way, `NA` is returned.

## Details

Two models can be compared by hierarchical regression analysis if one
model can be formed by adding one or more terms to the other model.

This function checks whether a list of `lm` outputs can be ordered from
the simplest model to the most complex model, with a more complex model
formed by adding one or more terms to a simpler model.

### How it works

It extracts the terms in each model by
[`stats::terms()`](https://rdrr.io/r/stats/terms.html) and then extracts
the labels of the terms by
[`labels()`](https://rdrr.io/r/base/labels.html). The labels are then
used to determine the hierarchical order.

Therefore, in principle, this function can be used for the outputs of
other model fitting functions as long as their outputs support the
[`stats::terms()`](https://rdrr.io/r/stats/terms.html) and the labels
can be used to determine hierarchical order of two models.

## See also

[`stats::lm()`](https://rdrr.io/r/stats/lm.html)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
dat <- data_test1
lm1 <- lm(y ~ x1 + x2, dat)
lm2 <- lm(y ~ x1 + x2 + x3 + x4, dat)
lm3 <- lm(y ~ x1 + cat1 + cat2 + x2 + x3 + x4, dat)
lm4 <- lm(y ~ x1 + x2*x3 + x4, dat)

# The order of entry does not matter
hierarchical(lm1, lm4, lm2)
#> [[1]]
#> 
#> Call:
#> lm(formula = y ~ x1 + x2, data = dat)
#> 
#> Coefficients:
#> (Intercept)           x1           x2  
#>     -0.1231       0.6016       0.2797  
#> 
#> 
#> [[2]]
#> 
#> Call:
#> lm(formula = y ~ x1 + x2 + x3 + x4, data = dat)
#> 
#> Coefficients:
#> (Intercept)           x1           x2           x3           x4  
#>    -0.04715      0.41484      0.27693      0.35712      0.36060  
#> 
#> 
#> [[3]]
#> 
#> Call:
#> lm(formula = y ~ x1 + x2 * x3 + x4, data = dat)
#> 
#> Coefficients:
#> (Intercept)           x1           x2           x3           x4        x2:x3  
#>    -0.04627      0.41504      0.27698      0.35490      0.35950     -0.01706  
#> 
#> 

# The following three models yield NA
hierarchical(lm3, lm4, lm2)
#> [1] NA
```
