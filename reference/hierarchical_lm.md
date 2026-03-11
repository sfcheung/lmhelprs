# Hierarchical Regression Analysis

Do hierarchical regression analysis on two or more models fitted by
'lm()'.

## Usage

``` r
hierarchical_lm(...)
```

## Arguments

- ...:

  The outputs of [`lm()`](https://rdrr.io/r/stats/lm.html), that is, one
  or more `lm`-class objects. The outputs of other model fitting
  functions may also be used, but should be used with cautions. Please
  refer to the "How it works" section in "Details." It also supports the
  output of
  [`many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.md),
  and can mix the outputs of
  [`many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.md)
  with those of [`lm()`](https://rdrr.io/r/stats/lm.html).

## Value

If the models can be ordered in a hierarchical way, the output is an
ANOVA table with the R-squared estimate of each model, and the R-squared
change of each model compared to the simpler model preceding this model
in the order. The class of the output is `hierarchical_lm`, with a print
method. If the models cannot be ordered this way, `NA` is returned.

### How it works

It call
[`hierarchical()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical.md)
firsts to order the outputs for
[`stats::lm()`](https://rdrr.io/r/stats/lm.html), If they can be ordered
in a hierarchical way, they will be passed to
[`stats::anova()`](https://rdrr.io/r/stats/anova.html). R-squared and
R-squared change will be computed if they are available in the
[`summary()`](https://rdrr.io/r/base/summary.html) method applied to
each model.

Therefore, in principle, this function can also be used for the outputs
of other model fitting functions if their outputs have
[`stats::anova()`](https://rdrr.io/r/stats/anova.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) methods.

### Check Datasets Used

The comparison is meaningful only if all models are fitted to the same
datasets. There is not way to guarantee this is the case, given only the
output of [`lm()`](https://rdrr.io/r/stats/lm.html). However, there are
necessary conditions to claim that the same datasets are used: the
number of cases are the same, the means, variances, and covariances of
numerical variables, and the frequency distributions of variables common
to two models are identical. If at least one of these conditions is not
met, then two models must have been fitted to two different datasets.

The function will check these conditions and raise an error if any of
these necessary conditions are not met.

## Details

It conducted hierarchical regression analysis on two or more models
fitted by [`stats::lm()`](https://rdrr.io/r/stats/lm.html). The models
must be able to be ordered from the simplest to the most complex, with
each more complex model formed by adding one or more terms to the
simpler model.

ANOVA will be conducted to compare each model with the next more complex
model in the order, with R-squared change computed.

## See also

[`stats::lm()`](https://rdrr.io/r/stats/lm.html),
[`hierarchical()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
dat <- data_test1
lm1 <- lm(y ~ x1 + x2, dat)
lm2 <- lm(y ~ x1 + x2 + x3 + x4, dat)
lm3 <- lm(y ~ x1 + cat1 + cat2 + x2 + x3 + x4, dat)
lm4 <- lm(y ~ x1 + x2*x3 + x4, dat)

hierarchical_lm(lm1, lm3, lm2)
#> Analysis of Variance Table
#> 
#> Model 1: y ~ x1 + x2
#> Model 2: y ~ x1 + x2 + x3 + x4
#> Model 3: y ~ x1 + cat1 + cat2 + x2 + x3 + x4
#>   adj.R.sq   R.sq R.sq.change Res.Df   RSS Df Sum of Sq      F Pr(>F)    
#> 1   0.5090 0.5189     0.00000     97 55.83                               
#> 2   0.7269 0.7380     0.21906     95 30.41  2    25.419 39.717 <0.001 ***
#> 3   0.7270 0.7518     0.01385     90 28.80  5     1.607  1.004   0.42    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
hierarchical_lm(lm1, lm2, lm4)
#> Analysis of Variance Table
#> 
#> Model 1: y ~ x1 + x2
#> Model 2: y ~ x1 + x2 + x3 + x4
#> Model 3: y ~ x1 + x2 * x3 + x4
#>   adj.R.sq   R.sq R.sq.change Res.Df   RSS Df Sum of Sq      F Pr(>F)    
#> 1   0.5090 0.5189      0.0000     97 55.83                               
#> 2   0.7269 0.7380      0.2191     95 30.41  2    25.419 39.320 <0.001 ***
#> 3   0.7242 0.7382      0.0002     94 30.38  1     0.024  0.073  0.787    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# The following models will yield an error message:
tryCatch(hierarchical_lm(lm1, lm3, lm2, lm4), error = function(e) e)
#> <simpleError in hierarchical_lm(lm1, lm3, lm2, lm4): The models do not have hierarchical relations.>
```
