# Test the Highest Order Term by ANOVA

Identify the highest order terms in a model fitted by 'lm()', and
compare this model to a model with this term removed using ANOVA.

## Usage

``` r
test_highest(lm_out)

highest_order(lm_out)
```

## Arguments

- lm_out:

  The output of [`stats::lm()`](https://rdrr.io/r/stats/lm.html).

## Value

A `hierarchical_lm`-class object, which is the output of
[`hierarchical_lm()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical_lm.md).
Two models are compared, the original model and the model with the
unique highest order term removed.

## Details

The function `test_highest()` first check if a model fitted by
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) has a unique highest
order term (e.g., the term `x1:x2`, in the model `y ~ x1 + x2 + x1:x2`).
If yes, it will fit a model with this term removed, and then call
[`hierarchical_lm()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical_lm.md)
to compare the original model with this reduced model.

If the model does not have a unique highest order term, an error will be
raised.

## Functions

- `test_highest()`: Test the highest order term.

- `highest_order()`: Find the highest order term.

## Limitation

It relies on terms created by
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) to determine the order
of each term. If a higher order term is created manually (e.g.,
`I(x1 * x2)`), then it cannot know that this term is a second order
term.

## See also

[`hierarchical_lm()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical_lm.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
dat <- data_test1

lm1 <- lm(y ~ x1 + x2 + cat1*x3, dat)
lm2 <- lm(y ~ x1 + x2*x3 + x4, dat)

test_highest(lm1)
#> Analysis of Variance Table
#> 
#> Model 1: y ~ x1 + x2 + cat1 + x3
#> Model 2: y ~ x1 + x2 + cat1 * x3
#>   adj.R.sq   R.sq R.sq.change Res.Df   RSS Df Sum of Sq     F Pr(>F)
#> 1   0.6517 0.6693   0.0000000     94 38.37                          
#> 2   0.6446 0.6698   0.0004711     92 38.32  2   0.05466 0.066  0.937
test_highest(lm2)
#> Analysis of Variance Table
#> 
#> Model 1: y ~ x1 + x2 + x3 + x4
#> Model 2: y ~ x1 + x2 * x3 + x4
#>   adj.R.sq   R.sq R.sq.change Res.Df   RSS Df Sum of Sq     F Pr(>F)
#> 1   0.7269 0.7380   0.0000000     95 30.41                          
#> 2   0.7242 0.7382   0.0002041     94 30.38  1   0.02368 0.073  0.787

highest_order(lm1)
#> [1] "cat1:x3"
highest_order(lm2)
#> [1] "x2:x3"

# The followings will yield an error

lm3 <- lm(y ~ x1 + x2 + x3, dat)
summary(lm3)
#> 
#> Call:
#> lm(formula = y ~ x1 + x2 + x3, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -1.1889 -0.3990 -0.0022  0.4116  1.5963 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -0.08776    0.06515  -1.347    0.181    
#> x1           0.44060    0.06905   6.381 6.20e-09 ***
#> x2           0.30064    0.06013   5.000 2.58e-06 ***
#> x3           0.42361    0.06804   6.226 1.26e-08 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.6436 on 96 degrees of freedom
#> Multiple R-squared:  0.6573, Adjusted R-squared:  0.6466 
#> F-statistic: 61.37 on 3 and 96 DF,  p-value: < 2.2e-16
#> 
tryCatch(test_highest(lm3), error = function(e) e)
#> <simpleError in highest_order(lm_out): No unique highest order term.>
tryCatch(highest_order(lm3), error = function(e) e)
#> <simpleError in highest_order(lm3): No unique highest order term.>

lm4 <- lm(y ~ x1 + x2*x3 + x4*x5, dat)
summary(lm4)
#> 
#> Call:
#> lm(formula = y ~ x1 + x2 * x3 + x4 * x5, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.00627 -0.31572  0.03453  0.30428  1.29839 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -0.012917   0.048049  -0.269 0.788661    
#> x1           0.363822   0.049985   7.279 1.11e-10 ***
#> x2           0.180795   0.045679   3.958 0.000149 ***
#> x3           0.296156   0.050633   5.849 7.49e-08 ***
#> x4           0.324834   0.054432   5.968 4.44e-08 ***
#> x5           0.346000   0.049278   7.021 3.69e-10 ***
#> x2:x3        0.009253   0.051784   0.179 0.858572    
#> x4:x5       -0.072788   0.069142  -1.053 0.295221    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.4589 on 92 degrees of freedom
#> Multiple R-squared:  0.833,  Adjusted R-squared:  0.8203 
#> F-statistic: 65.56 on 7 and 92 DF,  p-value: < 2.2e-16
#> 
tryCatch(test_highest(lm4), error = function(e) e)
#> <simpleError in highest_order(lm_out): No unique highest order term.>
tryCatch(highest_order(lm4), error = function(e) e)
#> <simpleError in highest_order(lm4): No unique highest order term.>
```
