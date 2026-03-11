# lmhelprs

## Introduction

At the time of writing,
[`lmhelprs`](https://sfcheung.github.io/lmhelprs/) has two sets of
functions:

- [`hierarchical_lm()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical_lm.md)
  for ordering linear regression models in a hierarchical way and use
  [`anova()`](https://rdrr.io/r/stats/anova.html) to compare them.

- [`test_highest()`](https://sfcheung.github.io/lmhelprs/reference/test_highest.md)
  to identify the highest order term in a linear regression model and
  compare the model to a model with this term removed.

## Hierarchical Regression Analysis

Users can use [`anova()`](https://rdrr.io/r/stats/anova.html) manually
to do hierarchical regression.
[`hierarchical_lm()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical_lm.md)
is written with these features:

- Check whether the models are really “hierarchical”. If yes, order them
  automatically. Users do not need to put them in the correct order
  manually.

- Compute R-squared and R-squared change and add them to the output.

Let’s fit three models for illustration:

``` r
library(lmhelprs)
data(data_test1)
lm1a <- lm(y ~ x1 + x2, data_test1)
lm1b <- lm(y ~ x1 + x2 + x3 + x4, data_test1)
lm1c <- lm(y ~ x1 + x2 + x3 + x4 + cat2, data_test1)
```

They are can be passed to
[`hierarchical_lm()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical_lm.md)
in any order. Hierarchical regression analysis will be conducted
correctly, with R-squared and R-squared change:

``` r
hierarchical_lm(lm1b, lm1a, lm1c)
#> Analysis of Variance Table
#> 
#> Model 1: y ~ x1 + x2
#> Model 2: y ~ x1 + x2 + x3 + x4
#> Model 3: y ~ x1 + x2 + x3 + x4 + cat2
#>   adj.R.sq   R.sq R.sq.change Res.Df   RSS Df Sum of Sq      F Pr(>F)    
#> 1   0.5090 0.5189     0.00000     97 55.83                               
#> 2   0.7269 0.7380     0.21906     95 30.41  2    25.419 39.314 <0.001 ***
#> 3   0.7242 0.7437     0.00573     92 29.74  3     0.665  0.686  0.563    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

If the models are not in hierarchical order, an error will be raised:

``` r
lm2a <- lm(y ~ x1 + x2, data_test1)
lm2b <- lm(y ~ x1 + x3 + x4, data_test1)
hierarchical_lm(lm2a, lm2b)
#> Error in `hierarchical_lm()`:
#> ! The models do not have hierarchical relations.
```

Please refer to the help page of
[`hierarchical_lm()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical_lm.md)
on how it works, and the print method of its output
([`print.hierarchical_lm()`](https://sfcheung.github.io/lmhelprs/reference/print.hierarchical_lm.md))
on how to customize the printout.

## Test The Highest Order Term

In a linear regression model with a term of second or higher order, the
function
[`test_highest()`](https://sfcheung.github.io/lmhelprs/reference/test_highest.md)
can be used to identify the highest order term, compare the model to a
model with this term removed, and estimate and test the R-squared
increase due to adding this term.

For example:

``` r
lm_mod <- lm(y ~ x1 + cat2 + cat1 + cat2:cat1, data_test1)
summary(lm_mod)
#> 
#> Call:
#> lm(formula = y ~ x1 + cat2 + cat1 + cat2:cat1, data = data_test1)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.54001 -0.63322 -0.02938  0.51963  1.45138 
#> 
#> Coefficients:
#>                      Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)         -0.099251   0.265818  -0.373    0.710    
#> x1                   0.655101   0.079709   8.219 1.78e-12 ***
#> cat2North           -0.510542   0.366084  -1.395    0.167    
#> cat2South           -0.062337   0.386181  -0.161    0.872    
#> cat2West            -0.070097   0.443321  -0.158    0.875    
#> cat1Beta             0.003442   0.386352   0.009    0.993    
#> cat1Gamma            0.026172   0.341096   0.077    0.939    
#> cat2North:cat1Beta  -0.011252   0.565646  -0.020    0.984    
#> cat2South:cat1Beta  -0.475115   0.577723  -0.822    0.413    
#> cat2West:cat1Beta    0.140028   0.577154   0.243    0.809    
#> cat2North:cat1Gamma  0.136924   0.534636   0.256    0.798    
#> cat2South:cat1Gamma  0.564488   0.497376   1.135    0.260    
#> cat2West:cat1Gamma   0.246710   0.606676   0.407    0.685    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.7945 on 87 degrees of freedom
#> Multiple R-squared:  0.5268, Adjusted R-squared:  0.4615 
#> F-statistic:  8.07 on 12 and 87 DF,  p-value: 5.975e-10
```

Although it has six product terms, in terms of variables, it only has
one higher order term: `cat2:cat1`, the interaction between `cat2` and
`cat1`.

To automatically find this term, and compare this model to a model
without this interaction,
[`test_highest()`](https://sfcheung.github.io/lmhelprs/reference/test_highest.md)
can be used:

``` r
test_highest(lm_mod)
#> Analysis of Variance Table
#> 
#> Model 1: y ~ x1 + cat2 + cat1
#> Model 2: y ~ x1 + cat2 + cat1 + cat2:cat1
#>   adj.R.sq   R.sq R.sq.change Res.Df   RSS Df Sum of Sq     F Pr(>F)
#> 1   0.4682 0.5004     0.00000     93 57.97                          
#> 2   0.4615 0.5268     0.02633     87 54.91  6     3.055 0.807  0.567
```

The R-squared change due to adding this interaction (represented by six
product terms) to a model without interaction is 0.02633, and the
*p*-value of this change is 0.567.

The function
[`test_highest()`](https://sfcheung.github.io/lmhelprs/reference/test_highest.md)
can be used for third and higher order term. For example:

``` r
lm_mod3 <- lm(y ~ x1 + x2 + x3*x4*cat2, data_test1)
summary(lm_mod3)
#> 
#> Call:
#> lm(formula = y ~ x1 + x2 + x3 * x4 * cat2, data = data_test1)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.19218 -0.37090 -0.03274  0.35201  1.16865 
#> 
#> Coefficients:
#>                  Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)     -0.078753   0.110170  -0.715  0.47674    
#> x1               0.434185   0.066903   6.490 6.14e-09 ***
#> x2               0.263447   0.057819   4.556 1.79e-05 ***
#> x3               0.536450   0.127818   4.197 6.82e-05 ***
#> x4               0.393184   0.148079   2.655  0.00952 ** 
#> cat2North       -0.119296   0.178050  -0.670  0.50473    
#> cat2South        0.180559   0.165472   1.091  0.27839    
#> cat2West         0.105126   0.176470   0.596  0.55300    
#> x3:x4            0.140337   0.223254   0.629  0.53136    
#> x3:cat2North    -0.358721   0.190254  -1.885  0.06291 .  
#> x3:cat2South    -0.177283   0.158573  -1.118  0.26684    
#> x3:cat2West     -0.341833   0.256182  -1.334  0.18579    
#> x4:cat2North     0.004772   0.186679   0.026  0.97967    
#> x4:cat2South    -0.167742   0.200490  -0.837  0.40521    
#> x4:cat2West      0.046933   0.248811   0.189  0.85085    
#> x3:x4:cat2North -0.202793   0.245556  -0.826  0.41128    
#> x3:x4:cat2South -0.264180   0.243922  -1.083  0.28196    
#> x3:x4:cat2West  -0.274082   0.336493  -0.815  0.41770    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.5771 on 82 degrees of freedom
#> Multiple R-squared:  0.7646, Adjusted R-squared:  0.7159 
#> F-statistic: 15.67 on 17 and 82 DF,  p-value: < 2.2e-16
test_highest(lm_mod3)
#> Analysis of Variance Table
#> 
#> Model 1: y ~ x1 + x2 + x3 + x4 + cat2 + x3:x4 + x3:cat2 + x4:cat2
#> Model 2: y ~ x1 + x2 + x3 * x4 * cat2
#>   adj.R.sq   R.sq R.sq.change Res.Df   RSS Df Sum of Sq     F Pr(>F)
#> 1   0.7217 0.7611    0.000000     85 27.72                          
#> 2   0.7159 0.7646    0.003568     82 27.31  3     0.414 0.414  0.743
```

The model with three-way interaction is compared to a model with only
two-way interactions (`x3:x4`, `x3:cat2`, and `x4:cat2`).

If a model has more than one term of the highest order, an error will be
raised:

``` r
lm_mod2 <- lm(y ~ x1 + x2*x3 + x2*x4, data_test1)
test_highest(lm_mod2)
#> Error in `highest_order()`:
#> ! No unique highest order term.
```

Please refer to the help page of
[`test_highest()`](https://sfcheung.github.io/lmhelprs/reference/test_highest.md)
on how it works,

## Issues

If you have any suggestions and found any bugs, please feel feel to open
a GitHub issue. Thanks.
