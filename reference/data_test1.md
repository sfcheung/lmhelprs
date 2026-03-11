# Sample Data: For Testing

A eight-variable dataset with 100 cases.

## Usage

``` r
data_test1
```

## Format

A data frame with 100 rows and 8 variables:

- x1:

  Predictor. Numeric.

- x2:

  Predictor. Numeric.

- x3:

  Predictor. Numeric.

- x4:

  Predictor. Numeric.

- x5:

  Predictor. Numeric.

- y:

  Outcome. Numeric.

- cat1:

  Predictor. String. Values: "Alpha", "Beta", "Gamma"

- cat2:

  Predictor. String. Values: "North", "South", "East", "West"

## Examples

``` r
data(data_test1)
lm(y ~ x1 + cat2 + cat1 + cat2:cat1, data_test1)
#> 
#> Call:
#> lm(formula = y ~ x1 + cat2 + cat1 + cat2:cat1, data = data_test1)
#> 
#> Coefficients:
#>         (Intercept)                   x1            cat2North  
#>           -0.099251             0.655101            -0.510542  
#>           cat2South             cat2West             cat1Beta  
#>           -0.062337            -0.070097             0.003442  
#>           cat1Gamma   cat2North:cat1Beta   cat2South:cat1Beta  
#>            0.026172            -0.011252            -0.475115  
#>   cat2West:cat1Beta  cat2North:cat1Gamma  cat2South:cat1Gamma  
#>            0.140028             0.136924             0.564488  
#>  cat2West:cat1Gamma  
#>            0.246710  
#> 
```
