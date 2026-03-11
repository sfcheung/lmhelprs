# Summary of an `lm_list_lmhelprs`-Class Object

The summary of content of the output of
[`many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.md).

## Usage

``` r
# S3 method for class 'lm_list_lmhelprs'
summary(object, ...)

# S3 method for class 'summary_lm_list_lmhelprs'
print(x, digits = 3, ...)
```

## Arguments

- object:

  The output of
  [`many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.md).

- ...:

  Other arguments. Not used.

- x:

  An object of class `summary_lm_list_lmhelprs`.

- digits:

  The number of significant digits in printing numerical results.

## Value

`summary.lm_list_lmhelprs()` returns a `summary_lm_list_lmhelprs`-class
object, which is a list of the
[`summary()`](https://rdrr.io/r/base/summary.html) outputs of the
[`lm()`](https://rdrr.io/r/stats/lm.html) outputs stored.

`print.summary_lm_list_lmhelprs()` returns `x` invisibly. Called for its
side effect.

Adapted from the package `manymome` such that
[`many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.md)
can be used without `manymome`.

## Functions

- `print(summary_lm_list_lmhelprs)`: Print method for output of summary
  for lm_list_lmhelprs.

## Examples

``` r
data(data_test1)
mod <- "x3 ~ x2 + x1
        x4 ~ x3
        x5 ~ x4*x1"
out <- many_lm(mod, data_test1)
summary(out)
#> Call:
#> many_lm(models = mod, data = data_test1)
#> 
#> Model:
#> x3 ~ x2 + x1
#> <environment: 0x562c21caa1c8>
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  -0.0835     0.0968   -0.86  0.39067    
#> x2           -0.0494     0.0896   -0.55  0.58289    
#> x1            0.3801     0.0955    3.98  0.00013 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> R-square = 0.143. Adjusted R-square = 0.125. F(2, 97) = 8.079, p < .001
#> 
#> Model:
#> x4 ~ x3
#> <environment: 0x562c21caa1c8>
#>             Estimate Std. Error t value Pr(>|t|)  
#> (Intercept)  -0.1144     0.0866   -1.32    0.190  
#> x3            0.2156     0.0846    2.55    0.012 *
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> R-square = 0.062. Adjusted R-square = 0.053. F(1, 98) = 6.489, p = 0.012
#> 
#> Model:
#> x5 ~ x4 * x1
#> <environment: 0x562c21caa1c8>
#>             Estimate Std. Error t value Pr(>|t|)   
#> (Intercept) -0.10644    0.10399   -1.02   0.3086   
#> x4           0.15124    0.11589    1.31   0.1950   
#> x1           0.27872    0.09885    2.82   0.0058 **
#> x4:x1        0.00945    0.12017    0.08   0.9375   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> R-square = 0.108. Adjusted R-square = 0.080. F(3, 96) = 3.878, p = 0.012
```
