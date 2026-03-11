# Fit Linear Models Defined By Model Syntax

Fit a list of linear models defined by model syntax.

## Usage

``` r
many_lm(models, data, na_omit_all = TRUE, ...)
```

## Arguments

- models:

  Character. Model syntax. See Details.

- data:

  The data frame. Must be supplied if `na_omit_all` is `TRUE`. If
  `na_omit_all` is `FALSE`, it can be omitted (though not suggested).

- na_omit_all:

  How missing data is handled across models. If `TRUE`, the default,
  then only cases with no missing data on all variables used at least
  one of the models will be retained (i.e., listwise deletion). If
  `FALSE`, then missing data will be handled in each model separately by
  [`lm()`](https://rdrr.io/r/stats/lm.html).

- ...:

  Additional arguments. To be passed to
  [`lm()`](https://rdrr.io/r/stats/lm.html).

## Value

A list of the output of [`lm()`](https://rdrr.io/r/stats/lm.html). The
class is `lm_list_lmhelprs`.

## Details

This function extracts linear model formulas from a model syntax (a
character vector), fits each of them by
[`lm()`](https://rdrr.io/r/stats/lm.html), and stores the results in a
list.

Lines with the first non-whitespace character `"#"` are treated as
comments and ignored.

Each line must be a valid formula for
[`lm()`](https://rdrr.io/r/stats/lm.html).

### Listwise deletion

If `na_omit_all` is `TRUE`, the default, then cases with missing data on
at least one of the variables used in the model will be removed. Each
call to [`lm()`](https://rdrr.io/r/stats/lm.html) will have `subset` set
to an integer vector of cases *not* removed (i.e., cases retained)

### Handling the `subset` argument

If `subset` is used when calling this function, it will also be used to
select cases.

Note that the `subset` argument in the call in each model will be
replaced by a numeric vector of cases retained, determined by both
missing data and the original value of the `subset`.

## See also

[`stats::lm()`](https://rdrr.io/r/stats/lm.html)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

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
#> <environment: 0x562c1812b590>
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
#> <environment: 0x562c1812b590>
#>             Estimate Std. Error t value Pr(>|t|)  
#> (Intercept)  -0.1144     0.0866   -1.32    0.190  
#> x3            0.2156     0.0846    2.55    0.012 *
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> R-square = 0.062. Adjusted R-square = 0.053. F(1, 98) = 6.489, p = 0.012
#> 
#> Model:
#> x5 ~ x4 * x1
#> <environment: 0x562c1812b590>
#>             Estimate Std. Error t value Pr(>|t|)   
#> (Intercept) -0.10644    0.10399   -1.02   0.3086   
#> x4           0.15124    0.11589    1.31   0.1950   
#> x1           0.27872    0.09885    2.82   0.0058 **
#> x4:x1        0.00945    0.12017    0.08   0.9375   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> R-square = 0.108. Adjusted R-square = 0.080. F(3, 96) = 3.878, p = 0.012



```
