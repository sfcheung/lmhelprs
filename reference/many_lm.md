# Fit Linear Models Defined By Model Syntax

Fit a list of linear models defined by model syntax.

## Usage

``` r
many_lm(models, data, na_omit_all = TRUE, ...)

# S3 method for class 'lm_list_lmhelprs'
getCall(x, ...)

# S3 method for class 'lm_list_lmhelprs'
update(object, ..., evaluate = TRUE)

# S3 method for class 'lm_list_lmhelprs'
coef(object, y = NULL, ...)

# S3 method for class 'lm_list_lmhelprs'
vcov(object, y = NULL, vcov_args = list(), ...)

# S3 method for class 'lm_list_lmhelprs'
confint(
  object,
  parm = NULL,
  level = 0.95,
  ci_fun = stats::confint,
  ci_args = list(),
  ...
)

# S3 method for class 'lm_list_lmhelprs'
nobs(object, ...)

# S3 method for class 'lm_list_lmhelprs'
model.frame(formula, ...)

# S3 method for class 'lm_list_lmhelprs'
variable.names(object, ...)
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

  Additional arguments. For `many_lm()`, these arguments will be passed
  to [`lm()`](https://rdrr.io/r/stats/lm.html). For the update method of
  the output of `many_lm()`, these arguments will used to update the
  call to `many_lm()`. For other methods, these arguments will be passed
  to methods if applicable.

- x:

  For the `getCall` method, this is the output of `many_lm()`.

- object:

  For methods of the output of `many_lm()`, it is the output of
  `many_lm()`.

- evaluate:

  If `TRUE`, the updated call will be evaluated. If `FALSE`, the updated
  call will be returned.

- y:

  For some methods, if set to a character vector, only the coefficients
  of the models of the response variables listed in `y` will be
  returned. If `NULL`, all coefficients will be returned.

- vcov_args:

  A named list of arguments to be passed to
  [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html) when computing
  the variance-covariance matrices of the regression coefficients.
  Default is [`list()`](https://rdrr.io/r/base/list.html), an empty
  list.

- parm:

  The parameters for which the confidence intervals will be returned. If
  `NULL`, the confidence intervals for all coefficients will be
  returned.

- level:

  The level of confidence of the confidence levels.

- ci_fun:

  The function to be used to form the confidence intervals for
  regression coefficients. Default is
  [`stats::confint`](https://rdrr.io/r/stats/confint.html)

- ci_args:

  A named list of arguments to be passed to `ci_fun`. Default is
  `list(level = .95)`, requesting 95% confidence intervals.

- formula:

  For the `model.frame` method, this should be the output of
  `many_lm()`.

## Value

A list of the output of [`lm()`](https://rdrr.io/r/stats/lm.html). The
class is `lm_list_lmhelprs`.

The `getCall` method returns a call stored in the output of `many_lm()`.

For the update method of the output of `many_lm()`, if `evaluate` is
`TRUE`, the updated output of `many_lm()` will be returned. If
`evaluate` is `FALSE`, the updated call will be returned.

The `coef` method returns a numeric vector of the coefficients.

The `vcov` method returns the variance-covariance matrix of the
parameter estimates of the models.

The `confint` method returns a two-column matrix of the confidence
intervals.

The `nobs` method returns the number of cases used in the models.

The `model.frame` method of the output of `many_lm()` returns a model
frame, formed by merging the model frames of all models.

The `variable.names` method returns a character vector of unique
variable names in all the stored models.

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

The `getCall` method of the output of `many_lm()` extracts the stored
call.

The output of `many_lm()` has a update method.

The `coef` method extracts the regression coefficients in the `lavaan`
style: `y ~ x`, `y` the response variable and `x` a term.

The `vcov` method is used to extract the variance-covariance matrices of
the models.

The `confint` method computes the confidence intervals for the
coefficients.

The `nobs` method extracts the number of cases actually used in the
analysis. Should be identical for all models.

Unlike the corresponding method for the output of
[`stats::lm()`](https://rdrr.io/r/stats/lm.html), the `model.frame`
method of the output of `many_lm()` does only one thing: extracts the
stored model frame. It is because `many_lm()` is for fitting several
models, with several formulas instead of only one.

The `variable.names` method of the output of `many_lm()` extract the
variable names of all models.

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
#> <environment: 0x55b6ed1a1280>
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
#> <environment: 0x55b6ed1a1280>
#>             Estimate Std. Error t value Pr(>|t|)  
#> (Intercept)  -0.1144     0.0866   -1.32    0.190  
#> x3            0.2156     0.0846    2.55    0.012 *
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> R-square = 0.062. Adjusted R-square = 0.053. F(1, 98) = 6.489, p = 0.012
#> 
#> Model:
#> x5 ~ x4 * x1
#> <environment: 0x55b6ed1a1280>
#>             Estimate Std. Error t value Pr(>|t|)   
#> (Intercept) -0.10644    0.10399   -1.02   0.3086   
#> x4           0.15124    0.11589    1.31   0.1950   
#> x1           0.27872    0.09885    2.82   0.0058 **
#> x4:x1        0.00945    0.12017    0.08   0.9375   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> R-square = 0.108. Adjusted R-square = 0.080. F(3, 96) = 3.878, p = 0.012



```
