# Print a `hierarchial_lm` Class Object

Print the content of a 'hierarchical_lm\`-class object.

## Usage

``` r
# S3 method for class 'hierarchical_lm'
print(
  x,
  digits = 4,
  signif.stars = getOption("show.signif.stars"),
  eps.Pvalue = 0.001,
  ...
)
```

## Arguments

- x:

  A `hierarchical_lm`-class object, usually the output of
  [`hierarchical_lm()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical_lm.md).

- digits:

  The minimum number of significant digits to be used for most numbers.
  To be used by the print method of `anova`-class objects.

- signif.stars:

  Logical. To be used by the print method of `anova`-class objects.

- eps.Pvalue:

  To be passed to
  [`format.pval()`](https://rdrr.io/r/base/format.pval.html). It
  controls how small *p*-values are displayed. Default is `.001`. That
  is, *p*-values less than `.001` will be displayed as `<.001`.

- ...:

  Optional arguments. To be passed to the print method of `anova`-class
  objects.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

The printout is very similar to that of the print method of an `anova`
object. It simply overrides the default values for some arguments,
notably `esp.Pvalue` to prevent small *p*-values to be presented in
scientific notation.

## See also

[`hierarchical_lm()`](https://sfcheung.github.io/lmhelprs/reference/hierarchical_lm.md)

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
```
