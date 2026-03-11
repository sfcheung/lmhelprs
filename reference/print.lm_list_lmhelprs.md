# Print an `lm_list_lmhelprs`-Class Object

Print the content of the output of
[`many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.md).

## Usage

``` r
# S3 method for class 'lm_list_lmhelprs'
print(x, ...)
```

## Arguments

- x:

  The output of
  [`many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.md).

- ...:

  Other arguments. Not used.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

Adapted from the package `manymome` such that
[`many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.md)
can be used with `manymome`.

## Examples

``` r
data(data_test1)
mod <- "x3 ~ x2 + x1
        x4 ~ x3
        x5 ~ x4*x1"
out <- many_lm(mod, data_test1)
out
#> 
#> The models:
#> x3 ~ x2 + x1
#> <environment: 0x562c1ef47c38>
#> x4 ~ x3
#> <environment: 0x562c1ef47c38>
#> x5 ~ x4 * x1
#> <environment: 0x562c1ef47c38>
#> 
```
