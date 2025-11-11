# identify effect sizes

This function estimates an appropriate distribution of effect sizes to
simulate in a power analysis.

## Usage

``` r
find.PA.effect.sizes.2.sim(mydata)
```

## Arguments

- mydata:

  Your metabolite data matrix, with samples in rows

## Value

a vector of effect sizes

## Examples

``` r
ex_data = sapply(1:10, function(x){ rnorm(250, 40, 5) })
find.PA.effect.sizes.2.sim(ex_data)
#>  [1] 0.1401 0.1883 0.2365 0.2846 0.3328 0.3810 0.4292 0.4774 0.5255 0.5737
#> [11] 0.6219
```
