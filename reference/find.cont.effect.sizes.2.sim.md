# identify continuos trait effect sizes

This function estimates an appropriate distribution of effect sizes to
simulate in a continuous trait power analysis.

## Usage

``` r
find.cont.effect.sizes.2.sim(mydata)
```

## Arguments

- mydata:

  Your metabolite data matrix, with samples in rows

## Value

a vector of effect sizes

## Examples

``` r
ex_data = sapply(1:10, function(x){ rnorm(250, 40, 5) })
find.cont.effect.sizes.2.sim(ex_data)
#>  [1] 0.005001 0.014651 0.024301 0.033951 0.043601 0.053251 0.062901 0.072551
#>  [9] 0.082201 0.091851 0.101501
```
