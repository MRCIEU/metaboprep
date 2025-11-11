# Identify Outliers

This function identifies outliers from a vector of data at SD units from
the mean.

## Usage

``` r
outliers(x, nsd = 3)
```

## Arguments

- x:

  a numerical vector of data

- nsd:

  the number of SD units from the mean to be used as an outlier cutoff.

## Value

a list object of length three. (1) a vector of sample indexes indicating
the outliers, (2) the lower outlier cuttoff value, (3) the upper outlier
cuttoff value.

## Examples

``` r
ex_data = rnbinom(500, mu = 40, size = 5)
outliers(ex_data)
#> [[1]]
#> [1] 125 409 455
#> 
#> [[2]]
#> [1] -16.56037
#> 
#> [[3]]
#> [1] 95.07637
#> 
```
