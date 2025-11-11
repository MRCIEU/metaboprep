# continuous trait power analysis plot

This function (1) identifies an informative distribution of effect and
power estimates given your datas total sample size and (2) returns a
summary plot.

## Usage

``` r
continuous_power_plot(mydata)
```

## Arguments

- mydata:

  Your metabolite data matrix, with samples in rows

## Value

a ggplot2 object

## Examples

``` r
ex_data = matrix(NA, 1000, 2)
continuous_power_plot( ex_data )
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the metaboprep package.
#>   Please report the issue to the authors.

```
