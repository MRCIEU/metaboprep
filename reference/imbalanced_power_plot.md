# binary trait imbalanced design power analysis plot

This function (1) estimates an informative distribution of effect and
power estimates given your datas total sample size, over a distribution
of imbalanced sample sizes and (2) returns a summary plot.

## Usage

``` r
imbalanced_power_plot(mydata)
```

## Arguments

- mydata:

  a numeric data matrix with samples in rows and features in columns

## Value

a ggplot2 object

## Examples

``` r
ex_data = matrix(NA, 1000, 2)
imbalanced_power_plot( ex_data )

```
