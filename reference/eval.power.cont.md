# estimate power for continuous variable

This function estimates power for a continuous variable given the sample
size, effect size, significance threshold, and the degrees of freedom.

## Usage

``` r
eval.power.cont(N, n_coeff, effect, alpha)
```

## Arguments

- N:

  Sample size

- n_coeff:

  degrees of freedom for numerator

- effect:

  effect size

- alpha:

  significance level (Type 1 error)

## Examples

``` r
eval.power.cont(N = 1000, n_coeff = 1, effect = 0.0025, alpha = 0.05)
#>         N effect alpha power n_coeff
#> [1,] 1000 0.0025  0.05 0.352       1
```
