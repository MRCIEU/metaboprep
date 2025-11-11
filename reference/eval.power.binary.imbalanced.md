# Estimate power for a binary variable in an imbalanced design

This function allows you estimate power for a binary variable given a
defined number of case samples, control samples, effect size, and
significance threshold.

## Usage

``` r
eval.power.binary.imbalanced(N_case, N_control, effect, alpha)
```

## Arguments

- N_case:

  a numeric vector of sample size of cases

- N_control:

  a numeric vector of sample size of controls

- effect:

  a numeric vector of effect size

- alpha:

  a numeric vector of significance thresholds

## Value

a matrix of paramater inputs and power estimates are returned as a
matrix

## Examples

``` r
eval.power.binary.imbalanced( N_case = 1000,
 N_control = 1000,
 effect = 0.01,
 alpha = 0.05 )
#>         N N_case N_control effect alpha power
#> [1,] 2000   1000      1000   0.01  0.05 0.056

eval.power.binary.imbalanced( N_case = c(1000, 2000),
 N_control = c(1000, 2000),
 effect = 0.01,
 alpha = 0.05 )
#>         N N_case N_control effect alpha power
#> [1,] 2000   1000      1000   0.01  0.05 0.056
#> [2,] 4000   2000      2000   0.01  0.05 0.062

```
