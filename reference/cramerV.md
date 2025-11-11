# Cramer's V (phi)

Calculates Cramer's V for a table of nominal variables; confidence
intervals by bootstrap. Function taken from the rcompanion Rpackage.

## Usage

``` r
cramerV(
  x,
  y = NULL,
  ci = FALSE,
  conf = 0.95,
  type = "perc",
  R = 1000,
  digits = 4,
  bias.correct = FALSE,
  reportIncomplete = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  Either a two-way table or a two-way matrix. Can also be a vector of
  observations for one dimension of a two-way table.

- y:

  If `x` is a vector, `y` is the vector of observations for the second
  dimension of a two-way table.

- ci:

  If `TRUE`, returns confidence intervals by bootstrap. May be slow.

- conf:

  The level for the confidence interval.

- type:

  The type of confidence interval to use. Can be any of "`norm`",
  "`basic`", "`perc`", or "`bca`". Passed to `boot.ci`.

- R:

  The number of replications to use for bootstrap.

- digits:

  The number of significant digits in the output.

- bias.correct:

  If `TRUE`, a bias correction is applied.

- reportIncomplete:

  If `FALSE` (the default), `NA` will be reported in cases where there
  are instances of the calculation of the statistic failing during the
  bootstrap procedure.

- verbose:

  If `TRUE`, prints additional statistics.

- ...:

  Additional arguments passed to `chisq.test`.

## Value

A single statistic, Cramer's V. Or a small data frame consisting of
Cramer's V, and the lower and upper confidence limits.

## Details

Cramer's V is used as a measure of association between two nominal
variables, or as an effect size for a chi-square test of association.
For a 2 x 2 table, the absolute value of the phi statistic is the same
as Cramer's V.

Because V is always positive, if `type="perc"`, the confidence interval
will never cross zero. In this case, the confidence interval range
should not be used for statistical inference. However, if `type="norm"`,
the confidence interval may cross zero.

When V is close to 0 or very large, or with small counts, the confidence
intervals determined by this method may not be reliable, or the
procedure may fail.

## References

[http://rcompanion.org/handbook/H_10.html](http://rcompanion.org/handbook/H_10.md)

## Author

Salvatore Mangiafico, <mangiafico@njaes.rutgers.edu>
