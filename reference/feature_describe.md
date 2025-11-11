# Summary Statistics for Features

This function allows you to 'describe' metabolite features using the
describe() function from the psych package, as well as estimate
variance, a dispersion index, the coeficent of variation, and shapiro's
W-statistic.

## Usage

``` r
feature_describe(data)
```

## Arguments

- data:

  matrix, the metabolite data matrix. Samples in row, metabolites in
  columns

## Value

a data frame of summary statistics for features (columns) of a matrix
