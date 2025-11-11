# Estimate Missingness

This function estimates missingness in a matrix of data and provides an
option to exclude certain columns or features from the analysis, such as
xenobiotics (with high missingness rates) in metabolomics data sets.

## Usage

``` r
missingness(data, by = "row")
```

## Arguments

- data:

  matrix, a numeric matrix with samples in rows and features in columns

- by:

  character, whether to calculate missingness by rows (samples) or
  column (features)

## Value

data.frame, a data frame of missingness estimates for each
sample/feature.
