# Estimates total peak abundance

This function estimates total peak abundance\|area for numeric data in a
matrix, for (1) all features and (2) all features with complete data.

## Usage

``` r
total_peak_area(data, ztransform = TRUE)
```

## Arguments

- data:

  matrix, the metabolite data matrix. Samples in rows, metabolites in
  columns

- ztransform:

  logical, should the feature data be z-transformed and absolute value
  minimum, mean shifted prior to summing the feature values. TRUE or
  FALSE.

## Value

a data frame of estimates for (1) total peak abundance and (2) total
peak abundance at complete features for each samples
