# Identify indexes of outliers in data

Given a vector or matrix, this function returns a vector or matrix of
0\|1, of the same structure with 1 values indicating outliers.

## Usage

``` r
outlier_detection(data, nsd = 5, meansd = FALSE, by = "column")
```

## Arguments

- data:

  a matrix of numerical values, samples in row, features in columns

- nsd:

  the unit distance in SD or IQR from the mean or median estimate,
  respectively outliers are identified at. Default value is 5.

- meansd:

  set to TRUE if you would like to estimate outliers using a mean and SD
  method; set to FALSE if you would like to estimate medians and inter
  quartile ranges. The default is FALSE.

- by:

  character, either 'column' to compute along columns or 'row' to
  compute across rows. Irrelevant for vectors.

## Value

a matrix of 0 (not a sample outlier) and 1 (outlier)
