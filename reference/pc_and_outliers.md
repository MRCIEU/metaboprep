# Principal Component Analysis

This function performs principal component analysis. In the first,
missing data is imputed to the median. Subsequent to the derivation of
the PC, the median imputed PC data is used to identify the number of
informative or "significant" PC by (1) an acceleration analysis, and (2)
a parrallel analysis. Finally the number of sample outliers are
determined at 3, 4, and 5 standard deviations from the mean on the top
PCs as determined by the acceleration factor analysis.

## Usage

``` r
pc_and_outliers(
  metaboprep,
  source_layer = "input",
  sample_ids = NULL,
  feature_ids = NULL
)
```

## Arguments

- metaboprep:

  an object of class Metaboprep

- source_layer:

  character, type/source of data to use

- sample_ids:

  character, vector of sample ids to include, default NULL includes all

- feature_ids:

  character, vector of feature ids to include, default NULL includes all

## Value

a data.frame
