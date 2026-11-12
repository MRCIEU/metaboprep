# Feature Summary Statistics

This function estimates feature statistics for samples in a matrix of
metabolite features.

## Usage

``` r
feature_summary(
  metaboprep,
  source_layer = "input",
  outlier_udist = 5,
  tree_cut_height = 0.5,
  feature_selection = "max_var_exp",
  sample_ids = NULL,
  feature_ids = NULL,
  features_exclude = NULL,
  output = "data.frame"
)
```

## Arguments

- metaboprep:

  an object of class Metabolites

- source_layer:

  character, the data layer to summarise

- outlier_udist:

  the unit distance in SD or IQR from the mean or median estimate,
  respectively outliers are identified at. Default value is 5.

- tree_cut_height:

  numeric, the threshold for feature independence in hierarchical
  clustering. Default is 0.5.

- feature_selection:

  character, either 'max_var_exp' or 'least_missingness', how to select
  the independent feature within clusters

- sample_ids:

  character, vector of sample ids to work with

- feature_ids:

  character, vector of feature ids to work with

- features_exclude:

  character, vector of feature id indicating features to exclude from
  the sample and PCA summary analysis but keep in the data

- output:

  character, type of output, either 'object' to return the updated
  metaboprep object, or 'data.frame' to return the data.
