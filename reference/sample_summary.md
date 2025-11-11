# Sample Summary Statistics

Summarise the sample data

## Usage

``` r
sample_summary(
  metaboprep,
  source_layer = "input",
  outlier_udist = 5,
  sample_ids = NULL,
  feature_ids = NULL,
  output = "data.frame"
)
```

## Arguments

- metaboprep:

  an object of class Metaboprep

- source_layer:

  character, the data layer to summarise

- outlier_udist:

  the unit distance in SD or IQR from the mean or median estimate,
  respectively outliers are identified at. Default value is 5.

- sample_ids:

  character, vector of sample ids to work with

- feature_ids:

  character, vector of feature ids to work with

- output:

  character, type of output, either 'object' to return the updated
  metaboprep object, or 'data.frame' to return the data.
