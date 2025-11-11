# Batch Normalisation

Run batch normalisation based on the platform flag in the features data

## Usage

``` r
batch_normalise(
  metaboprep,
  run_mode_col,
  run_mode_colmap,
  source_layer = "input",
  dest_layer = "batch_normalised"
)
```

## Arguments

- metaboprep:

  an object of class Metaboprep

- run_mode_col:

  character, column name in features data containing the run mode

- run_mode_colmap:

  named character vector or list, c(mode = "mode col name in samples")

- source_layer:

  character, which data layer to get the data from

- dest_layer:

  character, which data layer to put the the data in to
