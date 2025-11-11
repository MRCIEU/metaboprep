# Metaboprep Object

A \`Metaboprep\` object is a container for matrices of metabolite data,
along with associated metadata. It allows for efficient storage and
manipulation of data, supporting quality control, transformations, and
various analyses. This object facilitates easy access to data layers,
sample and feature summaries, outlier treatment, and more.

## Usage

``` r
Metaboprep(
  data,
  samples,
  features,
  exclusions = list(samples = list(user_excluded = character(),
    extreme_sample_missingness = character(), user_defined_sample_missingness =
    character(), user_defined_sample_totalpeakarea = character(),
    user_defined_sample_pca_outlier = character()), features = list(user_excluded =
    character(), extreme_feature_missingness = character(),
    user_defined_feature_missingness = character())),
  feature_summary = array(data = NA_real_, dim = c(0, 0, 0)),
  sample_summary = array(data = NA_real_, dim = c(0, 0, 0))
)
```

## Arguments

- data:

  numeric matrix, the data matrix containing metabolite values (not to
  be set directly).

- samples:

  data.frame, a data frame containing sample-related information (not to
  be set directly).

- features:

  data.frame, a data frame containing feature-related information (not
  to be set directly).

- exclusions:

  list, holds exclusion codes for data masking (not to be set directly).

- feature_summary:

  numeric matrix, summary statistics for features (not to be set
  directly).

- sample_summary:

  numeric matrix, summary statistics for samples (not to be set
  directly).

## Value

An object of class Metaboprep, an S7 class.

## Slots

- `data`:

  numeric matrix, the metabolite data.

- `samples`:

  data.frame, the samples data frame

- `features`:

  data.frame, the features data frame

- `exclusions`:

  list, exclusion codes (mask for data).

- `feature_summary`:

  numeric matrix, feature summary statistics.

- `sample_summary`:

  numeric matrix, sample summary statistics.
