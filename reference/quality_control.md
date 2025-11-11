# Metabolite Quality Control

This function is a wrapper function that performs the key quality
controls steps on a metabolomics data set. Key principles: 1. keep the
source underlying data as it is 2. copy the source data to a new data
layer called qcing for processing 3. build an exclusion list,
accumulating codes for exclusion reasons 4. make any adjustments needed
in the destination copy of the data, flag these in the exclusion list 5.
copy the final result to a data layer called post_qc 6. return the
Metabolites object with the newly populated data layers

## Usage

``` r
quality_control(
  metaboprep,
  source_layer = "input",
  sample_missingness = 0.2,
  feature_missingness = 0.2,
  total_peak_area_sd = 5,
  outlier_udist = 5,
  outlier_treatment = "leave_be",
  winsorize_quantile = 1,
  tree_cut_height = 0.5,
  feature_selection = "max_var_exp",
  pc_outlier_sd = 5,
  max_num_pcs = 10,
  sample_ids = NULL,
  feature_ids = NULL,
  features_exclude_but_keep = NULL
)
```

## Arguments

- metaboprep:

  an object of class Metabolites

- source_layer:

  character, the data layer to summarise

- sample_missingness:

  numeric 0-1, percentage of data missingness which should prompt
  exclusion of a sample

- feature_missingness:

  numeric 0-1, percentage of data missingness which should prompt
  exclusion of a feature

- total_peak_area_sd:

  numeric, number of TPA SD after which a sample would be excluded

- outlier_udist:

  the unit distance in SD or IQR from the mean or median estimate,
  respectively outliers are identified at. Default value is 5.

- outlier_treatment:

  character, how to handle outlier data values - options 'leave_be',
  'turn_NA', or 'winsorize'

- winsorize_quantile:

  numeric, quantile to winsorize to, only relevant if
  'outlier_treatment'='winsorize'

- tree_cut_height:

  numeric, the threshold for feature independence in hierarchical
  clustering. Default is 0.5.

- feature_selection:

  character, either 'max_var_exp' or 'least_missingness', how to select
  the independent feature within clusters

- pc_outlier_sd:

  numeric, number of PCA SD after which a sample would be excluded

- max_num_pcs:

  numeric, the maximum number of PCs to use (look in) when filtering
  samples on PC outlier SD, default=10, set to NULL to use all
  informative PCs from the Scree analysis

- sample_ids:

  character, vector of sample ids to retain and work with, all others
  samples will be excluded

- feature_ids:

  character, vector of feature ids to retain and work with, all other
  features will be excluded

- features_exclude_but_keep:

  character, vector of feature ids indicating features to exclude from
  the sample and PCA quality control analysis but keep in the data, OR a
  name of a logical column in the features data indicating the same
