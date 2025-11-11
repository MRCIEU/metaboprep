# Import Nightingale Metabolomic Data

## Import Nightingale data directly into a Metaboprep object

Read in the Nightingale Health data using the `read_nightingale`
function. Here we will read in the example data provided with the
package, and convert it directly into a Metaboprep S7 object.

``` r
library(metaboprep)

# example file
filepath <- system.file("extdata", "nightingale_v2_example.xlsx", package = "metaboprep")

# import
mydata <- read_nightingale(filepath, return_Metaboprep = TRUE)
```

## Quick look at data structure of the imported data

``` r
str(mydata)
#> <metaboprep::Metaboprep>
#>  @ data           : num [1:50, 1:15, 1] 2.84e-10 3.86e-11 9.59e-11 2.21e-10 1.24e-10 ...
#>  .. - attr(*, "dimnames")=List of 3
#>  ..  ..$ : chr [1:50] "ind1" "ind2" "ind3" "ind4" ...
#>  ..  ..$ : chr [1:15] "XXL-VLDL-P" "XXL-VLDL-L" "XXL-VLDL-PL" "XXL-VLDL-C" ...
#>  ..  ..$ : chr "input"
#>  @ samples        :'data.frame': 50 obs. of  5 variables:
#>  .. $ sample_id                    : chr  "ind1" "ind2" "ind3" "ind4" ...
#>  .. $ high_pyruvate                : int  0 0 0 0 0 0 0 0 0 0 ...
#>  .. $ high_lactate                 : int  0 0 0 0 0 0 0 1 0 0 ...
#>  .. $ low_glutamine__high_glutamate: int  0 0 0 0 0 0 0 0 0 0 ...
#>  .. $ plasma_sample                : int  0 0 0 0 0 0 0 0 0 0 ...
#>  @ features       :'data.frame': 15 obs. of  1 variable:
#>  .. $ feature_id: chr  "XXL-VLDL-P" "XXL-VLDL-L" "XXL-VLDL-PL" "XXL-VLDL-C" ...
#>  @ exclusions     :List of 2
#>  .. $ samples :List of 5
#>  ..  ..$ user_excluded                    : chr(0) 
#>  ..  ..$ extreme_sample_missingness       : chr(0) 
#>  ..  ..$ user_defined_sample_missingness  : chr(0) 
#>  ..  ..$ user_defined_sample_totalpeakarea: chr(0) 
#>  ..  ..$ user_defined_sample_pca_outlier  : chr(0) 
#>  .. $ features:List of 3
#>  ..  ..$ user_excluded                   : chr(0) 
#>  ..  ..$ extreme_feature_missingness     : chr(0) 
#>  ..  ..$ user_defined_feature_missingness: chr(0) 
#>  @ feature_summary: num[0 , 0 , 0 ] 
#>  @ sample_summary : num[0 , 0 , 0 ]
```

## QC Nightingale

Perform the QC steps using the `quality_control` function.

``` r
mydata <- mydata |>
  quality_control(source_layer        = "input", 
                  sample_missingness  = 0.2, 
                  feature_missingness = 0.2, 
                  total_peak_area_sd  = 5, 
                  outlier_udist       = 5, 
                  outlier_treatment   = "leave_be", 
                  winsorize_quantile  = 1.0, 
                  tree_cut_height     = 0.5, 
                  pc_outlier_sd       = 5, 
                  feature_selection   = "max_var_exp",
                  features_exclude_but_keep = NULL
                  )
#> 
#> ── Starting Metabolite QC Process ──────────────────────────────────────────────
#> ℹ Validating input parameters
#> ✔ Validating input parameters [8ms]
#> 
#> ℹ Sample & Feature Summary Statistics for raw data
#> ✔ Sample & Feature Summary Statistics for raw data [195ms]
#> 
#> ℹ Copying input data to new 'qc' data layer
#> ✔ Copying input data to new 'qc' data layer [22ms]
#> 
#> ℹ Assessing for extreme sample missingness >=80% - excluding 0 sample(s)
#> ✔ Assessing for extreme sample missingness >=80% - excluding 0 sample(s) [16ms]
#> 
#> ℹ Assessing for extreme feature missingness >=80% - excluding 0 feature(s)
#> ✔ Assessing for extreme feature missingness >=80% - excluding 0 feature(s) [20m…
#> 
#> ℹ Assessing for sample missingness at specified level of >=20% - excluding 0 sa…
#> ✔ Assessing for sample missingness at specified level of >=20% - excluding 0 sa…
#> 
#> ℹ Assessing for feature missingness at specified level of >=20% - excluding 0 f…
#> ✔ Assessing for feature missingness at specified level of >=20% - excluding 0 f…
#> 
#> ℹ Calculating total peak abundance outliers at +/- 5 Sdev - excluding 0 sample(…
#> ✔ Calculating total peak abundance outliers at +/- 5 Sdev - excluding 0 sample(…
#> 
#> ℹ Running sample data PCA outlier analysis at +/- 5 Sdev
#> ✔ Running sample data PCA outlier analysis at +/- 5 Sdev [21ms]
#> 
#> ℹ Sample PCA outlier analysis - re-identify feature independence and PC outlier…
#> ! The stated max PCs [max_num_pcs=10] to use in PCA outlier assessment is greater than the number of available informative PCs [7]
#> ℹ Sample PCA outlier analysis - re-identify feature independence and PC outlier…✔ Sample PCA outlier analysis - re-identify feature independence and PC outlier…
#> 
#> ℹ Creating final QC dataset...
#> ✔ Creating final QC dataset... [113ms]
#> 
#> ℹ Metabolite QC Process Completed
#> ✔ Metabolite QC Process Completed [23ms]
```

## Quick summary of the metaboprep object following QC

``` r
summary(mydata)
#> Metaboprep Object Summary
#> --------------------------
#> Samples      : 50
#> Features     : 15
#> Data Layers  : 2
#> Layer Names  : input, qc
#> 
#> Sample Summary Layers : input, qc
#> Feature Summary Layers: input, qc
#> 
#> Sample Annotation (metadata):
#>   Columns: 7
#>   Names  : sample_id, high_pyruvate, high_lactate, low_glutamine__high_glutamate, plasma_sample, reason_excluded, excluded
#> 
#> Feature Annotation (metadata):
#>   Columns: 3
#>   Names  : feature_id, reason_excluded, excluded
#> 
#> Exclusion Codes Summary:
#> 
#>   Sample Exclusions:
#> Exclusion | Count
#> -----------------
#> user_excluded                     | 0
#> extreme_sample_missingness        | 0
#> user_defined_sample_missingness   | 0
#> user_defined_sample_totalpeakarea | 0
#> user_defined_sample_pca_outlier   | 0
#> 
#>   Feature Exclusions:
#> Exclusion | Count
#> -----------------
#> user_excluded                    | 0
#> extreme_feature_missingness      | 0
#> user_defined_feature_missingness | 0
```
