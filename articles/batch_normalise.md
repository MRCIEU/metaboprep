# Batch Normalise

## Create Metaboprep object

``` r
library(metaboprep)

data     <- read.csv(system.file("extdata", "dummy_data.csv", package = "metaboprep"), header=T, row.names = 1) |> as.matrix()
samples  <- read.csv(system.file("extdata", "dummy_samples.csv", package = "metaboprep"), header=T, row.names = 1)
features <- read.csv(system.file("extdata", "dummy_features.csv", package = "metaboprep"), header=T, row.names = 1)

## create Metaboprep object
mydata <- Metaboprep(data = data, samples = samples, features = features)

## summary of Metaboprep object
summary(mydata)
#> Metaboprep Object Summary
#> --------------------------
#> Samples      : 100
#> Features     : 20
#> Data Layers  : 1
#> Layer Names  : input
#> 
#> Sample Summary Layers : none
#> Feature Summary Layers: none
#> 
#> Sample Annotation (metadata):
#>   Columns: 5
#>   Names  : sample_id, age, sex, pos, neg
#> 
#> Feature Annotation (metadata):
#>   Columns: 5
#>   Names  : feature_id, platform, pathway, derived_feature, xenobiotic_feature
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

## Run batch normalisation

``` r
mydata <- mydata |>
  batch_normalise(run_mode_col = "platform", run_mode_colmap = c(pos="pos", neg="neg")) |>
  print()
#> <metaboprep::Metaboprep>
#>  @ data           : num [1:100, 1:20, 1:2] 0.755887 0.662386 0.444527 0.627146 0.000465 ...
#>  .. - attr(*, "dimnames")=List of 3
#>  ..  ..$ : chr [1:100] "id_100" "id_99" "id_98" "id_97" ...
#>  ..  ..$ : chr [1:20] "metab_id_1" "metab_id_2" "metab_id_3" "metab_id_4" ...
#>  ..  ..$ : chr [1:2] "input" "batch_normalised"
#>  @ samples        :'data.frame': 100 obs. of  5 variables:
#>  .. $ sample_id: chr  "id_100" "id_99" "id_98" "id_97" ...
#>  .. $ age      : int  29 47 65 57 52 40 42 63 49 42 ...
#>  .. $ sex      : chr  "male" "male" "female" "female" ...
#>  .. $ pos      : chr  "batch2" "batch1" "batch2" "batch1" ...
#>  .. $ neg      : chr  "batch2" "batch2" "batch2" "batch1" ...
#>  @ features       :'data.frame': 20 obs. of  5 variables:
#>  .. $ feature_id        : chr  "metab_id_1" "metab_id_2" "metab_id_3" "metab_id_4" ...
#>  .. $ platform          : chr  "neg" "neg" "neg" "pos" ...
#>  .. $ pathway           : logi  NA NA NA NA NA NA ...
#>  .. $ derived_feature   : logi  TRUE FALSE FALSE FALSE FALSE FALSE ...
#>  .. $ xenobiotic_feature: logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
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

## Accessing data

### Raw input data

``` r
mydata@data[1:5, 1:5, "input"]
#>          metab_id_1 metab_id_2 metab_id_3 metab_id_4 metab_id_5
#> id_100 0.7558872597 0.42346365  0.2830023  0.8074991 0.52739494
#> id_99  0.6623855066 0.94958532  0.4204349  0.1280644 0.86098936
#> id_98  0.4445273969 0.70903790  0.5872781  0.2507785 0.67355499
#> id_97  0.6271461844 0.41330541  0.8066944  0.3315470 0.01304103
#> id_96  0.0004653491 0.01836408  0.2019952  0.4072693 0.69319890
```

### Batch normlalised data

``` r
mydata@data[1:5, 1:5, "batch_normalised"]
#>          metab_id_1 metab_id_2 metab_id_3 metab_id_4 metab_id_5
#> id_100 1.2219122932 0.85292184  0.5567470  2.0502351  0.9094521
#> id_99  1.0707641687 1.91261295  0.8271164  0.2650566  1.4847101
#> id_98  0.7185906152 1.42811293  1.1553450  0.6367251  1.1614939
#> id_97  1.3714905954 0.71888003  1.6124149  0.6862071  0.0243190
#> id_96  0.0007522494 0.03698813  0.3973827  0.8429306  1.1953683
```
