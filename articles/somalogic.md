# Import Somalogic Proteomic Data

## Import SomaLogic data

Read in the SomaLogic data using the `read_somalogic` function. Here we
will read in the example data provided with the package, as a list
object.

``` r
library(metaboprep)

# example file
filepath <- system.file("extdata", "somalogic_v1_example.adat", package = "metaboprep")

# import
dat <- read_somalogic(filepath,
                      return_Metaboprep = FALSE)

# create the object (down-sampled for speed)
m <- Metaboprep(data     = dat$data[1:9, 1:100], 
                samples  = dat$samples[1:9, ], 
                features = dat$features[1:100, ],
                )
```

## Quick look to identify the types of data imported

Note that we would advocate that you read in olink data as a list object
first, as it will often have additional information that you may wish to
inspect prior to creating the `Metaboprep` object.

``` r
names(dat)
#> [1] "data"             "samples"          "features"         "controls"        
#> [5] "control_metadata"
```

## Create Metaboprep object

Once imported, we pass the data to the Metaboprep() function to build
the `Metaboprep` class object.

``` r
mydata <- Metaboprep(data     = dat$data[1:9, 1:100], 
                     samples  = dat$samples[1:9, ], 
                     features = dat$features[1:100, ]
                     )
```

## Quick summary of the metaboprep object

``` r
summary(mydata)
#> Metaboprep Object Summary
#> --------------------------
#> Samples      : 9
#> Features     : 100
#> Data Layers  : 1
#> Layer Names  : input
#> 
#> Sample Summary Layers : none
#> Feature Summary Layers: none
#> 
#> Sample Annotation (metadata):
#>   Columns: 34
#>   Names  : sample_id, PlateId, PlateRunDate, ScannerID, PlatePosition, SlideId, Subarray, SampleType, PercentDilution, SampleMatrix, Barcode, Barcode2d, SampleName, SampleNotes, AliquotingNotes, SampleDescription, AssayNotes, TimePoint, ExtIdentifier, SsfExtId, SampleGroup, SiteId, TubeUniqueID, CLI, HybControlNormScale, RowCheck, NormScale_20, NormScale_0_005, NormScale_0_5, ANMLFractionUsed_20, ANMLFractionUsed_0_005, ANMLFractionUsed_0_5, Age, Sex
#> 
#> Feature Annotation (metadata):
#>   Columns: 22
#>   Names  : feature_id, SeqId, SeqIdVersion, SomaId, TargetFullName, Target, UniProt, EntrezGeneID, EntrezGeneSymbol, Organism, Units, Type, Dilution, PlateScale_Reference, CalReference, Cal_Example_Adat_Set001, ColCheck, CalQcRatio_Example_Adat_Set001_170255, QcReference_170255, Cal_Example_Adat_Set002, CalQcRatio_Example_Adat_Set002_170255, Dilution2
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

## QC Olink data

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
#> ✔ Validating input parameters [10ms]
#> 
#> ℹ Sample & Feature Summary Statistics for raw data
#> ✔ Sample & Feature Summary Statistics for raw data [466ms]
#> 
#> ℹ Copying input data to new 'qc' data layer
#> ✔ Copying input data to new 'qc' data layer [31ms]
#> 
#> ℹ Assessing for extreme sample missingness >=80% - excluding 0 sample(s)
#> ✔ Assessing for extreme sample missingness >=80% - excluding 0 sample(s) [15ms]
#> 
#> ℹ Assessing for extreme feature missingness >=80% - excluding 0 feature(s)
#> ✔ Assessing for extreme feature missingness >=80% - excluding 0 feature(s) [16m…
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
#> ✔ Running sample data PCA outlier analysis at +/- 5 Sdev [16ms]
#> 
#> ℹ Sample PCA outlier analysis - re-identify feature independence and PC outlier…
#> ! The stated max PCs [max_num_pcs=10] to use in PCA outlier assessment is greater than the number of available informative PCs [2]
#> ℹ Sample PCA outlier analysis - re-identify feature independence and PC outlier…✔ Sample PCA outlier analysis - re-identify feature independence and PC outlier…
#> 
#> ℹ Creating final QC dataset...
#> ✔ Creating final QC dataset... [404ms]
#> 
#> ℹ Metabolite QC Process Completed
#> ✔ Metabolite QC Process Completed [14ms]
```

## Quick summary of the metaboprep object following QC

``` r
summary(mydata)
#> Metaboprep Object Summary
#> --------------------------
#> Samples      : 9
#> Features     : 100
#> Data Layers  : 2
#> Layer Names  : input, qc
#> 
#> Sample Summary Layers : input, qc
#> Feature Summary Layers: input, qc
#> 
#> Sample Annotation (metadata):
#>   Columns: 36
#>   Names  : sample_id, PlateId, PlateRunDate, ScannerID, PlatePosition, SlideId, Subarray, SampleType, PercentDilution, SampleMatrix, Barcode, Barcode2d, SampleName, SampleNotes, AliquotingNotes, SampleDescription, AssayNotes, TimePoint, ExtIdentifier, SsfExtId, SampleGroup, SiteId, TubeUniqueID, CLI, HybControlNormScale, RowCheck, NormScale_20, NormScale_0_005, NormScale_0_5, ANMLFractionUsed_20, ANMLFractionUsed_0_005, ANMLFractionUsed_0_5, Age, Sex, reason_excluded, excluded
#> 
#> Feature Annotation (metadata):
#>   Columns: 24
#>   Names  : feature_id, SeqId, SeqIdVersion, SomaId, TargetFullName, Target, UniProt, EntrezGeneID, EntrezGeneSymbol, Organism, Units, Type, Dilution, PlateScale_Reference, CalReference, Cal_Example_Adat_Set001, ColCheck, CalQcRatio_Example_Adat_Set001_170255, QcReference_170255, Cal_Example_Adat_Set002, CalQcRatio_Example_Adat_Set002_170255, Dilution2, reason_excluded, excluded
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
