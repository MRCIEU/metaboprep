# metaboprep

Metabolomics & proteomics data preparation and quality control pipeline
for R

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

## Overview

`metaboprep` supports the full data-preparation workflow for untargeted
and targeted omics data:

1.  **Import** raw data from Metabolon, Nightingale Health, Olink, and
    SomaLogic platforms (Excel / flat-text)  
2.  **Summarise** sample- and feature-level statistics  
3.  **Filter** using a standard QC pipeline with user-defined
    thresholds  
4.  **Report** results as an interactive HTML or PDF document  
5.  **Export** cleaned data for downstream analysis

## Installation

``` r

# install.packages("pak")
pak::pak("MRCIEU/metaboprep")
```

## Quick start

``` r

library(metaboprep)

# 1. Read data
mydata <- read_metabolon(
  system.file("extdata", "metabolon_v1.1_example.xlsx", package = "metaboprep"),
  sheet             = "OrigScale",
  return_Metaboprep = TRUE
)

# 2. Run QC pipeline
mydata <- mydata |> quality_control(
  source_layer        = "input",
  sample_missingness  = 0.2,
  feature_missingness = 0.2,
  total_peak_area_sd  = 5,
  outlier_udist       = 5,
  outlier_treatment   = "leave_be"
)

# 3. Summarise
summary(mydata)

# 4. Generate HTML report
generate_report(mydata, output_dir = ".")
```

------------------------------------------------------------------------

## Articles

### Importing Data

##### [Metabolon](https://mrcieu.github.io/metaboprep/articles/metabolon.md)

Import untargeted metabolomics data from Metabolon Excel sheets.

[Read →](https://mrcieu.github.io/metaboprep/articles/metabolon.md)

##### [Nightingale Health](https://mrcieu.github.io/metaboprep/articles/nightingale.md)

Import NMR-based metabolomic data from Nightingale Health.

[Read →](https://mrcieu.github.io/metaboprep/articles/nightingale.md)

##### [Olink](https://mrcieu.github.io/metaboprep/articles/olink.md)

Import proximity extension assay proteomic data from Olink.

[Read →](https://mrcieu.github.io/metaboprep/articles/olink.md)

##### [SomaLogic](https://mrcieu.github.io/metaboprep/articles/somalogic.md)

Import aptamer-based proteomic data from SomaLogic SomaScan.

[Read →](https://mrcieu.github.io/metaboprep/articles/somalogic.md)

### Summaries & QC

##### [Sample Summary](https://mrcieu.github.io/metaboprep/articles/sample_summary.md)

Compute per-sample statistics: missingness, total peak area, and
PCA-based outlier detection.

[Read →](https://mrcieu.github.io/metaboprep/articles/sample_summary.md)

##### [Feature Summary](https://mrcieu.github.io/metaboprep/articles/feature_summary.md)

Compute per-feature statistics: missingness, variance, and independent
feature trees.

[Read
→](https://mrcieu.github.io/metaboprep/articles/feature_summary.md)

##### [QC Pipeline](https://mrcieu.github.io/metaboprep/articles/quality_control.md)

Run the full quality control pipeline with configurable thresholds for
missingness, outliers, and more.

[Read
→](https://mrcieu.github.io/metaboprep/articles/quality_control.md)

### Reports & Export

##### [Generate HTML / PDF Report](https://mrcieu.github.io/metaboprep/articles/generate_report.md)

Produce a fully annotated, interactive QC report in HTML or PDF format.

[Read
→](https://mrcieu.github.io/metaboprep/articles/generate_report.md)

##### [Export Data](https://mrcieu.github.io/metaboprep/articles/export.md)

Export processed data and summary tables to Excel or tab-delimited flat
files.

[Read →](https://mrcieu.github.io/metaboprep/articles/export.md)

##### [Batch Normalisation](https://mrcieu.github.io/metaboprep/articles/batch_normalise.md)

Correct for run-order and batch effects using quantile or rank-based
normalisation.

[Read
→](https://mrcieu.github.io/metaboprep/articles/batch_normalise.md)
