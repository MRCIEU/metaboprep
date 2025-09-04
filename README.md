
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metaboprep

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<!-- <div style="text-align: center"> -->
<!--   <img src="man/figures/logo.png" alt="metaboprep logo" width="150"/>  -->
<!-- </div> -->

The goal of `metaboprep` is to:

1.  Read in and processes (un)targeted metabolite data, saving datasets
    in tab-delimited format for use elsewhere
2.  Provide useful summary data in the form of tab-delimited text file
    and a html report.  
3.  Perform data filtering on the data set using a standard pipeline and
    according to user-defined thresholds.

## Installation

You can install the development version of metaboprep from
[GitHub](https://github.com/MRCIEU/metaboprep/tree/v2_development) with:

``` r
# install.packages("pak")
pak::pak("MRCIEU/metaboprep")
```

## Cheatsheet

<figure>
<img src="man/figures/cheatsheet.png" alt="Cheatsheet" />
<figcaption aria-hidden="true">Cheatsheet</figcaption>
</figure>

## Example

This is a basic example which shows you how to load data and run the
`metaboprep` quality control pipeline.

### Read data into R and create the Metaboprep object

``` r
library(metaboprep)

# import data 
m <- read_metabolon(system.file("extdata", "metabolon_v1.1_example.xlsx", package = "metaboprep"), 
                    sheet = "OrigScale",      ## The name of the sheet in the excel file to read in
                    return_Metaboprep = TRUE  ## Whether to return a Metaboprep object (TRUE) or a list (FALSE)
                    )
```

### Run the quality control pipeline

``` r
# run QC
m <- quality_control(m, 
                     source_layer = "input", 
                     sample_missingness  = 0.2, 
                     feature_missingness = 0.2, 
                     total_peak_area_sd  = 5, 
                     outlier_udist       = 5, 
                     outlier_treatment   = "leave_be", 
                     winsorize_quantile  = 1.0, 
                     tree_cut_height     = 0.5, 
                     pc_outlier_sd       = 5, 
                     sample_ids          = NULL, 
                     feature_ids         = NULL)

```
