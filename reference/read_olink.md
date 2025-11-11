# Read and Process Olink NPX Data File

This function reads and processes an Olink NPX file in long format. It
supports \`.csv\`, \`.xls\`, \`.xlsx\`, \`.txt\`, \`.zip\`, and
\`.parquet\` formats, using Olink's own OlinkAnalyze::read_NPX()
function, and returns a metaboprep object or a list of matrices and
metadata frames for further analysis.

## Usage

``` r
read_olink(filepath, return_Metaboprep = FALSE)
```

## Arguments

- filepath:

  A string specifying the path to the Olink NPX file.

- return_Metaboprep:

  logical, if TRUE (default) return a Metaboprep object, if FALSE return
  a list.

## Value

Metaboprep object or a named list with the following elements:

- data:

  A matrix of NPX values with \`SampleID\` as rows and \`OlinkID\` as
  columns, containing only sample data.

- samples:

  A \`data.frame\` containing metadata for samples.

- features:

  A \`data.frame\` containing feature-level metadata for samples.

- controls:

  A matrix of NPX values for control samples.

- control_metadata:

  A \`data.frame\` containing metadata for control samples.

## Details

The function checks whether the input data is in long format by
verifying the presence of duplicate \`SampleID\` values. It also
accommodates two variants of Olink files:

- Files that include a \`Sample_Type\` column with values \`"SAMPLE"\`
  and \`"CONTROL"\`.

- Files that use the \`SampleID\` column to label control samples (e.g.,
  entries containing \`"CONTROL"\`).

If neither format is detected, the function stops with an error
indicating that the data is likely not from Olink.

## Examples

``` r
if (FALSE) { # \dontrun{
  filepath <- system.file("extdata", "example_olink_data.txt", package = "metaboprep")
  olink_data <- read_olink(filepath)
} # }
```
